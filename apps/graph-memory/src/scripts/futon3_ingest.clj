(ns scripts.futon3-ingest
  "Ingest Futon3 devmaps + pattern library definitions into Futon1."
  (:require [charon.core :as charon]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [app.config :as cfg]
            [app.store-manager :as store-manager]
            [app.store :as store]
            [app.invariants :as invariants]
            [app.model :as model]
            [app.sigil-allowlist :as sigil-allowlist]
            [datascript.core :as d]
            [clojure.stacktrace :as stack]
            [scripts.rehydrate :as rehydrate])
  (:import (java.time Instant)))

;; --- CLI parsing ------------------------------------------------------------

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M:scripts/ingest-futon3 [--root PATH] [--profile NAME] [--pattern NAME]"
             "       clojure -M:scripts/ingest-futon3 [--no-verify]"
             "       clojure -M:scripts/ingest-futon3 [--rehydrate] [--rehydrate-url URL]"
             "Environment variables:"
             "  FUTON3_ROOT   Override the Futon3 checkout root (default ../futon3)"
             "  BASIC_CHAT_DATA_DIR Override the Futon1 data root"
             "  ALPHA_PROFILE Futon1 profile to ingest into (defaults to configured profile)"
             "  FUTON1_REHYDRATE_URL Optional URL to POST for live rehydrate"]))

(defn- parse-cli [args]
  (loop [opts {:root nil :profile nil :pattern nil :help? false :verify? true
               :rehydrate? nil :rehydrate-url nil}
         remaining args]
    (if-let [arg (first remaining)]
      (cond
        (#{"-h" "--help"} arg)
        (recur (assoc opts :help? true) (rest remaining))

        (#{"-r" "--root" "--futon3-root"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :root value) (nnext remaining))
          (throw (ex-info "Missing value for --root" {})))

        (#{"-p" "--profile"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :profile value) (nnext remaining))
          (throw (ex-info "Missing value for --profile" {})))

        (#{"--pattern"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :pattern value) (nnext remaining))
          (throw (ex-info "Missing value for --pattern" {})))

        (#{"--no-verify"} arg)
        (recur (assoc opts :verify? false) (rest remaining))

        (#{"--rehydrate"} arg)
        (recur (assoc opts :rehydrate? true) (rest remaining))

        (#{"--rehydrate-url"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :rehydrate-url value :rehydrate? true) (nnext remaining))
          (throw (ex-info "Missing value for --rehydrate-url" {})))

        :else
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- resolve-root [explicit]
  (let [env-root (some-> (System/getenv "FUTON3_ROOT") str/trim not-empty)
        fallback (-> (io/file ".." "futon3") .getAbsolutePath)
        path (or explicit env-root fallback)
        dir (io/file path)]
    (when-not (.exists dir)
      (throw (ex-info (str "Futon3 root not found at " (.getAbsolutePath dir))
                      {:path (.getAbsolutePath dir)})))
    (.getAbsolutePath dir)))

(defn resolve-root*
  "Public wrapper for resolving FUTON3_ROOT."
  [explicit]
  (resolve-root explicit))

;; --- Config resolution -----------------------------------------------------

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        (.getAbsolutePath dir)
        (recur (.getParentFile dir))))))

(defn- resolve-data-root [app-cfg]
  (let [env-root (some-> (System/getenv "BASIC_CHAT_DATA_DIR") str/trim not-empty)
        raw-root (or env-root (:app/data-dir app-cfg))]
    (when raw-root
      (let [file (io/file raw-root)]
        (if (.isAbsolute file)
          (.getAbsolutePath file)
          (if-let [base (repo-root)]
            (.getAbsolutePath (io/file base raw-root))
            (.getAbsolutePath file)))))))

;; --- Parsing helpers --------------------------------------------------------

(def ^:private clause-re
  (re-pattern "(?m)^!\\s+(?:conclusion|claim|instantiated-by):\\s*(.*?)(?:\\s*\\[(.*?)\\])?\\s*$"))

(defn- normalize-sigils-block [block]
  (some-> block
          str/trim
          (str/replace #"^\[" "")
          (str/replace #"\]$" "")
          str/trim
          not-empty))

(defn- split-sigils [block]
  (->> (str/split block #"\s+")
       (keep (fn [token]
               (when (str/includes? token "/")
                 (let [[emoji hanzi] (str/split token #"/" 2)]
                   {:emoji (some-> emoji str/trim not-empty)
                    :hanzi (some-> hanzi str/trim not-empty)}))))))

(defn- extract-meta [text key]
  (some->> (re-find (re-pattern (str "@" key "\\s+(.*)")) text)
           second
           str/trim
           not-empty))

(defn- starts-arg-line? [line]
  (or (str/starts-with? line "@arg ")
      (str/starts-with? line "@flexiarg ")))

(defn- split-arg-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [remaining lines
           current []
           has-arg? false
           blocks []]
      (if-let [line (first remaining)]
        (let [rest-lines (rest remaining)
              starts-arg? (starts-arg-line? line)]
          (cond
            (and starts-arg? has-arg?)
            (recur rest-lines
                   [line]
                   true
                   (conj blocks (str/join "\n" current)))

            starts-arg?
            (recur rest-lines
                   (conj current line)
                   true
                   blocks)

            :else
            (recur rest-lines
                   (conj current line)
                   has-arg?
                   blocks)))
        (if has-arg?
          (conj blocks (str/join "\n" current))
          [text])))))

(defn- pattern-files [root]
  (let [lib-dir (io/file root "library")
        hole-dir (io/file root "holes")]
    (filter (fn [^java.io.File f]
              (and (.isFile f)
                   (let [name (.getName f)]
                     (or (str/ends-with? name ".flexiarg")
                         (str/ends-with? name ".multiarg")))))
            (concat (file-seq lib-dir)
                    (file-seq (io/file hole-dir "LDTS"))))))

(defn- normalize-kind [label]
  (-> label
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-" "")
      (str/replace #"-$" "")))

(def component-line-re
  (re-pattern "^(\\s*)([!+])\\s+([A-Za-z0-9/+-]+):\\s*(.*)$"))

(defn- slurp-utf8 [file]
  (slurp file :encoding "UTF-8"))

(defn- finalize-component [entry]
  (let [lines (or (:lines entry) [])
        text (->> lines
                  (remove str/blank?)
                  (map str/trim)
                  (str/join "\n")
                  str/trim)]
    (-> entry
        (dissoc :lines)
        (assoc :text text))))

(defn- parse-component-lines [block]
  (let [lines (str/split-lines block)]
    (loop [remaining lines
           order 0
           level-map {}
           current nil
           acc []]
      (if-let [line (first remaining)]
        (if-let [[_ indent _raw-marker label text] (re-matches component-line-re line)]
          (let [level (int (/ (count indent) 2))
                trimmed-text (str/trim text)
                trimmed-levels (into {}
                                     (remove (fn [[lvl _]] (> lvl level)) level-map))
                parent (get trimmed-levels (dec level))
                acc (if current (conj acc (finalize-component current)) acc)
                entry {:order order
                       :label label
                       :kind (normalize-kind label)
                       :lines (cond-> []
                                (not (str/blank? trimmed-text))
                                (conj trimmed-text))
                       :level level
                       :parent-index parent}]
            (recur (rest remaining)
                   (inc order)
                   (assoc trimmed-levels level order)
                   entry
                   acc))
          (let [current (if current
                            (update current :lines conj line)
                          current)]
            (recur (rest remaining) order level-map current acc)))
        (cond-> acc
          current (conj (finalize-component current)))))))

(defn- parse-patterns [root]
  (for [file (pattern-files root)
        :let [text (slurp-utf8 file)]
        block (split-arg-blocks text)
        :let [title (extract-meta block "title")
              arg (or (extract-meta block "arg")
                      (extract-meta block "flexiarg"))
              sigils-meta (normalize-sigils-block (extract-meta block "sigils"))
              matches (re-seq clause-re block)
              components (vec (parse-component-lines block))]
        match matches
        :let [[_ summary sigils-inline] match
              sigils-block (or (normalize-sigils-block sigils-inline) sigils-meta)]
        :when summary]
    {:id (or arg (.getName file))
     :title (or title arg (.getName file))
     :summary (str/trim summary)
     :sigils (vec (split-sigils (or sigils-block "")))
     :components components}))

(defn parse-patterns*
  "Public wrapper for parsing FUTON3 pattern definitions."
  [root]
  (parse-patterns root))

(defn- devmap-files [root]
  (filter (fn [^java.io.File f]
            (and (.isFile f)
                 (str/ends-with? (.getName f) ".devmap")))
          (file-seq (io/file root "holes"))))

(defn- futon-number [name]
  (some->> (re-find #"futon(\d+)" name)
           second
           (format "f%s")))

(defn- parse-devmaps [root]
  (reduce (fn [acc file]
            (let [text (slurp-utf8 file)
                  futon (futon-number (.getName file))]
              (reduce (fn [acc' [_ proto-part sigils-block]]
                        (let [proto-id (if (re-matches #"\d+" proto-part)
                                         (format "%s/p%s" futon proto-part)
                                         (format "%s/%s" futon (-> proto-part
                                                                   str/lower-case
                                                                   (str/replace #"[^a-z0-9]+" "-")
                                                                   (str/replace #"^-" "")
                                                                   (str/replace #"-$" ""))))
                              sigils (vec (split-sigils sigils-block))]
                          (assoc acc' proto-id sigils)))
                      acc
                      (re-seq (re-pattern
                               "!\\s+instantiated-by:\\s*(?:Prototype\\s+)?(\\S+) — .*?\\[(.*?)\\]")
                              text))))
          {}
          (devmap-files root)))

;; --- Persistence ------------------------------------------------------------

(defn- now-ms [] (.toEpochMilli (Instant/now)))

(defn- sigil-name [{:keys [emoji hanzi]}]
  (str (or emoji "?")
       (when hanzi
         (str " / " hanzi))))

(defn- sigil-token [{:keys [emoji hanzi]}]
  (str (or emoji "?") "/" (or hanzi "?")))

(defn- ensure-sigil-allowlisted! [opts sigil]
  (when-let [allowlist (:sigil-allowlist opts)]
    (when-not (sigil-allowlist/sigil-allowed? allowlist sigil)
      (throw (ex-info "Sigil not allowlisted"
                      {:sigil sigil
                       :token (sigil-token sigil)
                       :root (:root allowlist)})))))

(defn- ensure-sigil! [conn opts sigil]
  (ensure-sigil-allowlisted! opts sigil)
  (let [name (sigil-name sigil)
        external (str (or (:emoji sigil) "?") "|" (or (:hanzi sigil) "?"))]
    (store/ensure-entity! conn opts {:name name
                                     :type :sigil
                                     :external-id external
                                     :source "futon3/sigil"})))

(defn- verify-entity-roundtrip!
  "Fetch ENTITY back via `store/fetch-entity` and log a short summary.
  When FUTON4_VERIFY_FETCH env var is set (any non-empty value), also
  hit the HTTP API to ensure remote fetches succeed."
  [conn {:keys [id name type]}]
  (when id
    (try
      (when-let [fetched (store/fetch-entity conn {:id id} {})]
        (println (format "      ↳ verified %s (%s) type=%s"
                         (or (:name fetched) name)
                         id
                         (or (:type fetched) type)))
        (when (System/getenv "FUTON4_VERIFY_FETCH")
          (try
            (let [cmd ["curl" "-s" "-H" "X-Profile: default"
                       (format "http://localhost:8080/api/alpha/entity/%s" id)]
                  proc (.exec (Runtime/getRuntime) (into-array String cmd))
                  status (.waitFor proc)]
              (when-not (zero? status)
                (println (format "      ↳ curl failed for %s (%s)" (or name "?") id))))
            (catch Exception ex
              (println (format "      ↳ curl exception for %s (%s): %s"
                               (or name "?") id (.getMessage ex)))))))
      (catch Exception ex
        (println (format "      ↳ fetch failed for %s (%s): %s"
                         (or name "?")
                         id
                         (.getMessage ex)))))))

(defn- component-preview [text]
  (-> text (or "") str/trim (str/replace #"\s+" " ") (subs 0 (min 60 (count text)))))

(defn- ensure-pattern-component! [conn opts pattern {:keys [kind label text order level]}]
  (let [base-name (:name pattern)
        component-name (format "%s/%02d-%s" base-name order (or kind "section"))
        label* (or kind (normalize-kind label))
        component (store/ensure-entity! conn opts {:name component-name
                                                   :type :pattern/component
                                                   :external-id (str (:id pattern) "::" kind "::" order)
                                                   :source text})]
    (println (format "    [%02d] %-20s %s" order (or kind "section") (component-preview (or text ""))))
    (store/upsert-relation! conn opts {:type :pattern/includes
                                       :src {:id (:id pattern)
                                             :name (:name pattern)
                                             :type :pattern/library}
                                       :dst {:id (:id component)
                                             :name (:name component)
                                             :type :pattern/component}
                                       :props {:pattern/component-kind kind
                                               :pattern/component-label label*
                                               :pattern/component-order order
                                               :pattern/component-level level}})
    component))

(defn- link-component-parent! [conn opts parent child]
  (store/upsert-relation! conn opts {:type :pattern/component-parent
                                     :src {:id (:id parent)
                                           :name (:name parent)
                                           :type :pattern/component}
                                     :dst {:id (:id child)
                                           :name (:name child)
                                           :type :pattern/component}}))

(defn- ensure-pattern! [conn opts {:keys [id title summary sigils components]}]
  (println (format "  pattern %-25s %s" (or id title)
                   (component-preview summary)))
  (let [entity (store/ensure-entity! conn opts {:name (or id title)
                                                :type :pattern/library
                                                :external-id id
                                                :source summary})]
    (verify-entity-roundtrip! conn entity)
    (let [rels (d/q '[:find ?rel-id
                      :in $ ?type ?src-id
                      :where
                      [?r :relation/type ?type]
                      [?r :relation/id ?rel-id]
                      [?r :relation/src ?src]
                      [?src :entity/id ?src-id]]
                    @conn :pattern/includes (:id entity))]
      (doseq [[rel-id] rels]
        (store/delete-relation! conn opts rel-id)))
    (doseq [sigil sigils
            :when (seq (remove nil? (vals sigil)))]
      (let [sig-entity (ensure-sigil! conn opts sigil)]
        (store/upsert-relation! conn opts {:type :pattern/has-sigil
                                           :src {:id (:id entity)
                                                 :name (:name entity)
                                                 :type :pattern/library}
                                           :dst {:id (:id sig-entity)
                                                 :name (:name sig-entity)
                                                 :type :sigil}})))
    (let [component-entities (mapv (fn [idx component]
                                     (ensure-pattern-component! conn opts entity
                                                               (cond-> component
                                                                 (nil? (:order component))
                                                                 (assoc :order idx))))
                                   (range)
                                   (or (seq components) []))]
      (doseq [component component-entities]
        (verify-entity-roundtrip! conn component))
      (doseq [component components
              :let [parent-idx (:parent-index component)
                    order (:order component)]
              :when (and (some? parent-idx)
                         (< parent-idx (count component-entities)))]
        (let [parent (nth component-entities parent-idx nil)
              child (nth component-entities order nil)]
          (when (and parent child)
            (link-component-parent! conn opts parent child)))))
    entity))

(defn- ensure-prototype! [conn opts {:keys [id sigils]}]
  (println (format "  ingesting prototype %s" id))
  (let [entity (store/ensure-entity! conn opts {:name id
                                                :type :devmap/prototype
                                                :external-id id
                                                :source "futon3/devmap"})]
    (doseq [sigil sigils
            :when (seq (remove nil? (vals sigil)))]
      (let [sig-entity (ensure-sigil! conn opts sigil)]
        (store/upsert-relation! conn opts {:type :prototype/has-sigil
                                           :src {:id (:id entity)
                                                 :name (:name entity)
                                                 :type :devmap/prototype}
                                           :dst {:id (:id sig-entity)
                                                 :name (:name sig-entity)
                                           :type :sigil}})))
    entity))

(defn- summarize-pattern-failures [entries]
  (->> entries
       (map (fn [{:keys [key failures]}]
              {:key key
               :count (count failures)
               :sample (vec (take 5 failures))}))
       vec))

(defn- print-pattern-failures! [pattern-id entries]
  (when-let [entry (first entries)]
    (when-let [failure (first (:failures entry))]
      (let [pid (or (:pattern-id failure) pattern-id)
            pname (:pattern-name failure)]
        (println (format "Pattern invariant failure: %s pattern=%s%s details=%s"
                         (:key entry)
                         (or pid "?")
                         (if pname (format " (%s)" pname) "")
                         (pr-str (dissoc failure :pattern-name :pattern-id))))))))

(defn- filter-pattern-failures [model-result pattern-id]
  (->> (:results model-result)
       (map (fn [entry]
              (let [failures (filter (fn [failure]
                                       (let [name (:pattern-name failure)
                                             pid (:pattern-id failure)]
                                         (or (= name pattern-id)
                                             (= pid pattern-id))))
                                     (:failures entry))]
                (assoc entry :failures (vec failures)))))
       (filter (comp seq :failures))
       vec))

(defn- verify-patterns-or-throw! [conn env {:keys [pattern-id]}]
  (invariants/ensure-descriptors! conn env [:patterns])
  (let [result (model/verify conn)]
    (when-let [pattern-failures (seq (filter-pattern-failures result pattern-id))]
      (print-pattern-failures! pattern-id pattern-failures)
      (throw (ex-info (format "Pattern invariants failed after ingesting %s"
                              (or pattern-id "?"))
                      {:pattern-id pattern-id
                       :failures (summarize-pattern-failures pattern-failures)})))))

(defn- ingest-patterns! [conn opts patterns]
  (println (format "Persisting %d Futon3 patterns…" (count patterns)))
  (let [opts* (if (:verify? opts)
                (dissoc opts :verify-fn)
                opts)]
    (doseq [entry patterns]
      (ensure-pattern! conn opts* entry)
      (when (:verify? opts*)
        (verify-patterns-or-throw! conn opts* {:pattern-id (:id entry)}))))
  (println "Patterns ingested."))

(defn ingest-patterns*
  "Public wrapper for ingesting FUTON3 pattern definitions."
  [conn opts patterns]
  (try
    (ingest-patterns! conn opts patterns)
    (charon/ok :patterns/ingest {:count (count patterns)})
    (catch Exception ex
      (charon/reject :patterns/ingest
                     :patterns/ingest-failed
                     {:error (.getMessage ex)
                      :data (ex-data ex)}
                     "Pattern ingest failed; fix invariants and retry."))))

(defn- ingest-devmaps! [conn opts devmaps]
  (println (format "Persisting %d devmap prototypes…" (count devmaps)))
  (doseq [[proto sigils] devmaps]
    (ensure-prototype! conn opts {:id proto :sigils sigils}))
  (println "Devmaps ingested."))

(defn- pattern-match? [entry needle]
  (when (and entry needle)
    (let [candidate (or (:id entry) (:title entry))]
      (and candidate
           (= candidate needle)))))

(defn -main [& args]
  (try
    (let [{:keys [root profile pattern help? verify? rehydrate? rehydrate-url]} (parse-cli args)]
      (when help?
        (println (usage))
        (System/exit 0))
      (let [resolved-root (resolve-root root)
            sigil-allowlist (sigil-allowlist/allowlist-from-root resolved-root)
            ;; Respect FUTON_CONFIG / config.edn for data-dir
            app-cfg (cfg/config)
            data-root (resolve-data-root app-cfg)
            sm-cfg (store-manager/configure!
                    (cond-> {}
                      data-root (assoc :data-root data-root)))
            profile-id (or profile (:default-profile sm-cfg))
            conn (store-manager/conn profile-id)
            env (assoc (store-manager/env profile-id)
                       :now (now-ms)
                       :verify? verify?
                       :sigil-allowlist sigil-allowlist
                       :penholder "charon")
            patterns (vec (parse-patterns resolved-root))
            patterns (if pattern
                       (vec (filter #(pattern-match? % pattern) patterns))
                       patterns)
            devmaps (when-not pattern (parse-devmaps resolved-root))]
        (when data-root
          (println (format "Using data root: %s" data-root)))
        (when (and pattern (empty? patterns))
          (throw (ex-info (str "No patterns matched " pattern) {:pattern pattern})))
        (let [result (ingest-patterns* conn env patterns)]
          (when (false? (:ok? result))
            (binding [*out* *err*]
              (println "Pattern ingest rejected.")
              (println (pr-str result)))
            (System/exit 1)))
        (when devmaps
          (ingest-devmaps! conn env devmaps))
        (rehydrate/maybe-rehydrate! {:rehydrate? rehydrate?
                                     :rehydrate-url rehydrate-url})
        (store-manager/shutdown!)))
    (catch Exception ex
      (binding [*out* *err*]
        (println "Ingest failed:" (.getMessage ex))
        (stack/print-stack-trace ex))
      (System/exit 1))))
