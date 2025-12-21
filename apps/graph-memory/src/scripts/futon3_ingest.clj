(ns scripts.futon3-ingest
  "Ingest Futon3 devmaps + pattern library definitions into Futon1."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [app.store-manager :as store-manager]
            [app.store :as store]
            [clojure.stacktrace :as stack])
  (:import (java.time Instant)))

;; --- CLI parsing ------------------------------------------------------------

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M:scripts/ingest-futon3 [--root PATH] [--profile NAME]"
             "Environment variables:"
             "  FUTON3_ROOT   Override the Futon3 checkout root (default ../futon3)"
             "  ALPHA_PROFILE Futon1 profile to ingest into (defaults to configured profile)"]))

(defn- parse-cli [args]
  (loop [opts {:root nil :profile nil :help? false}
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

;; --- Parsing helpers --------------------------------------------------------

(def ^:private clause-re
  (re-pattern "!\\s+(?:conclusion|claim|instantiated-by):\\s*(.*?)\\s*\\[(.*?)\\]"))

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

(defn- split-arg-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [remaining lines
           current []
           has-arg? false
           blocks []]
      (if-let [line (first remaining)]
        (let [rest-lines (rest remaining)
              starts-arg? (str/starts-with? line "@arg ")]
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

(defn- parse-component-lines [block]
  (let [lines (str/split-lines block)]
    (loop [remaining lines
           order 0
           level-map {}
           acc []]
      (if-let [line (first remaining)]
        (if-let [[_ indent _raw-marker label text] (re-matches component-line-re line)]
          (let [level (int (/ (count indent) 2))
                trimmed-text (str/trim text)
                trimmed-levels (into {}
                                     (remove (fn [[lvl _]] (> lvl level)) level-map))
                parent (get trimmed-levels (dec level))
                entry {:order order
                       :label label
                       :kind (normalize-kind label)
                       :text trimmed-text
                       :level level
                       :parent-index parent}]
            (recur (rest remaining)
                   (inc order)
                   (assoc trimmed-levels level order)
                   (conj acc entry)))
          (recur (rest remaining) order level-map acc))
        acc))))

(defn- parse-patterns [root]
  (for [file (pattern-files root)
        :let [text (slurp file)]
        block (split-arg-blocks text)
        :let [title (extract-meta block "title")
              arg (extract-meta block "arg")
              matches (re-seq clause-re block)
              components (vec (parse-component-lines block))]
        match matches
        :let [[_ summary sigils-block] match]
        :when summary]
    {:id (or arg (.getName file))
     :title (or title arg (.getName file))
     :summary (str/trim summary)
     :sigils (vec (split-sigils sigils-block))
     :components components}))

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
            (let [text (slurp file)
                  futon (futon-number (.getName file))]
              (reduce (fn [acc' [_ idx sigils-block]]
                        (let [proto (format "%s/p%s" futon idx)
                              sigils (vec (split-sigils sigils-block))]
                          (assoc acc' proto sigils)))
                      acc
                      (re-seq (re-pattern
                               "!\\s+instantiated-by: Prototype\\s+(\\d+) — .*?\\[(.*?)\\]")
                              text))))
          {}
          (devmap-files root)))

;; --- Persistence ------------------------------------------------------------

(defn- now-ms [] (.toEpochMilli (Instant/now)))

(defn- sigil-name [{:keys [emoji hanzi]}]
  (str (or emoji "?")
       (when hanzi
         (str " / " hanzi))))

(defn- ensure-sigil! [conn opts sigil]
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
                                               :pattern/component-label label
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
                                                :external-id (or title id)
                                                :source summary})]
    (verify-entity-roundtrip! conn entity)
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
    (let [component-entities (mapv (fn [component]
                                     (ensure-pattern-component! conn opts entity component))
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

(defn- ingest-patterns! [conn opts patterns]
  (println (format "Persisting %d Futon3 patterns…" (count patterns)))
  (doseq [entry patterns]
    (ensure-pattern! conn opts entry))
  (println "Patterns ingested."))

(defn- ingest-devmaps! [conn opts devmaps]
  (println (format "Persisting %d devmap prototypes…" (count devmaps)))
  (doseq [[proto sigils] devmaps]
    (ensure-prototype! conn opts {:id proto :sigils sigils}))
  (println "Devmaps ingested."))

(defn -main [& args]
  (try
    (let [{:keys [root profile help?]} (parse-cli args)]
      (when help?
        (println (usage))
        (System/exit 0))
      (let [resolved-root (resolve-root root)
            cfg (store-manager/configure!)
            profile-id (or profile (:default-profile cfg))
            conn (store-manager/conn profile-id)
            env (assoc (store-manager/env profile-id) :now (now-ms))
            patterns (vec (parse-patterns resolved-root))
            devmaps (parse-devmaps resolved-root)]
        (ingest-patterns! conn env patterns)
        (ingest-devmaps! conn env devmaps)
        (store-manager/shutdown!)))
    (catch Exception ex
      (binding [*out* *err*]
        (println "Ingest failed:" (.getMessage ex))
        (stack/print-stack-trace ex))
      (System/exit 1))))
