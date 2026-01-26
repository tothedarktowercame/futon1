(ns app.docbook
  "Docbook ingest/query utilities for XTDB."
  (:require [app.config :as config]
            [app.xt :as xt]
            [charon.core :as charon]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- ensure-xt-node! []
  (when-not (xt/started?)
    (let [cfg (config/config)
          cfg-path (:xtdb/config-path cfg "apps/graph-memory/resources/xtdb.edn")
          data-dir (:app/data-dir cfg)]
      (xt/start! cfg-path {:data-dir (when data-dir (str (io/file data-dir "xtdb")))
                           :xt/created-by "app.docbook"}))))

(defn- normalize-status [value]
  (cond
    (keyword? value) value
    (string? value) (keyword value)
    :else :active))

(defn- normalize-outline [value]
  (cond
    (vector? value) value
    (sequential? value) (vec value)
    (string? value) (->> (str/split value #"\s*/\s*")
                         (remove str/blank?)
                         vec)
    :else nil))

(defn- blank->nil [value]
  (when-not (and (string? value) (str/blank? value))
    value))

(defn- missing? [value]
  (cond
    (nil? value) true
    (string? value) (str/blank? value)
    (sequential? value) (empty? value)
    :else false))

(defn- missing-fields [m fields]
  (->> fields (filter #(missing? (get m %))) vec))

(def ^:private heading-required
  [:doc/id :doc/book :doc/title :doc/outline_path :doc/path_string :doc/level])

(def ^:private entry-required
  [:doc/id :doc/entry-id :doc/book])

(defn- heading-doc [m]
  (let [doc-id (or (:doc_id m) (:doc/id m))
        outline (normalize-outline (or (:doc/outline_path m) (:outline_path m)))
        path-str (or (:doc/path_string m) (:path_string m))
        level (or (:doc/level m) (:level m) (some-> outline count))
        supersedes (or (:doc/supersedes m) (:supersedes m))
        title (or (:doc/title m) (:title m))]
    (cond-> {:xt/id doc-id
             :doc/id doc-id}
      (or (:doc/book m) (:book m)) (assoc :doc/book (or (:doc/book m) (:book m)))
      title (assoc :doc/title title)
      outline (assoc :doc/outline_path outline)
      path-str (assoc :doc/path_string path-str)
      level (assoc :doc/level level)
      supersedes (assoc :doc/supersedes supersedes))))

(defn- sha1 [^String value]
  (let [digest (java.security.MessageDigest/getInstance "SHA-1")]
    (.update digest (.getBytes value "UTF-8"))
    (format "%040x" (BigInteger. 1 (.digest digest)))))

(defn- short-hash [value]
  (subs (sha1 value) 0 12))

(defn derive-doc-id
  "Derive a doc-id from book + outline path using the toc hash rule."
  [book outline-path]
  (when (seq outline-path)
    (let [outline-key (str/join "/" outline-path)]
      (str book "-" (short-hash (format "%s::%s" book outline-key))))))

(defn- spine-includes [root spine-file]
  (when (and spine-file (.exists (io/file spine-file)))
    (let [lines (str/split-lines (slurp spine-file))
          re #"(?i)^\s*#\+INCLUDE:\s+\"?([^\"]+)\"?"]
      (->> lines
           (keep (fn [line]
                   (when-let [m (re-find re line)]
                     (second m))))
           (map (fn [path]
                  (.getCanonicalPath (io/file root path))))
           (filter #(str/ends-with? % ".org"))
           (filter #(-> (io/file %) .exists))))))
(defn- org-doc-files [root]
  (let [root (io/file root)
        spine2 (io/file root "spine2.org")
        files (concat (when (.exists spine2)
                        [(.getCanonicalPath spine2)])
                      (spine-includes root spine2))]
    (->> files distinct vec)))

(defn- org-heading-lines [lines]
  (keep-indexed
   (fn [idx line]
     (when-let [m (re-find #"^\s*(\*+)\s+(.*)$" line)]
       {:idx idx
        :level (count (nth m 1))
        :title (str/trim (nth m 2))}))
   lines))
(defn- org-section-bounds [headings total]
  (map-indexed
   (fn [idx heading]
     (let [level (:level heading)
           next-idx (->> (drop (inc idx) headings)
                         (filter #(<= (:level %) level))
                         first
                         :idx)
           end (or next-idx total)]
       (assoc heading :end end)))
   headings))

(defn- org-sections [file]
  (let [text (slurp file)
        lines (vec (str/split-lines text))
        headings (org-heading-lines lines)
        bounds (org-section-bounds headings (count lines))]
    (loop [items bounds
           stack []
           out []]
      (if-let [item (first items)]
        (let [level (:level item)
              title (:title item)
              stack (->> stack
                         (take-while #(< (:level %) level))
                         vec)
              stack (conj stack {:level level :title title})
              outline (mapv :title stack)
              body-lines (subvec lines (inc (:idx item)) (:end item))
              body (str/trim (str/join "\n" body-lines))]
          (recur (rest items)
                 stack
                 (conj out (assoc item :outline outline :body body))))
        out))))

(defn- function-name-from-title [title]
  (when-let [m (re-find #"`([^`]+)`" (or title ""))]
    (second m)))

(declare heading-error entry-error)

(defn- function-index [root]
  (let [root (io/file root "dev")]
    (if-not (.exists root)
      {}
      (let [root-path (.toPath root)
            files (->> (file-seq root)
                       (filter #(.isFile ^java.io.File %))
                       (filter #(str/ends-with? (.getName ^java.io.File %) ".el")))]
        (reduce
         (fn [acc file]
           (let [content (slurp file)
                 matches (re-seq #"\((?:cl-)?def(?:un|macro|subst|alias|generic|method)\s+([^\s\)]+)" content)]
             (reduce (fn [acc [_ name]]
                       (if (contains? acc name)
                         acc
                         (let [rel (.toString (.relativize root-path (.toPath file)))]
                           (assoc acc name (str "dev/" rel)))))
                     acc
                     matches)))
         {}
         files)))))

(defn- org-entry-doc [book doc-id entry-id title body opts]
  (let [status (normalize-status (:status opts))]
    (cond-> {:xt/id entry-id
             :doc/id doc-id
             :doc/entry-id entry-id
             :doc/book book
             :doc/version "org"
             :doc/status status
             :doc/body body}
      title (assoc :doc/title title)
      (:doc/function-name opts) (assoc :doc/function-name (:doc/function-name opts))
      (:doc/function-anchor opts) (assoc :doc/function-anchor (:doc/function-anchor opts))
      (:doc/source-path opts) (assoc :doc/source-path (:doc/source-path opts))
      (:doc/source-doc opts) (assoc :doc/source-doc (:doc/source-doc opts)))))

(defn- org-doc-txs [root book]
  (let [root-path (.getCanonicalPath (io/file root))
        files (org-doc-files root)
        fn-index (function-index root)]
    (reduce
     (fn [{:keys [txs stats errors]} file]
       (let [rel-path (str/replace (.getCanonicalPath (io/file file))
                                   (str root-path "/")
                                   "")
             sections (org-sections file)]
        (reduce
          (fn [{:keys [txs stats errors]} {:keys [outline level title body]}]
            (let [path-str (str/join " / " outline)
                  outline-key (str/join "/" outline)
                  doc-id (str book "-" (short-hash (format "%s::%s" book outline-key)))
                  entry-id (str doc-id "::org")
                  function-name (function-name-from-title title)
                  source-path (when function-name (get fn-index function-name))
                  anchor (when function-name (short-hash (str function-name "|" (or source-path ""))))
                  heading (heading-doc {:doc/id doc-id
                                        :doc/book book
                                        :doc/title title
                                        :doc/outline_path outline
                                        :doc/path_string path-str
                                        :doc/level level})
                  entry (org-entry-doc book doc-id entry-id title body
                                       {:doc/function-name function-name
                                        :doc/function-anchor anchor
                                        :doc/source-path source-path
                                        :doc/source-doc rel-path})
                  heading-err (heading-error heading)
                  entry-err (entry-error entry)
                  stats (-> stats
                            (update :total (fnil inc 0))
                            (update-in [:levels level] (fnil inc 0))
                            (cond-> function-name
                              (update :functions (fnil inc 0))))]
              (if (or heading-err entry-err)
                {:txs txs
                 :stats (update stats :skipped (fnil inc 0))
                 :errors (conj (or errors [])
                               {:error "Invalid org docbook entry"
                                :doc-id doc-id
                                :entry-id entry-id
                                :heading-error heading-err
                                :entry-error entry-err
                                :source-doc rel-path})}
                {:txs (into txs [[::xtdb/put heading]
                                 [::xtdb/put entry]])
                 :stats stats
                 :errors errors})))
          {:txs txs :stats stats :errors errors}
          sections)))
     {:txs [] :stats {:total 0 :levels {} :functions 0 :files (count files)} :errors []}
     files)))

(defn- entry-doc [book m]
  (let [doc-id (or (:doc_id m) (:doc/id m))
        entry-id (or (:doc/entry-id m) (:doc_entry_id m) (:doc/entry_id m) (:entry_id m) (:run_id m))
        body (blank->nil (or (:doc/body m)
                             (:doc/context m)
                             (:body m)
                             (:context m)
                             (:agent_summary m)
                             (:summary m)))]
    (when (and doc-id entry-id)
      (let [status (normalize-status (or (:doc/status m) (:status m)))]
        (cond-> {:xt/id entry-id
                 :doc/id doc-id
                 :doc/entry-id entry-id
                 :doc/book book
                 :doc/version (or (:version m) (:doc/version m))
                 :doc/timestamp (or (:timestamp m) (:doc/timestamp m))
                 :doc/status status}
          body (assoc :doc/body body)
          (:replaces m) (assoc :doc/replaces (:replaces m))
          (:doc/replaces m) (assoc :doc/replaces (:doc/replaces m))
          (:merges m) (assoc :doc/merges (:merges m))
          (:doc/merges m) (assoc :doc/merges (:doc/merges m))
          (:links_to m) (assoc :doc/links-to (:links_to m))
          (:doc/links-to m) (assoc :doc/links-to (:doc/links-to m))
          (:files_touched m) (assoc :doc/files (:files_touched m))
          (:doc/files m) (assoc :doc/files (:doc/files m))
          (:doc/commit m) (assoc :doc/commit (:doc/commit m))
          (:commits m) (assoc :doc/commit (first (:commits m)))
          (:scope m) (assoc :doc/scope (:scope m))
          (:doc/scope m) (assoc :doc/scope (:doc/scope m))
          (:tracker_refs m) (assoc :doc/tracker-refs (:tracker_refs m))
          (:doc/tracker-refs m) (assoc :doc/tracker-refs (:doc/tracker-refs m))
          (:doc/delta m) (assoc :doc/delta (:doc/delta m))
          (:doc/verification m) (assoc :doc/verification (:doc/verification m))
          (:doc/toc-version m) (assoc :doc/toc-version (:doc/toc-version m))
          (:notes m) (assoc :doc/notes (:notes m)))))))

(defn- normalize-entry-payload
  [book payload]
  (let [outline-raw (or (:doc/outline_path payload) (:outline_path payload))
        path-str-raw (or (:doc/path_string payload) (:path_string payload))
        outline (or (normalize-outline outline-raw)
                    (normalize-outline path-str-raw))
        path-str (or path-str-raw
                     (when (seq outline) (str/join " / " outline)))
        title (or (:doc/title payload) (:title payload)
                  (when (seq outline) (last outline)))
        level (or (:doc/level payload) (:level payload) (some-> outline count))
        doc-id (or (:doc/id payload) (:doc_id payload)
                   (derive-doc-id book outline))]
    (cond-> (assoc payload :doc/book book)
      doc-id (assoc :doc/id doc-id)
      outline (assoc :doc/outline_path outline)
      path-str (assoc :doc/path_string path-str)
      title (assoc :doc/title title)
      level (assoc :doc/level level))))

(defn- heading-error [heading]
  (when-let [missing (seq (missing-fields heading heading-required))]
    {:issue :missing-fields
     :missing (vec missing)}))

(defn- entry-error [entry]
  (let [missing (seq (missing-fields entry entry-required))
        status (:doc/status entry)
        body (:doc/body entry)]
    (cond
      missing {:issue :missing-fields
               :missing (vec missing)}
      (and (not= :removed status) (missing? body))
      {:issue :missing-body}
      :else nil)))

(defn upsert-entry!
  "Upsert a single docbook heading + entry (idempotent by entry-id)."
  [book payload]
  (ensure-xt-node!)
  (let [book (or book "futon4")
        payload (normalize-entry-payload book payload)
        heading (heading-doc payload)
        entry (entry-doc book payload)
        heading-err (when heading (heading-error heading))
        entry-err (when entry (entry-error entry))]
    (cond
      (not (and heading entry))
      {:ok? false
       :error "doc_id + entry_id required (or outline_path to derive doc_id)"
       :book book}

      heading-err
      {:ok? false
       :error "Invalid docbook heading payload"
       :book book
       :doc-id (:doc/id heading)
       :details heading-err}

      entry-err
      {:ok? false
       :error "Invalid docbook entry payload"
       :book book
       :doc-id (:doc/id entry)
       :entry-id (:doc/entry-id entry)
       :details entry-err}

      :else
      (do
        (xt/submit! [[::xtdb/put heading]
                     [::xtdb/put entry]])
        {:ok? true
         :book book
         :doc-id (:doc/id heading)
         :entry-id (:doc/entry-id entry)
         :heading heading
         :entry entry}))))

(defn upsert-entries!
  "Upsert a batch of docbook entries (idempotent by entry-id)."
  [book entries]
  (ensure-xt-node!)
  (let [book (or book "futon4")
        prepared (map (fn [payload]
                        (let [payload (normalize-entry-payload book payload)
                              heading (heading-doc payload)
                              entry (entry-doc book payload)
                              heading-err (when heading (heading-error heading))
                              entry-err (when entry (entry-error entry))]
                          {:payload payload
                           :heading heading
                           :entry entry
                           :heading-err heading-err
                           :entry-err entry-err
                           :ok? (and heading entry (not heading-err) (not entry-err))}))
                      entries)
        errors (->> prepared
                    (remove :ok?)
                    (map (fn [{:keys [payload heading entry heading-err entry-err]}]
                           {:error "Invalid docbook payload"
                            :doc-id (or (:doc/id heading) (:doc/id entry))
                            :entry-id (:doc/entry-id entry)
                            :heading-error heading-err
                            :entry-error entry-err
                            :payload payload}))
                    vec)
        txs (->> prepared
                 (filter :ok?)
                 (mapcat (fn [{:keys [heading entry]}]
                           [[::xtdb/put heading]
                            [::xtdb/put entry]]))
                 vec)]
    (when (seq txs)
      (xt/submit! txs))
    {:ok? (empty? errors)
     :book book
     :count (count entries)
     :accepted (- (count entries) (count errors))
     :errors errors}))

(defn- read-json [path]
  (with-open [r (io/reader path)]
    (json/read r :key-fn keyword)))

(defn- docbook-log-base [root book]
  (let [root (or root ".")
        data-base (io/file root "data" "logs" "books" book)
        dev-base (io/file root "dev" "logs" "books" book)]
    (cond
      (.exists data-base) data-base
      (.exists dev-base) dev-base
      :else data-base)))

(defn- read-stub-meta [path]
  (when (and path (.exists (io/file path)))
    (let [text (slurp path)
          lines (vec (str/split-lines text))
          title (some (fn [line]
                        (when (str/starts-with? line "#+TITLE:")
                          (str/trim (subs line (count "#+TITLE:")))))
                      lines)
          props (loop [remaining lines
                       in-props? false
                       acc {}]
                  (if-let [line (first remaining)]
                    (cond
                      (= line ":PROPERTIES:")
                      (recur (rest remaining) true acc)

                      (= line ":END:")
                      (recur (rest remaining) false acc)

                      in-props?
                      (if-let [[_ key value] (re-find #"^:([A-Z0-9_]+):\s*(.*)$" line)]
                        (recur (rest remaining) true (assoc acc key (str/trim value)))
                        (recur (rest remaining) true acc))

                      :else
                      (recur (rest remaining) false acc))
                    acc))
          outline (some-> (get props "OUTLINE_PATH") normalize-outline)
          path-str (get props "PATH_STRING")
          body-start (or (first (keep-indexed (fn [idx line]
                                                (when (str/starts-with? line "*")
                                                  idx))
                                              lines))
                         0)
          body (->> (subvec lines body-start)
                    (str/join "\n")
                    str/trim)]
      (cond-> {:doc/body body}
        title (assoc :doc/title title)
        outline (assoc :doc/outline_path outline)
        path-str (assoc :doc/path_string path-str)))))

(defn- merge-stub-into-payload [payload stub]
  (if stub
    (-> payload
        (cond-> (and (not (:doc/body payload))
                     (not (:doc/context payload))
                     (not (:context payload))
                     (not (:body payload))
                     (not (:agent_summary payload))
                     (not (:summary payload)))
          (assoc :doc/body (:doc/body stub)))
        (cond-> (and (not (:doc/title payload)) (:doc/title stub))
          (assoc :doc/title (:doc/title stub)))
        (cond-> (and (not (:doc/outline_path payload)) (not (:outline_path payload)) (:doc/outline_path stub))
          (assoc :doc/outline_path (:doc/outline_path stub)))
        (cond-> (and (not (:doc/path_string payload)) (not (:path_string payload)) (:doc/path_string stub))
          (assoc :doc/path_string (:doc/path_string stub))))
    payload))

(defn- entry-files [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".json"))))

(defn ingest!
  "Ingest docbook files from ROOT/dev/logs/books/<book>/ into XTDB."
  [{:keys [root book]}]
  (ensure-xt-node!)
  (let [book (or book "futon4")
        root (or root ".")
        base (docbook-log-base root book)
        raw-dir (io/file base "raw")
        toc (io/file base "toc.json")
        txs-acc (transient [])
        errors-acc (transient [])]
    (when (.exists toc)
      (let [payload (read-json toc)]
        (when (sequential? payload)
          (doseq [h payload]
            (let [heading (heading-doc h)
                  heading-err (when heading (heading-error heading))]
              (if heading-err
                (conj! errors-acc
                       {:error "Invalid docbook toc heading"
                        :doc-id (:doc/id heading)
                        :heading-error heading-err})
                (conj! txs-acc [::xtdb/put heading])))))))
    (doseq [f (entry-files raw-dir)]
      (let* [payload (normalize-entry-payload book (read-json f))
             doc-id (:doc/id payload)
             stub (when doc-id
                    (read-stub-meta (io/file root "docs" "docbook" book
                                             (format "%s.org" doc-id))))
             payload (merge-stub-into-payload payload stub)
             heading (heading-doc payload)
             entry (entry-doc book payload)
             heading-err (when heading (heading-error heading))
             entry-err (when entry (entry-error entry))]
        (if (or heading-err entry-err)
          (conj! errors-acc
                 {:error "Invalid docbook entry"
                  :doc-id (:doc/id heading)
                  :entry-id (:doc/entry-id entry)
                  :source (.getName f)
                  :heading-error heading-err
                  :entry-error entry-err})
          (do
            (when heading
              (conj! txs-acc [::xtdb/put heading]))
            (when entry
              (conj! txs-acc [::xtdb/put entry]))))))
    (let [{:keys [txs stats errors]} (org-doc-txs root book)]
      (doseq [tx txs]
        (conj! txs-acc tx))
      (doseq [err errors]
        (conj! errors-acc err))
      (let [levels (:levels stats)
            level-str (if (seq levels)
                        (->> levels
                             (sort-by key)
                             (map (fn [[lvl cnt]] (format "L%d=%d" lvl cnt)))
                             (str/join " "))
                        "none")]
        (println (format "[docbook-ingest] org-headings total=%d %s functions=%d files=%d"
                         (or (:total stats) 0)
                         level-str
                         (or (:functions stats) 0)
                         (or (:files stats) 0)))))
    (let [txs* (persistent! txs-acc)]
      (when (seq txs*)
        (xt/submit! txs*)))
    (let [errors (persistent! errors-acc)
          result {:book book
                  :headings (when (.exists toc) (count (read-json toc)))
                  :entries (count (entry-files raw-dir))
                  :errors errors}]
      (if (empty? errors)
        (charon/ok :docbook/ingest result)
        (charon/reject :docbook/ingest
                       :docbook/invalid-entry
                       result
                       "Docbook ingest rejected invalid entries.")))))

(defn- latest-entry [entries]
  (->> entries
       (remove #(= :removed (:doc/status %)))
       (sort-by (juxt #(or (:doc/timestamp %) "") #(or (:doc/version %) ""))
                #(compare %2 %1))
       first))

(defn- normalize-entry [entry]
  (cond-> entry
    (and (:doc/context entry) (not (:doc/body entry)))
    (-> (assoc :doc/body (:doc/context entry))
        (dissoc :doc/context))
    (:doc/context entry) (dissoc :doc/context)))

(defn- entries-for-doc [db doc-id]
  (->> (xt/q db '{:find [(pull ?e [*])]
                  :in [?doc-id]
                  :where [[?e :doc/id ?doc-id]]}
             doc-id)
       (map first)))

(defn heading->with-latest [db heading]
  (let [entries (->> (entries-for-doc db (:doc/id heading))
                     (map normalize-entry))
        latest (latest-entry entries)]
    (assoc heading
           :doc/entries (sort-by #(or (:doc/timestamp %) "") #(compare %2 %1) entries)
           :doc/latest latest)))

(defn- toc-order-id [book]
  (str "docbook-toc-order::" book))

(defn- fetch-toc-order [db book]
  (some-> (xtdb/entity db (toc-order-id book))
          :doc/toc-order))

(defn- heading-sort-key [heading]
  [(or (:doc/path_string heading) "")
   (or (:doc/title heading) "")
   (or (:doc/id heading) "")])

(defn- active-entry? [entry]
  (not= :removed (:doc/status entry)))

(defn- default-heading-order [headings]
  (->> headings
       (sort-by heading-sort-key)
       (map :doc/id)
       vec))

(defn- merge-order [preferred fallback]
  (let [preferred* (vec (distinct preferred))
        seen (set preferred*)]
    (vec (concat preferred* (remove seen fallback)))))

(defn- ordered-headings [db book]
  (let [headings (->> (xt/q db '{:find [(pull ?h [*])]
                                 :in [?book]
                                 :where [[?h :doc/book ?book]
                                         (not [?h :doc/toc-order _])]}
                            book)
                      (map first)
                      (remove :doc/entry-id))
        default-order (default-heading-order headings)
        stored-order (fetch-toc-order db book)
        ordered-ids (if (seq stored-order)
                      (merge-order (filter (set default-order) stored-order)
                                   default-order)
                      default-order)
        headings-by-id (group-by :doc/id headings)]
    (->> ordered-ids
         (mapcat #(get headings-by-id %)))))

(defn contents [book]
  (ensure-xt-node!)
  (let [db (xt/db)
        headings (->> (ordered-headings db book)
                      (map #(heading->with-latest db %)))]
    {:book book
     :headings headings}))

(def ^:private toc-heading-keys
  [:doc/id :doc/path_string :doc/outline_path :doc/level :doc/title])

(def ^:private toc-latest-keys
  [:doc/entry-id :doc/timestamp :doc/version :doc/status])

(defn toc [book]
  (ensure-xt-node!)
  (let [db (xt/db)
        headings (ordered-headings db book)]
    {:book book
     :headings (mapv (fn [heading]
                       (let [entries (entries-for-doc db (:doc/id heading))
                             active (filter active-entry? entries)
                             latest (latest-entry entries)]
                         (cond-> (select-keys heading toc-heading-keys)
                           (seq active) (assoc :doc/entry-count (count active))
                           latest (assoc :doc/latest (select-keys latest toc-latest-keys)))))
                     headings)}))

(defn update-toc-order!
  [book {:keys [order source timestamp]}]
  (ensure-xt-node!)
  (let [db (xt/db)
        headings (->> (xt/q db '{:find [(pull ?h [*])]
                                 :in [?book]
                                 :where [[?h :doc/book ?book]]}
                            book)
                      (map first)
                      (remove :doc/entry-id))
        default-order (default-heading-order headings)
        heading-ids (set default-order)
        order* (->> order
                    (map #(some-> % str str/trim not-empty))
                    (remove nil?))
        known-order (filter heading-ids order*)
        ignored (remove heading-ids order*)
        stored-order (fetch-toc-order db book)
        stored-order* (->> stored-order
                           (map #(some-> % str str/trim not-empty))
                           (remove nil?))
        existing-order (if (seq stored-order*)
                         (merge-order (filter heading-ids stored-order*)
                                      default-order)
                         default-order)
        final-order (->> (merge-order known-order existing-order)
                         (remove nil?)
                         vec)
        doc {:xt/id (toc-order-id book)
             :doc/book book
             :doc/toc-order final-order
             :doc/toc-source source
             :doc/toc-updated-at timestamp}]
    ;; Async submit avoids blocking API callers if XTDB indexing stalls.
    (xt/submit! [[::xtdb/put (cond-> doc
                               (nil? source) (dissoc :doc/toc-source)
                               (nil? timestamp) (dissoc :doc/toc-updated-at))]]
                {:await? false})
    (cond-> {:status "ok"
             :book book
             :count (count final-order)}
      (seq ignored) (assoc :ignored-doc-ids (vec (distinct ignored))))))

(defn heading+entries [book doc-id]
  (ensure-xt-node!)
  (let [db (xt/db)
        heading (->> (xt/q db '{:find [(pull ?h [*])]
                                :in [?id]
                                :where [[?h :doc/id ?id]]}
                           doc-id)
                     ffirst)]
    (when heading
      (heading->with-latest db heading))))

(defn- docs-for [db book doc-id]
  (->> (xt/q db '{:find [(pull ?e [*])]
                  :in [?book ?doc-id]
                  :where [[?e :doc/book ?book]
                          [?e :doc/id ?doc-id]]}
             book
             doc-id)
       (map first)))

(defn- doc-xt-ids [docs]
  (->> docs
       (keep :xt/id)
       distinct
       vec))

(defn delete-doc!
  "Delete a docbook heading and its entries by book + doc-id."
  [book doc-id]
  (ensure-xt-node!)
  (let [db (xt/db)
        ids (doc-xt-ids (docs-for db book doc-id))]
    (when (seq ids)
      (xt/submit! (mapv (fn [id] [::xtdb/delete id]) ids)))
    {:book book
     :doc-id doc-id
     :deleted (count ids)}))

(defn delete-toc!
  "Delete a docbook heading from the TOC; optionally cascade to entries."
  ([book doc-id]
   (delete-toc! book doc-id {}))
  ([book doc-id {:keys [cascade?]}]
   (ensure-xt-node!)
   (let [db (xt/db)
         docs (docs-for db book doc-id)
         entries (filter :doc/entry-id docs)
         headings (remove :doc/entry-id docs)
         ids (doc-xt-ids (if cascade?
                           (concat headings entries)
                           headings))]
     (when (seq ids)
       (xt/submit! (mapv (fn [id] [::xtdb/delete id]) ids)))
     {:book book
      :doc-id doc-id
      :deleted (count ids)
      :cascade? (boolean cascade?)})))

(defn recent-entries
  ([book] (recent-entries book 20))
  ([book limit]
   (ensure-xt-node!)
   (let [db (xt/db)
         entries (->> (xt/q db '{:find [(pull ?e [*]) (pull ?h [:doc/id :doc/path_string :doc/book])]
                                 :in [?book]
                                 :where [[?e :doc/book ?book]
                                         [?e :doc/id ?did]
                                         [?h :doc/id ?did]]}
                            book)
                      (map (fn [[e h]]
                             (-> e
                                 normalize-entry
                                 (assoc :doc/heading h))))
                      (remove #(= :removed (:doc/status %)))
                      (sort-by #(or (:doc/timestamp %) "") #(compare %2 %1))
                      (take (or limit 20)))]
     {:book book
      :entries entries})))
