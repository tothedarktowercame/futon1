(ns app.docbook
  "Docbook ingest/query utilities for XTDB."
  (:require [app.config :as config]
            [app.xt :as xt]
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

(defn- heading-doc [m]
  (let [doc-id (or (:doc_id m) (:doc/id m))
        outline (or (:doc/outline_path m) (:outline_path m))
        path-str (or (:doc/path_string m) (:path_string m))
        level (or (:doc/level m) (:level m))
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
             :doc/context body}
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
     (fn [{:keys [txs stats]} file]
       (let [rel-path (str/replace (.getCanonicalPath (io/file file))
                                   (str root-path "/")
                                   "")
             sections (org-sections file)]
         (reduce
          (fn [{:keys [txs stats]} {:keys [outline level title body]}]
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
                  stats (-> stats
                            (update :total (fnil inc 0))
                            (update-in [:levels level] (fnil inc 0))
                            (cond-> function-name
                              (update :functions (fnil inc 0))))
                  txs (into txs [[::xtdb/put heading]
                                 [::xtdb/put entry]])]
              {:txs txs :stats stats}))
          {:txs txs :stats stats}
          sections)))
     {:txs [] :stats {:total 0 :levels {} :functions 0 :files (count files)}}
     files)))

(defn- entry-doc [book m]
  (let [doc-id (or (:doc_id m) (:doc/id m))
        entry-id (or (:doc/entry-id m) (:doc_entry_id m) (:doc/entry_id m) (:entry_id m) (:run_id m))]
    (when (and doc-id entry-id)
      (let [status (normalize-status (or (:doc/status m) (:status m)))]
        (cond-> {:xt/id entry-id
                 :doc/id doc-id
                 :doc/entry-id entry-id
                 :doc/book book
                 :doc/version (or (:version m) (:doc/version m))
                 :doc/timestamp (or (:timestamp m) (:doc/timestamp m))
                 :doc/status status}
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
          (:doc/context m) (assoc :doc/context (:doc/context m))
          (:doc/delta m) (assoc :doc/delta (:doc/delta m))
          (:doc/verification m) (assoc :doc/verification (:doc/verification m))
          (:doc/toc-version m) (assoc :doc/toc-version (:doc/toc-version m))
          (:notes m) (assoc :doc/notes (:notes m)))))))

(defn- read-json [path]
  (with-open [r (io/reader path)]
    (json/read r :key-fn keyword)))

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
        base (io/file root "dev/logs/books" book)
        raw-dir (io/file base "raw")
        toc (io/file base "toc.json")
        txs-acc (transient [])]
    (when (.exists toc)
      (doseq [h (read-json toc)]
        (conj! txs-acc [::xtdb/put (heading-doc h)])))
    (doseq [f (entry-files raw-dir)]
      (let [payload (read-json f)
            heading (heading-doc payload)
            entry (entry-doc book payload)]
        (when heading
          (conj! txs-acc [::xtdb/put heading]))
        (when entry
          (conj! txs-acc [::xtdb/put entry]))))
    (let [{:keys [txs stats]} (org-doc-txs root book)]
      (doseq [tx txs]
        (conj! txs-acc tx))
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
    {:book book
     :headings (when (.exists toc) (count (read-json toc)))
     :entries (count (entry-files raw-dir))}))

(defn- latest-entry [entries]
  (->> entries
       (remove #(= :removed (:doc/status %)))
       (sort-by (juxt #(or (:doc/timestamp %) "") #(or (:doc/version %) ""))
                #(compare %2 %1))
       first))

(defn- entries-for-doc [db doc-id]
  (->> (xt/q db '{:find [(pull ?e [*])]
                  :in [?doc-id]
                  :where [[?e :doc/id ?doc-id]]}
             doc-id)
       (map first)))

(defn heading->with-latest [db heading]
  (let [entries (entries-for-doc db (:doc/id heading))]
    (assoc heading
           :doc/entries (sort-by #(or (:doc/timestamp %) "") #(compare %2 %1) entries)
           :doc/latest (latest-entry entries))))

(defn contents [book]
  (ensure-xt-node!)
  (let [db (xt/db)
        headings (->> (xt/q db '{:find [(pull ?h [*])]
                                 :in [?book]
                                 :where [[?h :doc/book ?book]]}
                            book)
                      (map first)
                      (map #(heading->with-latest db %)))]
    {:book book
     :headings headings}))

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
                      (map (fn [[e h]] (assoc e :doc/heading h)))
                      (remove #(= :removed (:doc/status %)))
                      (sort-by #(or (:doc/timestamp %) "") #(compare %2 %1))
                      (take (or limit 20)))]
     {:book book
      :entries entries})))
