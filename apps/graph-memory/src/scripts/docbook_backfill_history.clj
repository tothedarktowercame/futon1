(ns scripts.docbook-backfill-history
  "Backfill docbook raw JSON entries from XTDB history when placeholders exist."
  (:require [app.config :as config]
            [app.xt :as xt]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M -m scripts.docbook-backfill-history [--book BOOK] [--dry-run]"
             "Defaults:"
             "  BOOK=futon4"]))

(defn- parse-args [args]
  (loop [opts {:book "futon4" :dry-run? false :help? false}
         remaining args]
    (if-let [arg (first remaining)]
      (cond
        (#{"-h" "--help"} arg)
        (recur (assoc opts :help? true) (rest remaining))

        (#{"-b" "--book"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :book value) (nnext remaining))
          (throw (ex-info "Missing value for --book" {})))

        (#{"--dry-run"} arg)
        (recur (assoc opts :dry-run? true) (rest remaining))

        :else
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- find-repo-root []
  (loop [dir (.getCanonicalFile (java.io.File. "."))]
    (let [candidate (java.io.File. dir "config.edn")]
      (cond
        (.exists candidate) (.getAbsolutePath dir)
        (nil? (.getParentFile dir)) nil
        :else (recur (.getParentFile dir))))))

(defn- resolve-config-path [path]
  (let [file (java.io.File. path)]
    (if (.isAbsolute file)
      (.getAbsolutePath file)
      (if-let [root (find-repo-root)]
        (.getAbsolutePath (java.io.File. root path))
        (.getAbsolutePath file)))))

(defn- resolve-data-dir [root path]
  (let [file (java.io.File. path)]
    (if (.isAbsolute file)
      (.getAbsolutePath file)
      (if root
        (.getAbsolutePath (java.io.File. root path))
        (.getAbsolutePath file)))))

(defn- placeholder? [text]
  (when (string? text)
    (str/includes? (str/lower-case text) "no summary yet")))

(defn- load-json [path]
  (when (and path (.exists (io/file path)))
    (with-open [r (io/reader path)]
      (json/read r :key-fn keyword))))

(defn- ensure-parent-dir! [path]
  (let [file (io/file path)
        parent (.getParentFile file)]
    (when parent
      (.mkdirs parent)))
  path)

(defn- write-json! [path payload]
  (ensure-parent-dir! path)
  (let [out (json/write-str payload :escape-slash false)]
    (spit path (str out "\n"))))

(defn- raw-path [root book doc-id]
  (io/file root "data" "logs" "books" book "raw" (str doc-id "::org.json")))

(defn- history-body [db entry-id]
  (let [history (xtdb/entity-history db entry-id :desc {:with-docs? true})]
    (some (fn [item]
            (let [doc (:xtdb.api/doc item)
                  body (or (:doc/body doc) (:doc/context doc))]
              (when (and (string? body)
                         (not (placeholder? body)))
                doc)))
          history)))

(defn- entry-entities [db book]
  (->> (xtdb/q db '{:find [(pull ?e [:doc/id :doc/entry-id :doc/book :doc/body :doc/title :doc/source-doc])]
                    :in [?book]
                    :where [[?e :doc/book ?book]
                            [?e :doc/entry-id _]]}
                book)
       (map first)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (if (:help? opts)
      (println (usage))
      (let [root (find-repo-root)
            _ (when (and root
                         (nil? (System/getenv "FUTON_CONFIG"))
                         (nil? (System/getProperty "futon.config")))
                (System/setProperty "futon.config"
                                    (.getAbsolutePath (java.io.File. root "config.edn"))))
            cfg (config/config)
            cfg-path (resolve-config-path
                      (:xtdb/config-path cfg "apps/graph-memory/resources/xtdb.edn"))
            data-dir (resolve-data-dir root (:app/data-dir cfg))
            dry-run? (:dry-run? opts)]
        (println (format "Using XTDB config: %s (exists=%s)" cfg-path (.exists (java.io.File. cfg-path))))
        (println (format "Using data dir: %s" data-dir))
        (println (format "Book: %s (dry-run=%s)" (:book opts) dry-run?))
        (xt/start! cfg-path {:data-dir (when data-dir (str (java.io.File. data-dir "xtdb")))
                             :xt/created-by "scripts.docbook-backfill-history"})
        (try
          (let [db (xtdb/db (xt/node))
                entries (entry-entities db (:book opts))
                placeholder-entries (filter (fn [entry]
                                              (placeholder? (:doc/body entry)))
                                            entries)]
            (println (format "Found %d placeholder entries." (count placeholder-entries)))
            (doseq [entry placeholder-entries]
              (let [doc-id (:doc/id entry)
                    entry-id (:doc/entry-id entry)
                    raw (raw-path root (:book opts) doc-id)
                    hist-doc (history-body db entry-id)]
                (if-not hist-doc
                  (println (format "No history body for %s" doc-id))
                  (let [payload (or (load-json raw)
                                    {:doc_id doc-id
                                     :run_id entry-id
                                     :book_id (:book opts)
                                     :version "org"})]
                    (println (format "Backfilling %s -> %s" doc-id (.getPath raw)))
                    (when-not dry-run?
                      (write-json! raw
                                   (cond-> payload
                                     (:doc/body hist-doc) (assoc :doc/body (:doc/body hist-doc))
                                     (:doc/context hist-doc) (assoc :doc/body (:doc/context hist-doc))
                                     (:doc/title hist-doc) (assoc :doc/title (:doc/title hist-doc))
                                     (:doc/source-doc hist-doc) (assoc :doc/source-doc (:doc/source-doc hist-doc))))))))))
          (finally
            (xt/stop!)))))))
