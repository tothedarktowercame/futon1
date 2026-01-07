(ns scripts.docbook-cleanup
  "Clean up docbook headings and toc order for a profile."
  (:require [app.config :as config]
            [app.xt :as xt]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- heading-docs [db]
  (->> (xtdb/q db '{:find [(pull ?h [:xt/id :doc/id :doc/book :doc/outline_path
                                     :doc/path_string :doc/level :doc/title])]
                    :where [[?h :doc/id _]
                            (not [?h :doc/entry-id _])]} )
       (map first)))

(defn- entry-doc-ids [db]
  (->> (xtdb/q db '{:find [?doc-id]
                    :where [[?e :doc/entry-id _]
                            [?e :doc/id ?doc-id]]})
       (map first)
       set))

(defn- infer-book [doc-id]
  (when-let [idx (and doc-id (str/index-of doc-id "-"))]
    (let [prefix (subs doc-id 0 idx)]
      (when (seq prefix) prefix))))

(defn- infer-level [heading]
  (or (:doc/level heading)
      (when-let [outline (:doc/outline_path heading)]
        (count outline))
      (when-let [path (:doc/path_string heading)]
        (count (remove str/blank? (str/split path #"\s*/\s*"))))))

(defn- toc-order [headings]
  (->> headings
       (remove (comp nil? :doc/id))
       (sort-by (juxt #(or (:doc/path_string %) "")
                      #(or (:doc/title %) "")
                      #(or (:doc/id %) "")))
       (map :doc/id)
       vec))

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

(defn -main [& _args]
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
        _ (println (format "Using XTDB config: %s (exists=%s)" cfg-path (.exists (java.io.File. cfg-path))))
        _ (println (format "Using data dir: %s" data-dir))
        _ (xt/start! cfg-path {:data-dir (when data-dir (str (java.io.File. data-dir "xtdb")))
                               :xt/created-by "scripts.docbook-cleanup"})
        db (xtdb/db (xt/node))
        headings (heading-docs db)
        entries (entry-doc-ids db)
        now (System/currentTimeMillis)
        to-delete (filter (fn [heading]
                            (not (contains? entries (:doc/id heading))))
                          headings)
        filtered-headings (remove (set to-delete) headings)
        to-update (->> filtered-headings
                       (map (fn [heading]
                              (let [book (or (:doc/book heading) (infer-book (:doc/id heading)))
                                    level (infer-level heading)]
                                (when (or (nil? (:doc/book heading))
                                          (nil? (:doc/level heading)))
                                  (cond-> (assoc heading :xt/id (:doc/id heading))
                                    book (assoc :doc/book book)
                                    level (assoc :doc/level level))))))
                       (remove nil?))
        headings-by-book (group-by :doc/book (remove #(nil? (:doc/book %)) filtered-headings))]
    (try
      (when (seq to-delete)
        (println (format "Deleting %d stale headings (no entries)." (count to-delete)))
        (xt/submit! (mapv (fn [heading] [::xtdb/delete (:doc/id heading)]) to-delete)))
      (when (seq to-update)
        (println (format "Updating %d headings (book/level)." (count to-update)))
        (xt/submit! (mapv (fn [heading] [::xtdb/put heading]) to-update)))
      (doseq [[book book-headings] headings-by-book]
        (let [order (toc-order book-headings)
              doc {:xt/id (str "docbook-toc-order::" book)
                   :doc/book book
                   :doc/toc-order order
                   :doc/toc-source "cleanup"
                   :doc/toc-updated-at now}]
          (println (format "Updating toc order for %s (%d headings)." book (count order)))
          (xt/submit! [[::xtdb/put doc]])))
      (finally
        (xt/stop!)))))
