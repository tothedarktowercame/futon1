(require '[clojure.pprint :refer [pprint]])
(require '[scripts.xtdb-support :as xtdb-support])
(require '[xtdb.api :as xt])

(let [{:keys [node backend store-root]} (xtdb-support/start-node!)
      db (xt/db node)]
  (try
    (let [docs (xtdb-support/first-entity-ids db 20)]
      (println "Backend:" backend)
      (println "Store root:" store-root)
      (doseq [row docs]
        (pprint row))
      (println "rows:" (count docs)))
    (finally
      (xtdb-support/close-node! node))))
