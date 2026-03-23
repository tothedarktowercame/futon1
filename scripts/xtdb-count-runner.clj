(require '[clojure.pprint :refer [pprint]])
(require '[scripts.xtdb-support :as xtdb-support])
(require '[xtdb.api :as xt])

(let [{:keys [node backend store-root]} (xtdb-support/start-node!)
      db (xt/db node)]
  (try
    (let [sample (xtdb-support/first-entity-ids db 20)
          entity-count (ffirst (xt/q db '{:find [(count-distinct ?e)]
                                       :where [[?e :xt/id _]]}))]
      (println "Sample entity ids:")
      (doseq [row sample] (pprint row))
      (println "Backend:" backend)
      (println "Store root:" store-root)
      (println "Distinct entities:" entity-count))
    (finally
      (xtdb-support/close-node! node))))
