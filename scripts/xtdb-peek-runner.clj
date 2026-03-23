(require '[clojure.pprint :refer [pprint]])
(require '[scripts.xtdb-support :as xtdb-support])
(require '[xtdb.api :as xt])

(let [{:keys [node backend store-root]} (xtdb-support/start-node!)
      db (xt/db node)]
  (try
    (let [target-id (or (xtdb-support/parse-id (first *command-line-args*))
                        (first (xtdb-support/first-entity-ids db 1)))
          docs (if target-id
                 (take 20 (xt/entity-history db target-id :asc {:with-docs? true}))
                 [])]
      (println "Backend:" backend)
      (println "Store root:" store-root)
      (println "Target id:" target-id)
      (doseq [doc docs]
        (pprint doc)
        (println "-----"))
      (when-not (seq docs)
        (println "No history entries found.")))
    (finally
      (xtdb-support/close-node! node))))
