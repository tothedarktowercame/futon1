#!/usr/bin/env clojure
;; Quick XTDB inspector: lists entity counts and prints sample docs.

(require '[clojure.pprint :refer [pprint]])
(require '[scripts.xtdb-support :as xtdb-support])
(require '[xtdb.api :as xt])

(let [{:keys [node backend store-root]} (xtdb-support/start-node!)
      db (xt/db node)]
  (try
    (let [entity-ids (xtdb-support/first-entity-ids db 10)]
      (println "Backend:" backend)
      (println "Store root:" store-root)
      (println "Sample documents:")
      (if (seq entity-ids)
        (doseq [entity-id entity-ids]
          (pprint (xt/entity db entity-id))
          (println "-----"))
        (println "No documents found.")))
    (finally
      (xtdb-support/close-node! node))))
