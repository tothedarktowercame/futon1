(require '[xtdb.api :as xt])
(require '[clojure.pprint :refer [pprint]])

(def data-dir (or (System/getenv "BASIC_CHAT_DATA_DIR") "data/default"))
(def rocks-prefix (str data-dir "/xtdb"))

(def node (xt/start-node {:xtdb/tx-log {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                 :db-dir (str rocks-prefix "/tx-log")}}
                          :xtdb/document-store {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                           :db-dir (str rocks-prefix "/doc-store")}}
                          :xtdb/index-store {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                       :db-dir (str rocks-prefix "/index-store")}}}))

(try
  (let [db (xt/db node)
        docs (->> (xt/entity-history db :xt/id {:sort-order :asc :with-docs? true})
                  (take 20))]
    (doseq [doc docs]
      (pprint doc)
      (println "-----")))
  (finally (xt/close node)))
