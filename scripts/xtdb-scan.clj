(require '[xtdb.api :as xt])
(require '[clojure.pprint :refer [pprint]])

(def data-dir (or (System/getenv "BASIC_CHAT_DATA_DIR") "apps/demo/data/default"))
(def node (xt/start-node {:xtdb/tx-log {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                 :db-dir (str data-dir "/xtdb/tx-log")}}
                          :xtdb/document-store {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                           :db-dir (str data-dir "/xtdb/doc-store")}}
                          :xtdb/index-store {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                       :db-dir (str data-dir "/xtdb/index-store")}}}))

(try
  (let [db (xt/db node)
        docs (take 20 (xt/q db '{:find [e attr val]
                                :where [[e attr val]]}))]
    (doseq [row docs]
      (pprint row))
    (println "rows:" (count docs)))
  (finally (xt/close node)))
