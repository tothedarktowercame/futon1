(require '[xtdb.api :as xt])
(require '[clojure.pprint :refer [pprint]])

(def data-dir (or (System/getenv "BASIC_CHAT_DATA_DIR") "data/default"))
(def node (xt/start-node {:xtdb/tx-log {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                 :db-dir (str data-dir "/xtdb/tx-log")}}
                          :xtdb/document-store {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                           :db-dir (str data-dir "/xtdb/doc-store")}}
                          :xtdb/index-store {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                                                       :db-dir (str data-dir "/xtdb/index-store")}}}))

(try
  (let [db (xt/db node)
        sample (take 20 (xt/q db '{:find [e a v] :where [[e a v]]}))]
    (println "Sample rows:")
    (doseq [row sample] (pprint row))
    (println "Total entries:" (count sample)))
  (finally (xt/close node)))
