#!/usr/bin/env clojure
;; Quick XTDB inspector: lists entity counts and prints sample docs.

(require '(xtdb [api :as xt]))
(require '(clojure.java.io :as io))
(require '(clojure.pprint :refer [pprint]))
(require '(clojure.string :as str))

(def cfg-path (or (System/getenv "XTDB_CONFIG")
                  "apps/graph-memory/resources/xtdb.edn"))
(def data-dir (or (System/getenv "BASIC_CHAT_DATA_DIR")
                  "data/default"))

(def node (xt/start-node {:xtdb/tx-log {:kv-store {:xtdb/module 'xtdb.lmdb/->kv-store
                                                  :db-dir (str data-dir "/xtdb/tx-log")}}
                          :xtdb/document-store {:kv-store {:xtdb/module 'xtdb.lmdb/->kv-store
                                                            :db-dir (str data-dir "/xtdb/doc-store")}}
                          :xtdb/index-store {:kv-store {:xtdb/module 'xtdb.lmdb/->kv-store
                                                         :db-dir (str data-dir "/xtdb/index-store")}}}))

(try
  (let [db (xt/db node)
        q '{:find [e attr value]
            :in []
            :where [[e attr value]]}
        first-rows (take 20 (xt/q db q))]
    (println "Sample documents (up to 20 rows):")
    (doseq [[e a v] first-rows]
      (println e a v))
    (println)
    (println "Entity count:" (count (set (map first first-rows)))))
  (finally
    (xt/close node)))
