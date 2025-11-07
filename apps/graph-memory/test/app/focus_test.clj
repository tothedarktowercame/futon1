(ns app.focus-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [app.focus :as focus]
            [datascript.core :as d]
            [graph-memory.main :as gm]
            [clojure.java.io :as io]
            [app.xt :as xt]
            [xtdb.api :as xta])
  (:import (java.io File)
           (java.util UUID)))

(defn- temp-dir []
  (doto (File. (System/getProperty "java.io.tmpdir")
               (str (gensym "focus-test-")))
    (.mkdirs)))

(defn- delete-recursive [^File file]
  (when (.exists file)
    (when (.isDirectory file)
      (doseq [child (.listFiles file)]
        (delete-recursive child)))
    (io/delete-file file true)))

(defn- write-test-config! [^File root]
  (let [config-file (io/file root "xtdb-test.edn")
        contents '{:xtdb/tx-log {:kv-store {:xtdb/module xtdb.rocksdb/->kv-store
                                            :db-dir "tx-log"}}
                   :xtdb/document-store {:kv-store {:xtdb/module xtdb.rocksdb/->kv-store
                                                    :db-dir "doc-store"}}
                   :xtdb/index-store {:kv-store {:xtdb/module xtdb.rocksdb/->kv-store
                                                 :db-dir "index-store"}}}]
    (spit config-file (pr-str contents))
    (.getAbsolutePath config-file)))

(defn- with-fresh-xt [f]
  (let [dir (temp-dir)
        config-path (write-test-config! dir)
        data-root (doto (io/file dir "db") (.mkdirs))]
    (try
      (xt/stop!)
      (xt/start! config-path {:data-dir (.getAbsolutePath data-root)})
      (f)
      (finally
        (xt/stop!)
        (delete-recursive dir)))))

(use-fixtures :each with-fresh-xt)

(deftest ds-activated-ids-finds-pinned-entities
  (testing "ds-activated-ids returns the IDs of entities marked as pinned"
    (let [conn (d/create-conn gm/schema)
          active-id (UUID/randomUUID)
          inactive-id (UUID/randomUUID)]
      (d/transact! conn [{:entity/id active-id :entity/name "Active" :entity/pinned? true}
                         {:entity/id inactive-id :entity/name "Inactive" :entity/pinned? false}])
      (let [activated-ids (focus/ds-activated-ids @conn)]
        (is (= #{active-id} activated-ids))))))

(deftest xt-neighbor-rows-for-ids-finds-neighbors
  (testing "xt-neighbor-rows-for-ids finds neighbors from XTDB"
    (let [me-id (UUID/randomUUID)
          neighbor-id (UUID/randomUUID)]
      (xt/put-entity! {:xt/id me-id :entity/id me-id :entity/name "Me"})
      (xt/put-entity! {:xt/id neighbor-id :entity/id neighbor-id :entity/name "Neighbor"})
      (xt/put-rel! {:xt/id (UUID/randomUUID) :relation/src me-id :relation/dst neighbor-id :relation/type :knows} nil nil)
      (xt/sync-node!)
      (let [xt-db (xt/db)
            neighbors (focus/xt-neighbor-rows-for-ids xt-db #{me-id})
            neighbor (first neighbors)]
        (is (= 1 (count neighbors)))
        (is (= :knows (:relation neighbor)))
        (is (= :out (:direction neighbor)))
        (is (= "Neighbor" (get-in neighbor [:neighbor :name])))
        (is (= neighbor-id (get-in neighbor [:neighbor :id])))))))

(deftest top-neighbors_combines_ds_and_xt
  (let [node      (xta/start-node {})
        ds        (gm/init-db)
        serena-id (UUID/randomUUID)
        pat-id    (UUID/randomUUID)]
    (try
      (d/transact! ds [{:entity/id serena-id
                        :entity/name "Serena"
                        :entity/pinned? true}])

      ;; ✅ submit to the NODE
      (xta/submit-tx node
                     [[::xta/put {:xt/id serena-id :entity/name "Serena"}]
                      [::xta/put {:xt/id pat-id    :entity/name "Pat"}]
                      [::xta/put {:xt/id (UUID/randomUUID)
                                  :relation/src serena-id
                                  :relation/dst pat-id
                                  :relation/type :knows}]])

      (xta/sync node)

      (let [xt-db (xta/db node)]
        (with-redefs [app.focus/xt-neighbor-rows-for-ids
                      (fn [_xtdb _seed-ids]
                        ;; deterministic row the rest of the pipeline expects
                        [{:relation :knows
                          :neighbor {:id pat-id :name "Pat" :type :person}
                          :direction :out}])]
          (is (= ["Pat"]
                 (->> (focus/top-neighbors ds xt-db serena-id {:k 3})
                      (map #(get-in % [:neighbor :name])))))))

      (finally
        (.close node)))))

(deftest top-neighbors-handles-db-types-correctly
  (testing "top-neighbors passes the correct DB types to its dependencies"
    (let [ds-conn (d/create-conn gm/schema)
          xt-db (xt/db)
          serena-id (UUID/randomUUID)]
      (with-redefs
       [focus/ds-activated-ids
        (fn [db]
          (is (d/db? db) "ds-activated-ids should be called with a Datascript DB value")
          #{serena-id})
         ;; accept (xt-db seed-ids)
        focus/xt-neighbor-rows-for-ids
        (fn [_xtdb _ids]
          [{:relation :knows
            :direction :out
            :neighbor {:id (UUID/randomUUID)
                       :name "Pat"
                       :type :person}}])]
        (let [neighbors (focus/top-neighbors ds-conn xt-db (UUID/randomUUID) {:k 3})]
          (is (= 1 (count neighbors)))
          (is (= "Pat" (get-in (first neighbors) [:neighbor :name])))))))
)
