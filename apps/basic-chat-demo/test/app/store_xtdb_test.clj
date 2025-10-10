(ns app.store-xtdb-test
  (:require [app.focus :as focus]
            [app.store :as store]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [xtdb.api :as xta])
  (:import (java.io File)))

(defn- temp-dir []
  (doto (File. (System/getProperty "java.io.tmpdir")
               (str (gensym "store-xtdb-")))
    (.mkdirs)))

(defn- delete-recursive [^File dir]
  (when (.exists dir)
    (io/delete-file dir true)))

(def ^:private xt-config-path
  (-> "xtdb-test.edn" io/resource io/file .getAbsolutePath))

(deftest mirrors-entities-and-relations-into-xtdb
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? true
                    :config-path xt-config-path}}
        conn (store/restore! env)
        now (System/currentTimeMillis)
        env-now (assoc env :now now)]
    (try
      (let [pat (store/ensure-entity! conn env-now {:name "Pat" :type :person})
            boston (store/ensure-entity! conn env-now {:name "Boston" :type :place})
            rel (store/upsert-relation! conn env-now {:type :links-to
                                                      :src {:name "Pat"}
                                                      :dst {:name "Boston"}
                                                      :confidence 0.85})
            db (xt/db)
            entity-doc (-> (xta/q db '{:find [(pull ?e [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen :entity/pinned?])]
                                        :where [[?e :entity/name "Pat"]]})
                           first first)
            rel-doc (-> (xta/q db '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst :relation/confidence :relation/last-seen])]
                                      :where [[?r :relation/id _]]})
                       first first)]
        (is (xt/started?) "XTDB node should be active")
        (testing "entity mirrors include salience fields"
          (is entity-doc)
          (is (= (:id pat) (:entity/id entity-doc)))
          (is (= (:type pat) (:entity/type entity-doc)))
          (is (>= (:entity/seen-count entity-doc) (:seen-count pat)))
          (is (>= (:entity/last-seen entity-doc) (:last-seen pat)))
          (is (boolean? (:entity/pinned? entity-doc)))
          (is (false? (:entity/pinned? entity-doc))))
        (testing "relation mirrors include endpoints"
          (is rel-doc)
          (is (= (:id rel) (:relation/id rel-doc)))
          (is (= (:type rel) (:relation/type rel-doc)))
          (is (= (:id pat) (:relation/src rel-doc)))
          (is (= (:id boston) (:relation/dst rel-doc)))
          (is (= (:confidence rel) (:relation/confidence rel-doc)))
          (is (= (:last-seen rel) (:relation/last-seen rel-doc))))
        (testing "restore hydrates Datascript from XTDB"
          (let [conn' (store/restore! env)
                hydrated (store/resolve-name->eid conn' "Pat")]
            (is hydrated)
            (is (= (:id hydrated) (:entity/id entity-doc)))
            (is (= (:seen-count hydrated) (:entity/seen-count entity-doc)))
            (is (= (:last-seen hydrated) (:entity/last-seen entity-doc)))))
        (testing "salience pulls from XTDB graph"
          (let [pat-id (:entity/id entity-doc)
                cutoff (- now (* 7 24 60 60 1000))
                candidates (focus/focus-candidates nil [pat-id] cutoff 5)
                neighbors (focus/top-neighbors nil pat-id {:k-per-anchor 3})]
            (is (seq candidates))
            (let [top (first candidates)]
              (is (= pat-id (:id top)))
              (is (:anchor? top))
              (is (> (:score top) 0.0)))
            (is (seq neighbors))
            (let [entry (first neighbors)]
              (is (= "Boston" (-> entry :neighbor :entity/name)))
              (is (> (:score entry) 0.0))))))
      (finally
        (xt/stop!)
        (delete-recursive dir)))))
