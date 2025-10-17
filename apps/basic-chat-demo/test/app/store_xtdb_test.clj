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
          (xt/sync-node!)
          (let [pat-id (:entity/id entity-doc)
                cutoff (- now (* 7 24 60 60 1000))
                candidates (focus/focus-candidates (xt/node) [pat-id] cutoff 5)
                neighbors (focus/top-neighbors conn (xt/db) pat-id {:k-per-anchor 3})]
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

(deftest forget-entity-prunes-xtdb-and-datascript
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? true
                    :config-path xt-config-path}}
        conn (store/restore! env)
        now (System/currentTimeMillis)
        env-now (assoc env :now now)]
    (try
      (let [how (store/ensure-entity! conn env-now {:name "How" :type :person})
            cambridge (store/ensure-entity! conn env-now {:name "Cambridge" :type :place})
            relation (store/upsert-relation! conn env-now {:type :located-in
                                                           :src {:name "How"}
                                                           :dst {:name "Cambridge"}
                                                           :confidence 0.4})
            result (store/forget-entity! conn env {:name "How"})]
        (testing "entity metadata is returned"
          (is (= (:id how) (get-in result [:entity :id])))
          (is (= (:type how) (get-in result [:entity :type]))))
        (testing "relations referencing the entity are reported"
          (is (= #{(:id relation)} (set (map :id (:relations result))))))
        (testing "entity removed from Datascript"
          (is (nil? (store/resolve-name->eid conn "How"))))
        (testing "entity and relations removed from XTDB"
          (is (empty? (xt/q '{:find [?e]
                               :where [[?e :entity/name "How"]]})))
          (is (nil? (xt/entity (:id relation)))))
        (testing "remaining entities stay intact"
          (is (= (:id cambridge)
                 (:id (store/resolve-name->eid conn "Cambridge"))))))
      (finally
        (xt/stop!)
        (delete-recursive dir)))))

(deftest expire-entity-resets-salience
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? true
                    :config-path xt-config-path}}
        conn (store/restore! env)
        now (System/currentTimeMillis)
        env-now (assoc env :now now)]
    (try
      (let [_ (store/ensure-entity! conn env-now {:name "Pat"
                                                  :type :person
                                                  :seen-count 5
                                                  :pinned? true})
            expired (store/expire-entity! conn env {:name "Pat"})
            doc (-> (xt/q '{:find [(pull ?e [:entity/seen-count :entity/last-seen :entity/pinned?])]
                            :in [?id]
                            :where [[?e :entity/id ?id]]}
                          (:id expired))
                    first first)]
        (testing "Datascript view reflects reset salience"
          (is (= 0 (:seen-count expired)))
          (is (= 0 (:last-seen expired)))
          (is (false? (:pinned? expired))))
        (testing "XTDB mirrors the reset metadata"
          (is (= 0 (:entity/seen-count doc)))
          (is (= 0 (:entity/last-seen doc)))
          (is (false? (:entity/pinned? doc))))
        (testing "expire without target returns nil"
          (is (nil? (store/expire-entity! conn env {:name "Nonexistent"}))))
        (testing "forget without target returns nil"
          (is (nil? (store/forget-entity! conn env {:name "Missing"}))))
        )
      (finally
        (xt/stop!)
        (delete-recursive dir)))))
