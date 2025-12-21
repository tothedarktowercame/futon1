(ns app.store-test
  (:require [app.store :as store]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon1.id :as fid])
  (:import (java.io File)
           (java.util UUID)))

(defn- temp-dir []
  (doto (File. (System/getProperty "java.io.tmpdir")
               (str (gensym "store-test-")))
    (.mkdirs)))

(defn- delete-recursive [^File dir]
  (when (.exists dir)
    (doseq [file (.listFiles dir)]
      (if (.isDirectory file)
        (delete-recursive file)
        (io/delete-file file)))
    (io/delete-file dir true)))

(deftest normalize-entity-spec-allows-string-ids
  (let [raw-id "arxana:article:Demo Article"
        spec (#'app.store/normalize-entity-spec {:name "Demo Article"
                                                :id raw-id})]
    (is (instance? UUID (:id spec)))
    (is (= raw-id (:external-id spec))))
  (let [raw-id "  demo-id  "
        spec (#'app.store/normalize-entity-spec {:name "Demo"
                                                :id raw-id})]
    (is (instance? UUID (:id spec)))
    (is (= (str/trim raw-id) (:external-id spec)))))

(deftest ensure-entity-handles-string-ids
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :now 42
             :xtdb {:enabled? false}}
        conn (store/restore! env)
        entity-id "arxana:article:Demo Article"
        expected-id (:entity/id (fid/coerce-id {:id entity-id
                                                :type :arxana/article}))]
    (try
      (let [entity (store/ensure-entity! conn env {:name "Demo Article"
                                                   :type :arxana/article
                                                   :id entity-id})
            fetched (store/resolve-name->eid conn "Demo Article")]
        (is (= expected-id (:id entity)))
        (is (= expected-id (:id fetched)))
        (is (= entity-id (:external-id entity)))
        (is (= entity-id (:external-id fetched)))
        (is (= :arxana/article (:type entity)))
        (is (= :arxana/article (:type fetched)))
        (is (some? (:version entity)))
        (is (some? (:version fetched))))
      (finally
        (delete-recursive dir)))))

(deftest entity-version-history
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :xtdb {:enabled? false}}
        conn (store/restore! env)
        entity-id "user-42"]
    (try
      (let [_ (store/ensure-entity! conn (assoc env :now 1000)
                                    {:name "User 42"
                                     :type :person
                                     :id entity-id})
            _ (store/ensure-entity! conn (assoc env :now 2000)
                                    {:name "User 42"
                                     :type :person
                                     :id entity-id
                                     :pinned? true})
            history (store/entity-history conn {:id entity-id} {:limit 10})
            versions (:versions history)
            latest (first versions)
            oldest (last versions)
            version-id (:id oldest)
            fetched-by-version (store/fetch-entity conn {:id entity-id}
                                                   {:version-id version-id})
            as-of (store/fetch-entity conn {:id entity-id}
                                      {:as-of (:created-at oldest)})]
        (is (= 2 (count versions)))
        (is (> (:created-at latest) (:created-at oldest)))
        (is (= version-id (get-in fetched-by-version [:version :id])))
        (is (= version-id (get-in as-of [:version :id]))))
      (finally
        (delete-recursive dir)))))
