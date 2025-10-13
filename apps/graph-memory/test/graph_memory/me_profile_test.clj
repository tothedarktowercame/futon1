(ns graph-memory.me-profile-test
  (:require [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [graph-memory.me-profile :as me-profile])
  (:import (java.io File)
           (java.util UUID)))

(defn- temp-dir []
  (doto (File. (System/getProperty "java.io.tmpdir")
               (str (gensym "me-profile-")))
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

(deftest preferences-from-manual-extracts-identifiers
  (let [uuid-a (UUID/randomUUID)
        uuid-b (UUID/randomUUID)
        doc {:name "Joe"
             :aliases ["Joseph" "J."]
             :handles [:futon]
             :entity-id (str uuid-a)
             :primary-entity {:id uuid-b
                              :name "Joseph A."}}
        prefs (me-profile/preferences-from-manual doc)]
    (is (= [uuid-a uuid-b] (:preferred-ids prefs)))
    (is (= ["Joe" "Joseph" "J." "futon" "Joseph A."]
           (:preferred-names prefs)))))

(deftest profile-builds-hot-relations
  (let [now 1700000000000
        me-id (UUID/randomUUID)
        friend-id (UUID/randomUUID)
        project-id (UUID/randomUUID)
        rel-out (UUID/randomUUID)
        rel-in (UUID/randomUUID)]
    (xt/put-entity! {:entity/id me-id
                     :entity/name "Joe"
                     :entity/type :person
                     :entity/seen-count 12
                     :entity/last-seen (- now 1000)
                     :entity/pinned? true})
    (xt/put-entity! {:entity/id friend-id
                     :entity/name "Pat"
                     :entity/type :person
                     :entity/seen-count 8
                     :entity/last-seen (- now 2000)})
    (xt/put-entity! {:entity/id project-id
                     :entity/name "Project Phoenix"
                     :entity/type :project
                     :entity/seen-count 5
                     :entity/last-seen (- now 500)})
    (xt/put-rel! {:relation/id rel-out
                  :relation/type :works-on
                  :relation/src me-id
                  :relation/dst project-id
                  :relation/confidence 0.92
                  :relation/last-seen (- now 200)
                  :relation/provenance {:role "lead"}}
                 nil nil)
    (xt/put-rel! {:relation/id rel-in
                  :relation/type :advisor-of
                  :relation/src friend-id
                  :relation/dst me-id
                  :relation/confidence 0.7
                  :relation/last-seen (- now 2000)
                  :relation/provenance {:since "2005"}}
                 nil nil)
    (let [profile (me-profile/profile {:preferred-ids [me-id]
                                       :neighbor-limit 5
                                       :window-days 60
                                       :now now})
          relations (:relations profile)
          topics (:topics profile)
          salience (:salience profile)]
      (is (= me-id (get-in profile [:entity :id])))
      (is (= 2 (count relations)))
      (is (= :works-on (-> relations first :type)))
      (is (= :out (-> relations first :direction)))
      (is (= project-id (-> relations first :neighbor :id)))
      (is (= :in (-> relations second :direction)))
      (is (= friend-id (-> relations second :neighbor :id)))
      (is (= :works-on (-> topics first :type)))
      (is (= ["Project Phoenix"] (-> topics first :neighbors)))
      (is (= 12 (get salience :seen-count)))
      (is (= 60 (get-in salience [:window :days])))
      (is (= now (:generated-at profile))))))

(deftest relation-lines-with-chains-groups-linked-relations
  (let [relations [{:id :r1
                     :type :aim/has
                     :direction :out
                     :subject "Joe"
                     :object "long term aim"
                     :score 1.0}
                    {:id :r2
                     :type :structure/is
                     :direction :out
                     :subject "long term aim"
                     :object "translate good faith into structure"
                     :score 0.8}
                    {:id :r3
                     :type :design/requires
                     :direction :out
                     :subject "translate good faith into structure"
                     :object "iterative design"
                     :score 0.7}]
        lines (#'graph-memory.me-profile/relation-lines-with-chains "Joe" relations)]
    (is (= 3 (count lines)))
    (is (str/starts-with? (first lines) "- Joe —has → long term aim"))
    (is (str/starts-with? (second lines) "    —is → translate good faith into structure"))
    (is (str/starts-with? (nth lines 2) "        —requires → iterative design"))))


(deftest summary-reflects-profile-and-manual
  (let [now 1700000000000
        me-id (UUID/randomUUID)
        project-id (UUID/randomUUID)
        rel-id (UUID/randomUUID)
        manual {:headline "Operating at the intersection of product and research"
                :tags ["graph" "memory"]}]
    (xt/put-entity! {:entity/id me-id
                     :entity/name "Joe"
                     :entity/type :person
                     :entity/seen-count 10
                     :entity/last-seen (- now 300)
                     :entity/pinned? true})
    (xt/put-entity! {:entity/id project-id
                     :entity/name "Project Phoenix"
                     :entity/type :project
                     :entity/seen-count 4
                     :entity/last-seen (- now 400)})
    (xt/put-rel! {:relation/id rel-id
                  :relation/type :works-on
                  :relation/src me-id
                  :relation/dst project-id
                  :relation/confidence 0.95
                  :relation/last-seen (- now 100)}
                 nil nil)
    (let [profile (me-profile/profile {:preferred-ids [me-id]
                                       :now now})
          text (me-profile/summary profile {:manual manual
                                             :limit 400})]
      (is (<= (count text) 400))
      (is (re-find #"Joe" text))
      (is (re-find #"works on" text))
      (is (re-find #"Project Phoenix" text))
      (is (re-find #"Operating at the intersection" text))
      (is (re-find #"Manual notes" text))
      (is (re-find #"Generated:" text)))))

(deftest profile-and-summary-handle-empty-relations
  (let [now 1700000000000
        me-id (UUID/randomUUID)]
    (xt/put-entity! {:entity/id me-id
                     :entity/name "Joe"
                     :entity/type :person
                     :entity/seen-count 1
                     :entity/last-seen (- now 100)
                     :entity/pinned? true})
    (let [profile (me-profile/profile {:preferred-ids [me-id]
                                       :now now
                                       :window-days 30})
          text (me-profile/summary profile {:limit 400})]
      (is (= me-id (get-in profile [:entity :id])))
      (is (empty? (:relations profile)))
      (is (re-find #"Hot relations:" text))
      (is (re-find #"none in window" text)))))
