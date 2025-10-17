(ns graph-memory.me-profile-test
  (:require [app.xt :as xt]
            [xtdb.api :as xta]
            [datascript.core :as d]
            [graph-memory.main :as gm]
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
  (let [now        1700000000000
        me-id      (UUID/randomUUID)
        friend-id  (UUID/randomUUID)
        project-id (UUID/randomUUID)
        rel-out    (UUID/randomUUID)
        rel-in     (UUID/randomUUID)
        ds-conn    (d/create-conn gm/schema)
        xt-db      (xta/db (xta/start-node {}))]    ;; or use your fixture's node/db
    ;; no need to seed real XT/DS for this unit: we stub below

    (with-redefs
     [graph-memory.me-profile/hot-relations
      (fn [_ds _xt me _opts]
        (if (= (:id me) :me)              ;; avoid duplicate “:me bucket”
          []
          [{:id rel-out
            :type :works-on :direction :out :score 0.92
            :neighbor {:id project-id :name "Project Phoenix" :type :project}}
           {:id rel-in
            :type :advisor-of :direction :in :score 0.70
            :neighbor {:id friend-id :name "Pat" :type :person}}]))

      graph-memory.me-profile/build-topics
      (fn [_rels _limit]
        [{:type :works-on :neighbors ["Project Phoenix"]}])]

      (let [profile (me-profile/profile
                     {:db ds-conn
                      :xt-db xt-db
                   ;; ✅ provide the :me entity explicitly
                      :entity {:id me-id, :name "Joe", :type :person
                               :seen-count 12, :last-seen (- now 1000), :pinned? true}
                      :neighbor-limit 5
                      :window-days 60
                      :now now})
            relations (:relations profile)
            topics    (:topics profile)
            salience  (:salience profile)]
        (is (= me-id (get-in profile [:entity :id])))
        (is (= 2 (count relations)))
        (is (= :works-on (-> relations first :type)))
        (is (= :out      (-> relations first :direction)))
        (is (= project-id (-> relations first :neighbor :id)))
        (is (= :in       (-> relations second :direction)))
        (is (= friend-id (-> relations second :neighbor :id)))
        (is (= :works-on (-> topics first :type)))
        (is (= ["Project Phoenix"] (-> topics first :neighbors)))
        (is (= 60 (get-in salience [:window :days])))
        (is (= now (:generated-at profile)))))))

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
  (let [now        1700000000000
        me-id      (UUID/randomUUID)
        project-id (UUID/randomUUID)
        rel-id     (UUID/randomUUID)
        manual     {:headline "Operating at the intersection of product and research"
                    :tags ["graph" "memory"]}
        node       (xta/start-node {})]
    (try
      ;; --- seed XT ---
      (xta/submit-tx node
                     [[::xta/put {:xt/id me-id
                                  :entity/name "Joe"
                                  :entity/type :person
                                  :entity/seen-count 10
                                  :entity/last-seen (- now 300)
                                  :entity/pinned? true}]
                      [::xta/put {:xt/id project-id
                                  :entity/name "Project Phoenix"
                                  :entity/type :project
                                  :entity/seen-count 4
                                  :entity/last-seen (- now 400)}]
                      [::xta/put {:xt/id rel-id
                                  :relation/type :works-on
                                  :relation/src  me-id
                                  :relation/dst  project-id
                                  :relation/confidence 0.95
                                  :relation/last-seen (- now 100)}]])
      (xta/sync node)

      ;; --- build DS + activation (optional but helps waterfall) ---
      (let [ds-conn (d/create-conn gm/schema)
            _       (d/transact! ds-conn [{:entity/id me-id
                                           :entity/name "Joe"
                                           :entity/pinned? true}])
            xt-db   (xta/db node)]

        (with-redefs
         [graph-memory.me-profile/hot-relations
          (fn [_ds _xt _me _opts]
            [{:id rel-id
              :type :works-on
              :direction :out
              :score 0.95
              :neighbor {:id project-id :name "Project Phoenix" :type :project}}])]

          (let [profile (me-profile/profile {:db ds-conn
                                             :xt-db xt-db
                                             :entity {:id me-id :name "Joe" :type :person
                                                      :seen-count 10 :last-seen (- now 300) :pinned? true}
                                             :now now
                                             ;; (optionally) set window-days if you want a specific number:
                                             ;; :window-days 45
                                             })
                text (me-profile/summary profile {:manual manual :limit 400})]
            (is (<= (count text) 400))
            (is (re-find #"Joe" text))
            (is (re-find #"works on" text))
            (is (re-find #"Project Phoenix" text))
            (is (re-find #"Operating at the intersection" text))
            (is (re-find #"Manual notes" text))
            (is (re-find #"Generated:" text)))))

      (finally
        (.close node)))))

(deftest profile-and-summary-handle-empty-relations
  (let [now   1700000000000
        me-id (UUID/randomUUID)]
    ;; seed a :me candidate so select-me can find it (or adapt to your put helper)
    (xt/put-entity! {:entity/id        me-id
                     :entity/name      "Joe"
                     :entity/type      :person
                     :entity/seen-count 1
                     :entity/last-seen (- now 100)
                     :entity/pinned?   true})
    ;; Stub helpers so we don't touch any futures/concurrency paths.
    (with-redefs [;; Ensure no relations are returned at all
                  me-profile/hot-relations
                  (fn [& _] [])
                  ;; If profile tries to build topics, return none
                  me-profile/build-topics
                  (fn [& _] [])
                  ;; If summary tries to expand relation chains, return none
                  me-profile/relation-lines-with-chains
                  (fn [& _] [])]
      (let [profile (me-profile/profile {:preferred-ids [me-id]
                                         :now now
                                         :window-days 30})
            text    (me-profile/summary profile {:limit 400})]
        (is (= me-id (get-in profile [:entity :id])))
        (is (empty? (:relations profile)))
        (is (re-find #"Hot relations:" text))
        (is (re-find #"none in window" text))))))
