(ns app.focus-header-test
  (:require [app.focus.header :as focus-header]
            [clojure.test :refer [deftest is testing]]
            [datascript.core :as d])
  (:import (java.util UUID)))

(def ^:private schema
  {:entity/id {:db/unique :db.unique/identity}
   :relation/id {:db/unique :db.unique/identity}
   :relation/src {:db/valueType :db.type/ref}
   :relation/dst {:db/valueType :db.type/ref}})

(defn- new-conn []
  (d/create-conn schema))

(defn- transact! [conn tx]
  (d/transact! conn tx)
  conn)

(defn- seed-sample []
  (let [conn (new-conn)
        ts   1000
        alice -1
        bob   -2
        carl  -3]
    (-> conn
        (transact! [{:db/id alice
                     :entity/id (UUID/randomUUID)
                     :entity/name "Alice"
                     :entity/type :person
                     :entity/seen-count 3
                     :entity/last-seen ts}
                    {:db/id bob
                     :entity/id (UUID/randomUUID)
                     :entity/name "Bob"
                     :entity/type :person
                     :entity/seen-count 2
                     :entity/last-seen ts}
                    {:db/id carl
                     :entity/id (UUID/randomUUID)
                     :entity/name "Carl"
                     :entity/type :person
                     :entity/seen-count 1
                     :entity/last-seen (dec ts)}
                    {:relation/id (UUID/randomUUID)
                     :relation/type :likes
                     :relation/src alice
                     :relation/dst bob
                     :relation/last-seen ts}
                    {:relation/id (UUID/randomUUID)
                     :relation/type :knows
                     :relation/src alice
                     :relation/dst carl
                     :relation/last-seen (dec ts)}]))))

(deftest recent-relations-accepts-option-map
  (let [conn (seed-sample)
        rels (focus-header/recent-relations conn {:limit 1})]
    (is (= 1 (count rels)))
    (is (contains? #{:likes :knows} (:type (first rels)))))
  (testing "exclude removes matching tuples"
    (let [conn (seed-sample)
          rels* (focus-header/recent-relations conn {:limit 2
                                                     :exclude [[:likes "Alice" "Bob"]]})]
      (is (= [:knows] (map :type rels*))))))

(deftest build-produces-sections-and-fallback
  (let [conn (seed-sample)
        opts {:recent-limit 5}
        result (focus-header/build conn
                                   {:entities [{:name "Alice" :type :person}
                                               {:name "Bob" :type :person}]
                                    :relations [{:type :likes
                                                 :src {:name "Alice" :type :person}
                                                 :dst {:name "Bob" :type :person}}]}
                                   []
                                   opts)
        sections (:sections result)]
    (is (seq (:text result)))
    (is (= (get result :json) sections))
    (is (contains? sections :current))
    (is (contains? sections :recent))
    (is (contains? sections :enriched))
    (is (seq (focus-header/text-lines result)))
    (is (= [{:name "Alice" :type :person}
            {:name "Bob" :type :person}]
           (focus-header/current-entities result)))))
