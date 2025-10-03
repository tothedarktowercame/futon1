(ns graph-memory.graph-memory-test
  (:require [clojure.test :refer [deftest is testing]]
            [graph-memory.graph-memory :as sut])) ; system under test

;; Fresh DB for each test (optional but nice)
(use-fixtures :each
  (fn [f]
    (alter-var-root #'g/conn (constantly (d/create-conn g/schema)))
    (f)))

(deftest seeding-adds-nodes
  (g/seed!)
  (is (pos? (count (d/q '[:find ?e :where [?e :node/id]] @g/conn)))))

(deftest neighbors-of-joe
  (g/seed!)
  (is (= #{[:project "Graph Memory Seed"]}
         (set (d/q '[:find ?to ?label
                     :where
                     [?e :node/id :joe]
                     [?e :edge/to ?to-e]
                     [?to-e :node/id ?to]
                     [?to-e :node/label ?label]]
                   @g/conn)))))

