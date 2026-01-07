(ns open-world-ingest.affect-candidates-test
  (:require [clojure.test :refer [deftest is testing]]
            [nlp-interface.intent :as intent]
            [open-world-ingest.affect-candidates :as sut]
            [xtdb.api :as xt])
  (:import (java.time Instant)))

(def ^:private actor-id :me)
(def ^:private t1 (Instant/ofEpochMilli 1))
(def ^:private t2 (Instant/ofEpochMilli 2))

(defn- stub-xt-q
  [old-terms]
  (fn [& args]
    (case (count args)
      6 (let [[_ _ entity-id] args]
          (if (contains? old-terms entity-id)
            [[t1]]
            []))
      [])))

(defn- stub-intent [text]
  (if (re-find #"happy" text)
    {:type :joy :conf 0.9}
    {:type :unknown :conf 0.1}))

(deftest candidates-track-affect-and-consequence
  (with-redefs [xt/db (constantly ::db)
                xt/q (stub-xt-q #{})
                intent/analyze stub-intent]
    (sut/configure! {:lookahead-minutes 10
                     :novelty-days 30
                     :max-pending 10})
    (reset! @#'open-world-ingest.affect-candidates/!pending {})
    (testing "affect utterance creates a pending candidate"
      (sut/record-utterance! {:node ::node
                              :text "I am happy."
                              :ts t1
                              :utterance-id ::u1
                              :actor-id actor-id
                              :entities []})
      (let [pending (get (sut/pending-state) actor-id)]
        (is (= 1 (count pending)))
        (is (empty? (:capacity-tokens (first pending))))))
    (testing "novel terms attach to pending candidate"
      (sut/record-utterance! {:node ::node
                              :text "Biosemiotic sign."
                              :ts t2
                              :utterance-id ::u2
                              :actor-id actor-id
                              :entities [{:entity/id :biosemiotic
                                          :entity/label "biosemiotic"
                                          :entity/kind :concept}]})
      (let [pending (get (sut/pending-state) actor-id)
            tokens (-> pending first :capacity-tokens)]
        (is (= #{"biosemiotic"} (set (map :label (vals tokens)))))))))
