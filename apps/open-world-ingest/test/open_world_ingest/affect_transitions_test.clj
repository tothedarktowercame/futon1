(ns open-world-ingest.affect-transitions-test
  (:require [clojure.test :refer [deftest is testing]]
            [nlp-interface.intent :as intent]
            [open-world-ingest.affect-transitions :as sut]
            [xtdb.api :as xt])
  (:import (java.time Instant)))

(def ^:private actor-id :me)
(def ^:private t1 (Instant/ofEpochMilli 1))
(def ^:private t2 (Instant/ofEpochMilli 2))

(defn- stub-xt-q
  [old-terms]
  (fn [& args]
    (case (count args)
      5 ;; fetch utterances
      [[::u1 "I feel happy today." t1]
       [::u2 "Biosemiotic sign and affordance." t2]]
      3 ;; mentions query
      [[::u2 :biosemiotic "biosemiotic" :concept]
       [::u2 :sign "sign" :concept]
       [::u2 :old-term "old-term" :concept]]
      6 ;; novelty check
      (let [[_ _ entity-id] args]
        (if (contains? old-terms entity-id)
          [[t1]]
          []))
      [])))

(defn- stub-intent [text]
  (if (re-find #"happy" text)
    {:type :positive-affect :conf 0.9}
    {:type :unknown :conf 0.1}))

(deftest affect-transitions-yield-capacity-terms
(with-redefs [xt/q (stub-xt-q #{:old-term})
                intent/analyze stub-intent]
    (let [result (sut/affect-transitions ::db {:since t1
                                               :until (.plusSeconds t2 600)
                                               :actor-id actor-id})
          transitions (:transitions result)
          transition (first transitions)]
      (is (= 1 (count transitions)))
      (is (= "positive-affect" (get-in transition [:affect_token :id])))
      (is (= "affect->consequence" (:direction transition)))
      (is (= 2 (count (:capacity_tokens transition))))
      (is (= #{"biosemiotic" "sign"}
             (set (map :label (:capacity_tokens transition)))))
      (is (= 2 (count (get-in transition [:certificate :term_mentions])))))))

(deftest affect-transitions-require-novel-terms
(with-redefs [xt/q (stub-xt-q #{:old-term})
                intent/analyze stub-intent]
    (let [result (sut/affect-transitions ::db {:since t1
                                               :until (.plusSeconds t2 600)
                                               :actor-id actor-id
                                               :max-terms 1})
          transition (first (:transitions result))]
      (is (= #{"biosemiotic"}
             (set (map :label (:capacity_tokens transition)))))))
(with-redefs [xt/q (stub-xt-q #{:biosemiotic :sign :old-term})
                intent/analyze stub-intent]
    (let [result (sut/affect-transitions ::db {:since t1
                                               :until (.plusSeconds t2 600)
                                               :actor-id actor-id
                                               :max-terms 1})
          transition (first (:transitions result))]
      (is (nil? transition)))))
