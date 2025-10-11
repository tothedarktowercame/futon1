(ns nlp-interface.nlp-interface-test
  (:require [clojure.test :refer [deftest is testing]]
            [nlp-interface.nlp-interface :as sut]
            [nlp-interface.ner-v4 :as ner-v4])
  (:import (java.time ZoneId ZonedDateTime)))

(def sample-text "Meet Tom in Minneapolis on 12 Oct about XTDB v5 with Isabella.")
(def sample-now (ZonedDateTime/of 2024 3 1 12 0 0 0 (ZoneId/of "UTC")))

(deftest recognize-entities-v4
  (let [tokens (sut/tokenize sample-text)
        pos-tags (sut/pos-tag tokens)
        entities (ner-v4/recognize-entities tokens pos-tags sample-text sample-now)
        fallback (ner-v4/recognize-entities tokens pos-tags sample-text sample-now
                                             {:enable-fallback? true})]
    (testing "baseline recognition without fallback"
      (is (= ["Tom" "Minneapolis" "12 Oct" "XTDB" "Isabella von Holstein"]
             (map :label entities)))
      (is (= :person (:type (first entities))))
      (is (= :pos (:source (first entities))))
      (is (= "2024-10-12"
             (:value (some #(when (= (:type %) :date) %) entities)))))
    (testing "fallback matches baseline heuristics"
      (is (= (map :label entities)
             (map :label fallback)))
      (is (= :person (:type (first fallback))))
      (is (= :pos (:source (first fallback)))))))

(deftest question-words-are-ignored
  (let [text "How are things in Princeton?"
        tokens (sut/tokenize text)
        pos-tags (sut/pos-tag tokens)
        entities (ner-v4/recognize-entities tokens pos-tags text sample-now)]
    (is (= ["Princeton"]
           (map :label entities)))
    (is (= :place (:type (first entities))))
    (is (empty? (filter #(= "How" (:label %)) entities)))))

(deftest analyze-intent-from-dictionary
  (testing "greets surface high confidence"
    (let [{:keys [type conf]} (sut/analyze "Hello there!")]
      (is (= :greet type))
      (is (> conf 0.95))))
  (testing "food-centric text maps to orality"
    (let [{:keys [type conf]} (sut/analyze "I cooked dinner with garlic and wine.")]
      (is (= :primary-need-orality type))
      (is (>= conf 0.74))))
  (testing "sad sentiments outweigh body references"
    (let [{:keys [type conf]} (sut/analyze "My heart feels sad and lonely today.")]
      (is (= :sadness type))
      (is (>= conf 0.7))))
  (testing "travel wording selects voyage"
    (let [{:keys [type conf]} (sut/analyze "We will sail across the ocean and wander new shores.")]
      (is (= :voyage type))
      (is (>= conf 0.68))))
  (testing "explicit help requests are prioritised"
    (let [{:keys [type conf]} (sut/analyze "Can you help me understand this report?")]
      (is (= :help-request type))
      (is (>= conf 0.7)))))
