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
      (is (= ["Minneapolis" "12 Oct" "XTDB" "Isabella von Holstein"]
             (map :label entities)))
      (is (= "2024-10-12"
             (:value (some #(when (= (:type %) :date) %) entities)))))
    (testing "fallback adds titlecase singleton"
      (is (= ["Tom" "Minneapolis" "12 Oct" "XTDB" "Isabella von Holstein"]
             (map :label fallback)))
      (is (= :unknown (:type (first fallback))))
      (is (= :fallback (:source (first fallback)))))))
