(ns open-world-ingest.nlp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [open-world-ingest.nlp :as nlp]
            [open-world-ingest.trace :as trace])
  (:import (edu.stanford.nlp.ling CoreLabel)
           (edu.stanford.nlp.ling CoreAnnotations$LemmaAnnotation
                                  CoreAnnotations$NamedEntityTagAnnotation
                                  CoreAnnotations$PartOfSpeechAnnotation
                                  CoreAnnotations$TextAnnotation)
           (java.time Instant)))

(defn- proper-pos
  [text]
  (if (and (seq text)
           (Character/isUpperCase (int (first text))))
    "NNP"
    "NN"))

(defn- token
  ([text]
   (token text "O" (proper-pos text)))
  ([text ner]
   (token text ner (proper-pos text)))
  ([text ner pos]
   (let [lemma (str/lower-case text)]
     (doto (CoreLabel.)
       (.set CoreAnnotations$TextAnnotation text)
       (.set CoreAnnotations$LemmaAnnotation lemma)
       (.set CoreAnnotations$NamedEntityTagAnnotation ner)
       (.set CoreAnnotations$PartOfSpeechAnnotation pos)))))

(deftest ner-entities-capture-contiguous-ner-spans
  (let [tokens [(token "Cassie" "PERSON")
                (token "Lee" "PERSON")
                (token "at" "O" "IN")
                (token "the" "O" "DT")
                (token "MIT" "ORGANIZATION")
                (token "Media" "O" "NNP")
                (token "Lab" "ORGANIZATION")
                (token ",")
                (token "and")
                (token "hopefully")
                (token "also")
                (token "Eric" "PERSON")
                (token "White" "PERSON")]
        entities (#'open-world-ingest.nlp/ner-entities tokens 0)]
    (is (= ["Cassie Lee" "MIT Media Lab" "Eric White"]
           (map :entity/label entities)))
    (is (= [:person :org :person]
           (map :entity/kind entities)))
    (is (= [[0 2] [4 7] [11 13]]
           (map :mention/span entities)))))

(deftest dedupe-entities-prefers-first-occurrence
  (let [entities [{:entity/id "person:alice" :entity/label "Alice"}
                  {:entity/id "person:alice" :entity/label "Alice Smith"}
                  {:entity/id "org:mit" :entity/label "MIT"}]
        deduped (#'open-world-ingest.nlp/dedupe-entities entities)]
    (is (= ["Alice" "MIT"]
           (map :entity/label deduped)))))

(deftest normalize-date-converts-relative-days
  (let [now (Instant/parse "2025-10-09T12:00:00Z")]
    (is (= "2025-10-09" (#'open-world-ingest.nlp/normalize-date "today" now)))
    (is (= "2025-10-10" (#'open-world-ingest.nlp/normalize-date "tomorrow" now)))
    (is (= "2025-10-08" (#'open-world-ingest.nlp/normalize-date "yesterday" now)))
    (is (= "2025-10-12" (#'open-world-ingest.nlp/normalize-date "2025-10-12" now)))
    (is (nil? (#'open-world-ingest.nlp/normalize-date "next week" now)))))

(deftest annotate-entity-populates-date-value
  (let [entity {:entity/id :date
                :entity/label "Tomorrow"
                :entity/lower-label "tomorrow"
                :entity/kind :date
                :entity/sentence 0
                :mention/span [0 1]}
        now (Instant/parse "2025-10-09T12:00:00Z")
        annotated (#'open-world-ingest.nlp/annotate-entity entity now)]
    (is (= "2025-10-10" (:entity/time annotated)))))

(deftest normalize-predicate-applies-domain-rules
  (is (= "be in" (#'open-world-ingest.nlp/normalize-predicate "Is In")))
  (is (= "go to" (#'open-world-ingest.nlp/normalize-predicate "go")))
  (is (= "meet" (#'open-world-ingest.nlp/normalize-predicate "meet with")))
  (is (= "something else" (#'open-world-ingest.nlp/normalize-predicate "something else"))))

(deftest relation-map-from-record
  (let [record {:sent "Alice works at Acme Corp."
                :sent-idx 0
                :subj "Alice"
                :subj-lemma "alice"
                :obj "Acme Corp"
                :obj-lemma "acme corp"
                :pred "works at"
                :lemma "work at"
                :confidence 0.92
                :negated? false
                :spans {:subj [0 1]
                        :pred [1 3]
                        :obj [3 5]}}
        normalized (#'open-world-ingest.nlp/record->normalized record)
        now (Instant/parse "2025-01-01T00:00:00Z")
        entities {"alice" {:entity/id :alice :entity/kind :person}
                  "acme corp" {:entity/id :acme :entity/kind :org}}
        rel (#'open-world-ingest.nlp/relation->map normalized entities now)]
    (is (= :alice (:relation/src rel)))
    (is (= :acme (:relation/dst rel)))
    (is (= :works-at (:relation/label rel)))
    (is (= [:work-at] (:relation/type-aliases rel)))
    (is (= :asserted (:relation/polarity rel)))
    (is (= 0 (:relation/sentence rel)))
    (is (= :acme (:relation/loc rel)))
    (is (= 0.92 (:relation/confidence rel)))))

(deftest relation-map-falls-back-when-predicate-empty
  (let [record {:sent "Alice ??? Bob"
                :sent-idx 0
                :subj "Alice"
                :subj-lemma "alice"
                :obj "Bob"
                :obj-lemma "bob"
                :pred "???"
                :lemma "???"
                :confidence 0.5
                :negated? false
                :spans {:subj [0 1]
                        :pred [1 2]
                        :obj [2 3]}}
        normalized (#'open-world-ingest.nlp/record->normalized record)
        entities {"alice" {:entity/id :alice :entity/kind :person}
                  "bob" {:entity/id :bob :entity/kind :person}}
        rel (#'open-world-ingest.nlp/relation->map normalized entities (Instant/parse "2025-01-01T00:00:00Z"))]
    (is (= :links-to (:relation/label rel)))
    (is (nil? (:relation/type-aliases rel)))))

(deftest lookup-replay-prefers-specific-matches
  (let [records [{:sent-idx 0 :sent "Alpha" :subj "A" :pred "p" :obj "B" :lemma "p" :spans {}}
                 {:sent-idx 1 :sent "Beta" :subj "C" :pred "p" :obj "D" :lemma "p" :spans {}}]
        index (trace/index-by-sentence records)]
    (testing "exact sentence match"
      (is (= [(first records)]
             (#'open-world-ingest.nlp/lookup-replay index 0 "Alpha"))))
    (testing "fallback to global list when missing"
      (is (= records
             (#'open-world-ingest.nlp/lookup-replay index 5 "Unknown"))))
    (testing "missing index returns sentinel"
      (is (= ::nlp/missing
             (let [lookup #'open-world-ingest.nlp/lookup-replay]
               (lookup {} 0 "Empty")))))))
