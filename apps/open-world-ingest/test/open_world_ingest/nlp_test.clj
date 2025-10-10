(ns open-world-ingest.nlp-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [open-world-ingest.nlp])
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
