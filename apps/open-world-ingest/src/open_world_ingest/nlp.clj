(ns open-world-ingest.nlp
  (:require [clojure.string :as str]
            [open-world-ingest.util :as util])
  (:import (edu.stanford.nlp.pipeline Annotation StanfordCoreNLP)
           (edu.stanford.nlp.ling CoreAnnotations$LemmaAnnotation
                                   CoreAnnotations$NamedEntityTagAnnotation
                                   CoreAnnotations$TextAnnotation
                                   CoreAnnotations$TokensAnnotation
                                   CoreLabel)
           (edu.stanford.nlp.naturalli OpenIEAnnotations$RelationTriplesAnnotation)
           (edu.stanford.nlp.trees TreeCoreAnnotations$TreeAnnotation Tree)
           (edu.stanford.nlp.util CoreMap)
           (edu.stanford.nlp.ie.util RelationTriple)
           (java.util Properties)))

(defonce ^:private pipeline
  (delay
    (let [props (doto (Properties.)
                  (.setProperty "annotators" "tokenize,ssplit,pos,lemma,ner,parse,depparse,natlog,openie")
                  (.setProperty "coref.algorithm" "neural"))]
      (StanfordCoreNLP. props))))

(def ^:private ner->kind
  {"PERSON" :person
   "ORGANIZATION" :org
   "LOCATION" :place
   "LOC" :place
   "CITY" :place
   "STATE_OR_PROVINCE" :place
   "COUNTRY" :place
   "NATIONALITY" :proper
   "TITLE" :proper})

(defn- ^String token-text [^CoreLabel token]
  (.get token CoreAnnotations$TextAnnotation))

(defn- ^String token-lemma [^CoreLabel token]
  (.get token CoreAnnotations$LemmaAnnotation))

(defn- ^String token-ner [^CoreLabel token]
  (.get token CoreAnnotations$NamedEntityTagAnnotation))

(defn- tree-span
  [^Tree tree]
  (when tree
    (let [leaves (.getLeaves tree)
          indices (->> leaves
                       (map (fn [^Tree leaf]
                              (when-let [label (.label leaf)]
                                (let [core (cast CoreLabel label)
                                      idx (.index core)]
                                  (when (pos? idx)
                                    idx)))))
                       (remove nil?))]
      (when (seq indices)
        (let [start (dec (apply min indices))
              end (apply max indices)]
          [start end])))))

(defn- collect-np-spans
  ([^Tree tree]
   (collect-np-spans tree false []))
  ([^Tree tree parent-np? acc]
   (if-not tree
     acc
     (let [label (some-> tree .label .value)
           current-np? (= label "NP")
           children (seq (.children tree))
           acc' (reduce (fn [coll child]
                          (collect-np-spans child current-np? coll))
                        acc
                        children)]
       (if (and current-np? (not parent-np?))
         (if-let [span (tree-span tree)]
           (conj acc' span)
           acc')
         acc')))))

(defn- entity-kind
  [tokens]
  (let [ner-kind (->> tokens
                      (map token-ner)
                      (remove #(or (nil? %) (= "O" %)))
                      (keep ner->kind)
                      first)]
    (or ner-kind
        (let [head (last tokens)
              lemma (some-> head token-lemma str/lower-case)
              normalized (when (seq lemma)
                           (-> lemma
                               (str/replace #"[^a-z0-9]+" "-")
                               (str/replace #"^-+" "")
                               (str/replace #"-+$" "")))]
          (if (str/blank? normalized)
            :proper
            (keyword normalized))))))

(defn- build-entity
  [label tokens sentence-idx span]
  (let [kind (entity-kind tokens)
        lower-label (str/lower-case label)
        entity-id (util/sha1 (str lower-label ":" (name kind)))]
    {:entity/id entity-id
     :entity/label label
     :entity/lower-label lower-label
     :entity/kind kind
     :entity/sentence sentence-idx
     :mention/span span}))

(defn- np-entities
  [^CoreMap sentence sentence-idx]
  (let [tokens (vec (.get sentence CoreAnnotations$TokensAnnotation))
        tree (.get sentence TreeCoreAnnotations$TreeAnnotation)
        spans (collect-np-spans tree)
        unique-spans (distinct spans)]
    (->> unique-spans
         (map (fn [[start end]]
                (let [span-tokens (subvec tokens start end)
                      text (->> span-tokens (map token-text) (str/join " ") str/trim)]
                  (when (seq text)
                    (build-entity text span-tokens sentence-idx [start end])))))
         (remove nil?))))

(defn- relation->map
  [^RelationTriple triple sentence-idx entities-by-label]
  (let [subject (.subjectGloss triple)
        rel (.relationLemmaGloss triple)
        object (.objectGloss triple)
        polarity (if (.isNegated triple) :negated :asserted)
        subj-id (:entity/id (get entities-by-label (str/lower-case subject)))
        obj-id (:entity/id (get entities-by-label (str/lower-case object)))]
    (when (and subj-id obj-id (seq rel))
      {:relation/subject subject
       :relation/object object
       :relation/src subj-id
       :relation/dst obj-id
       :relation/label (-> rel str/lower-case str/trim)
       :relation/polarity polarity
       :relation/confidence (.confidence triple)
       :relation/sentence sentence-idx})))

(defn analyze
  [text]
  (let [document (Annotation. text)
        _ (.annotate ^StanfordCoreNLP @pipeline document)
        sentences (.get document CoreAnnotations$SentencesAnnotation)]
    (loop [idx 0
           remaining (seq sentences)
           entities []
           relations []]
      (if-let [sentence (first remaining)]
        (let [np-ents (np-entities sentence idx)
              entities' (into entities np-ents)
              entities-by-label (into {} (map (fn [e]
                                                [(:entity/lower-label e) e])
                                              np-ents))
              triples (.get sentence OpenIEAnnotations$RelationTriplesAnnotation)
              rels (when triples
                     (keep #(relation->map % idx entities-by-label) triples))]
          (recur (inc idx)
                 (next remaining)
                 entities'
                 (into relations rels)))
        {:entities entities
         :relations relations})))
