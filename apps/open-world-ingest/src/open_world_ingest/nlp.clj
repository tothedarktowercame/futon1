(ns open-world-ingest.nlp
  (:require [clojure.string :as str]
            [open-world-ingest.util :as util])
  (:import (edu.stanford.nlp.pipeline Annotation StanfordCoreNLP)
           (edu.stanford.nlp.ling CoreAnnotations$LemmaAnnotation
                                   CoreAnnotations$PartOfSpeechAnnotation
                                   CoreAnnotations$NamedEntityTagAnnotation
                                   CoreAnnotations$TextAnnotation
                                   CoreAnnotations$TokensAnnotation
                                   CoreAnnotations$SentencesAnnotation
                                   CoreLabel)
           (edu.stanford.nlp.trees TreeCoreAnnotations$TreeAnnotation Tree)
           (edu.stanford.nlp.ie.util RelationTriple)
           (java.time Instant ZoneId)
           (java.time.format DateTimeFormatter)
           (java.util Properties)))

(defonce ^:private pipeline
  (delay
    (let [props (doto (Properties.)
                  (.setProperty "annotators" "tokenize,ssplit,pos,lemma,ner,parse,depparse,natlog,openie")
                  (.setProperty "coref.algorithm" "neural"))]
      (StanfordCoreNLP. props))))

(def ^:private relation-triples-annotation
  (delay
    (try
      (Class/forName "edu.stanford.nlp.naturalli.OpenIEAnnotations$RelationTriplesAnnotation")
      (catch ClassNotFoundException _
        nil))))

(def ^:private ner->kind
  {"PERSON" :person
   "ORGANIZATION" :org
   "LOCATION" :place
   "LOC" :place
   "CITY" :place
   "STATE_OR_PROVINCE" :place
   "COUNTRY" :place
   "DATE" :date
   "NATIONALITY" :proper
   "TITLE" :proper})

(def ^:private iso-formatter DateTimeFormatter/ISO_LOCAL_DATE)

(def ^:private ego-entity-id :open-world-ingest.nlp/ego)

(defn- ego-entity
  [sentence-idx]
  {:entity/id ego-entity-id
   :entity/label "Me"
   :entity/lower-label "me"
   :entity/kind :person
   :entity/sentence sentence-idx
   :mention/span nil})

(defn- ^String token-text [^CoreLabel token]
  (.get token CoreAnnotations$TextAnnotation))

(defn- ^String token-lemma [^CoreLabel token]
  (.get token CoreAnnotations$LemmaAnnotation))

(defn- ^String token-pos [^CoreLabel token]
  (.get token CoreAnnotations$PartOfSpeechAnnotation))

(defn- ^String token-ner [^CoreLabel token]
  (.get token CoreAnnotations$NamedEntityTagAnnotation))

(defn- proper-noun?
  [pos]
  (boolean (some #{"NNP" "NNPS"} [pos])))

(defn- extend-ner-span
  [tokens start kind]
  (loop [end (inc start)]
    (if (>= end (count tokens))
      end
      (let [next-token (nth tokens end)
            next-kind (get ner->kind (token-ner next-token))
            next-pos (token-pos next-token)
            bridge? (and (nil? next-kind)
                         (proper-noun? next-pos))]
        (if (or (= kind next-kind)
                bridge?)
          (recur (inc end))
          end)))))

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

(defn- normalize-date
  ([s]
   (normalize-date s (Instant/now)))
  ([s ^Instant now]
   (when (seq s)
     (let [trimmed (-> s str str/trim str/lower-case)
           zone (ZoneId/systemDefault)
           today (.toLocalDate (.atZone now zone))]
       (cond
         (contains? #{"today" "tonight"} trimmed) (.format iso-formatter today)
         (= "tomorrow" trimmed) (.format iso-formatter (.plusDays today 1))
         (= "yesterday" trimmed) (.format iso-formatter (.minusDays today 1))
         (re-matches #"\d{4}-\d{2}-\d{2}" trimmed) trimmed
         :else nil)))))

(defn- annotate-entity
  [entity now]
  (if (and (= :date (:entity/kind entity))
           (not (:entity/time entity)))
    (if-let [iso (normalize-date (:entity/label entity) now)]
      (assoc entity :entity/time iso)
      entity)
    entity))

(defn- normalize-predicate
  [lemma]
  (let [p (-> (or lemma "") str/lower-case str/trim)]
    (cond
      (re-matches #"(be|am|is|are|was|were)\s+in" p) "be in"
      (re-matches #"go(\s+to)?" p) "go to"
      (re-matches #"meet(\s+with)?" p) "meet"
      :else p)))

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

(defn- dedupe-entities
  [entities]
  (reduce (fn [acc entity]
            (if (some #(= (:entity/id entity) (:entity/id %)) acc)
              acc
              (conj acc entity)))
          []
          entities))

(defn- ner-entities
  [tokens sentence-idx]
  (loop [idx 0
         acc []]
    (if (>= idx (count tokens))
      acc
      (let [token (nth tokens idx)
            ner (token-ner token)
            kind (get ner->kind ner)]
        (if (nil? kind)
          (recur (inc idx) acc)
          (let [end (extend-ner-span tokens idx kind)
                span-tokens (subvec tokens idx end)
                text (->> span-tokens (map token-text) (str/join " ") str/trim)
                acc' (if (seq text)
                       (conj acc (build-entity text span-tokens sentence-idx [idx end]))
                       acc)]
            (recur end acc')))))))

(defn- np-entities
  [tokens tree sentence-idx]
  (let [spans (collect-np-spans tree)
        unique-spans (distinct spans)]
    (->> unique-spans
         (map (fn [[start end]]
                (let [span-tokens (subvec tokens start end)
                      text (->> span-tokens (map token-text) (str/join " ") str/trim)]
                  (when (seq text)
                    (build-entity text span-tokens sentence-idx [start end])))))
         (remove nil?))))

(defn- relation->map
  [^RelationTriple triple sentence-idx entities-by-label now]
  (let [subject (.subjectGloss triple)
        rel (.relationLemmaGloss triple)
        object (.objectGloss triple)
        polarity (if (.isNegated triple) :negated :asserted)
        subj-entity (get entities-by-label (str/lower-case subject))
        obj-entity (get entities-by-label (str/lower-case object))
        subj-id (:entity/id subj-entity)
        obj-id (:entity/id obj-entity)
        rel-label (normalize-predicate rel)
        rel-gloss (.relationGloss triple)
        time-val (or (:entity/time obj-entity)
                     (normalize-date object now))
        loc? (when rel-gloss
               (re-find #"(?i)\b(at|in|on)\b" rel-gloss))]
    (when (and subj-id obj-id (seq rel))
      (cond-> {:relation/subject subject
               :relation/object object
               :relation/src subj-id
               :relation/dst obj-id
               :relation/label rel-label
               :relation/polarity polarity
               :relation/confidence (.confidence triple)
               :relation/sentence sentence-idx}
        time-val (assoc :relation/time time-val)
        (and loc? obj-id (#{:place :org} (:entity/kind obj-entity))) (assoc :relation/loc obj-id)))))

(defn- process-sentences
  [sentences now]
  (loop [idx 0
         remaining (seq sentences)
         entities []
         relations []
         ego-present? false]
    (if-let [sentence (first remaining)]
      (let [tokens (vec (.get sentence CoreAnnotations$TokensAnnotation))
            tree (.get sentence TreeCoreAnnotations$TreeAnnotation)
            sentence-entities (->> (concat (np-entities tokens tree idx)
                                           (ner-entities tokens idx))
                                   dedupe-entities
                                   (map #(annotate-entity % now)))
            ego (ego-entity idx)
            pronoun-map {"i" ego "me" ego "my" ego "mine" ego "myself" ego}
            entities-by-label (into pronoun-map
                                    (map (fn [e]
                                           [(:entity/lower-label e) e])
                                         sentence-entities))
            annotation @relation-triples-annotation
            triples (when annotation
                      (.get sentence annotation))
            rels (when triples
                   (keep #(relation->map % idx entities-by-label now) triples))
            ego-used? (or (some #(= (:relation/src %) ego-entity-id) rels)
                          (some #(= (:relation/dst %) ego-entity-id) rels))
            entities' (cond-> (into entities sentence-entities)
                         (and ego-used? (not ego-present?)) (conj ego))
            ego-present?' (or ego-present? ego-used?)]
        (recur (inc idx)
               (next remaining)
               entities'
               (into relations rels)
               ego-present?'))
      {:entities entities
       :relations relations})))

(defn analyze
  ([text]
   (analyze text {:now (Instant/now)}))
  ([text {:keys [now] :or {now (Instant/now)}}]
   (let [document (Annotation. text)]
     (.annotate ^StanfordCoreNLP @pipeline document)
     (let [sentences (.get document CoreAnnotations$SentencesAnnotation)]
       (process-sentences sentences now)))))
