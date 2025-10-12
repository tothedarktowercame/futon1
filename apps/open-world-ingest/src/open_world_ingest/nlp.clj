(ns open-world-ingest.nlp
  (:require [clojure.string :as str]
            [open-world-ingest.trace :as trace]
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

(defn ^:private normalize-date
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

(defn ^:private annotate-entity
  [entity now]
  (if (and (= :date (:entity/kind entity))
           (not (:entity/time entity)))
    (if-let [iso (normalize-date (:entity/label entity) now)]
      (assoc entity :entity/time iso)
      entity)
    entity))

(def ^:private fallback-relation :links-to)

(defn ^:private normalize-predicate
  [lemma]
  (let [p (-> (or lemma "") str/lower-case str/trim)]
    (when (seq p)
      (let [normalized (cond
                         (re-matches #"(be|am|is|are|was|were)\s+in" p) "be in"
                         (re-matches #"go(\s+to)?" p) "go to"
                         (re-matches #"meet(\s+with)?" p) "meet"
                         :else p)]
        (when (seq normalized)
          normalized)))))

(defn- sanitize-predicate
  [predicate]
  (when-let [normalized (normalize-predicate predicate)]
    (let [preserved (str/replace normalized #"\s+/\s+" "/")
          cleaned (-> preserved
                      (str/replace #"[^a-z0-9/\s-]" " ")
                      (str/replace #"\s+" "-")
                      (str/replace #"-+" "-")
                      (str/replace #"-/-" "/")
                      (str/replace #"(^-|-$)" ""))]
      (when (seq cleaned)
        cleaned))))

(def ^:private namespace-stopwords
  #{"" "a" "an" "and" "dear" "his" "her" "their" "our" "your"
    "my" "the" "this" "that" "these" "those" "its" "of" "to" "for"
    "on" "in" "with" "at" "by" "me" "you" "we" "they"})

(defn- sanitize-label
  [value]
  (when-let [text (some-> value str str/lower-case)]
    (let [clean (-> text
                    (str/replace #"[^a-z0-9]+" "-")
                    (str/replace #"-+" "-")
                    (str/replace #"(^-|-$)" ""))]
      (when (seq clean)
        clean))))

(defn- candidate-namespace
  [object-text]
  (when-let [sanitized (sanitize-label object-text)]
    (let [parts (str/split sanitized #"-")
          candidate (some (fn [token]
                             (let [token' (str/replace token #"^[^a-z0-9]+|[^a-z0-9]+$" "")]
                               (when (and (seq token')
                                          (not (namespace-stopwords token')))
                                 token')))
                           (reverse parts))]
      candidate)))

(defn- derive-relation-type
  [lemma gloss object-text]
  (let [candidates (->> [gloss lemma]
                        (map sanitize-predicate)
                        (remove nil?)
                        distinct)
        primary (first candidates)
        base-keyword (some-> primary keyword)
        aliases (->> (rest candidates)
                     (map keyword)
                     distinct
                     vec)
        ns-label (candidate-namespace object-text)
        final-label (cond
                      (and ns-label (seq primary)) (keyword (str ns-label "/" primary))
                      base-keyword base-keyword
                      :else nil)
        aliases' (cond-> aliases
                   (and base-keyword final-label (not= final-label base-keyword))
                   (into [base-keyword]))]
    (when final-label
      {:label final-label
       :aliases (vec (distinct aliases'))})))

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

(defn ^:private dedupe-entities
  [entities]
  (reduce (fn [acc entity]
            (if (some #(= (:entity/id entity) (:entity/id %)) acc)
              acc
              (conj acc entity)))
          []
          entities))

(defn ^:private ner-entities
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

(defn- maybe-invoke
  [obj method]
  (when (and obj method)
    (try
      (clojure.lang.Reflector/invokeInstanceMethod obj method (object-array 0))
      (catch Exception _
        nil))))

(defn- span->vec
  [span]
  (when span
    (let [begin (or (some-> (maybe-invoke span "getBegin") int)
                    (some-> (maybe-invoke span "begin") int)
                    (some-> (maybe-invoke span "getStart") int)
                    (some-> (maybe-invoke span "first") int)
                    (some-> (maybe-invoke span "getFirst") int))
          end (or (some-> (maybe-invoke span "getEnd") int)
                  (some-> (maybe-invoke span "end") int)
                  (some-> (maybe-invoke span "getStop") int)
                  (some-> (maybe-invoke span "second") int)
                  (some-> (maybe-invoke span "getSecond") int))]
      (when (and (number? begin) (number? end))
        [begin end]))))

(defn- relation-triple->record
  [^RelationTriple triple sentence sentence-idx]
  (let [subject (.subjectGloss triple)
        subject-lemma (.subjectLemmaGloss triple)
        object (.objectGloss triple)
        object-lemma (.objectLemmaGloss triple)
        relation-gloss (.relationGloss triple)
        relation-lemma (.relationLemmaGloss triple)
        negated? (.isNegated triple)
        confidence (.confidence triple)
        subj-span (span->vec (maybe-invoke triple "subjectTokenSpan"))
        obj-span (span->vec (maybe-invoke triple "objectTokenSpan"))
        rel-span (span->vec (maybe-invoke triple "relationTokenSpan"))]
    {:sent sentence
     :sent-idx sentence-idx
     :subj subject
     :subj-lemma subject-lemma
     :obj object
     :obj-lemma object-lemma
     :pred relation-gloss
     :lemma relation-lemma
     :confidence confidence
     :negated? negated?
     :spans {:subj subj-span
             :obj obj-span
             :pred rel-span}}))

(defn ^:private record->normalized
  [{:keys [sent sent-idx subj subj-lemma obj obj-lemma pred lemma confidence negated? spans]}]
  {:sentence sent
   :sentence-idx sent-idx
   :subject {:text subj
             :lemma (or subj-lemma subj)
             :span (:subj spans)}
   :object {:text obj
            :lemma (or obj-lemma obj)
            :span (:obj spans)}
   :relation {:text pred
              :lemma (or lemma pred)
              :span (:pred spans)}
   :confidence confidence
   :negated? (boolean negated?)})

(defn- prepare-replay
  [replay]
  (let [records (trace/load-triples replay)]
    (when (seq records)
      (trace/index-by-sentence records))))

(defn ^:private lookup-replay
  [indexed sentence-idx sentence-text]
  (if (seq indexed)
    (let [candidates [[sentence-idx sentence-text]
                      [sentence-idx :any]
                      [:any sentence-text]
                      [:any :any]]]
      (loop [[k & ks] candidates]
        (cond
          (nil? k) ::missing
          (contains? indexed k) (get indexed k)
          :else (recur ks))))
    ::missing))

(defn ^:private relation->map
  [{:keys [sentence-idx subject object relation confidence negated?] :as triple}
   entities-by-label now]
  (let [subject-text (:text subject)
        object-text (:text object)
        lemma (:lemma relation)
        relation-text (:text relation)
        polarity (if negated? :negated :asserted)
        subj-entity (some-> subject-text str/lower-case entities-by-label)
        obj-entity (some-> object-text str/lower-case entities-by-label)
        subj-id (:entity/id subj-entity)
        obj-id (:entity/id obj-entity)
        {:keys [label aliases]} (derive-relation-type lemma relation-text object-text)
        rel-label (or label fallback-relation)
        aliases' (when label
                   (->> aliases
                        (remove #(= % rel-label))
                        vec))
        time-val (or (:entity/time obj-entity)
                     (normalize-date object-text now))
        loc? (when relation-text
               (re-find #"(?i)\b(at|in|on)\b" relation-text))]
    (when (and subj-id obj-id (or (seq lemma) (seq relation-text)))
      (cond-> {:relation/subject subject-text
               :relation/object object-text
               :relation/src subj-id
               :relation/dst obj-id
               :relation/label rel-label
               :relation/polarity polarity
               :relation/confidence confidence
               :relation/sentence sentence-idx}
        (and (seq aliases') (not= rel-label fallback-relation)) (assoc :relation/type-aliases aliases')
        time-val (assoc :relation/time time-val)
        (and loc? obj-id (#{:place :org} (:entity/kind obj-entity))) (assoc :relation/loc obj-id)))))

(defn- process-sentences
  [sentences now {:keys [trace replay]}]
  (let [trace-config (trace/config trace)
        replay-index (prepare-replay replay)]
    (loop [idx 0
           remaining (seq sentences)
           entities []
           relations []
           ego-present? false]
      (if-let [sentence (first remaining)]
        (let [tokens (vec (.get sentence CoreAnnotations$TokensAnnotation))
              tree (.get sentence TreeCoreAnnotations$TreeAnnotation)
              sentence-text (.get sentence CoreAnnotations$TextAnnotation)
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
              base-records (when triples
                             (mapv #(relation-triple->record % sentence-text idx) triples))
              replay-records (lookup-replay replay-index idx sentence-text)
              effective-records (cond
                                  (identical? ::missing replay-records) base-records
                                  :else replay-records)
              normalized-records (map record->normalized effective-records)
              rels (keep #(relation->map % entities-by-label now) normalized-records)
              _ (when (and trace-config (seq base-records))
                  (trace/append! trace-config base-records))
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
         :relations relations}))))

(defn analyze
  ([text]
   (analyze text {:now (Instant/now)}))
  ([text {:keys [now trace-openie replay-openie]
          :or {now (Instant/now)}}]
   (let [document (Annotation. text)]
     (.annotate ^StanfordCoreNLP @pipeline document)
     (let [sentences (.get document CoreAnnotations$SentencesAnnotation)]
       (process-sentences sentences
                          now
                          {:trace trace-openie
                           :replay replay-openie})))))
