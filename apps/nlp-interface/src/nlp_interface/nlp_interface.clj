(ns nlp-interface.nlp-interface
  (:require [clojure.string :as str]
            [graph-memory.main :as gm]
            [nlp-interface.intent :as intent]
            [nlp-interface.ner-er :as ner-er]
            [nlp-interface.ner-v4 :as ner-v4])
  (:import (java.time Instant ZoneId ZonedDateTime))
  (:gen-class))

(def gazetteer (delay (ner-er/load-gazetteer)))

(defn tokenize [text]
  (->> (re-seq #"\b\w+\b|'s|'re|'ve|'ll|'d|n't|[.,?!]" text)
        (map #(apply str %))
        vec))

(def ^:private entity-type->catalog-key
  {:person :people
   :place :places
   :org :orgs
   :project :projects
   :tool :tools})

(defn- entity-catalog
  [db]
  (let [entities (gm/entities-by-name db nil)]
    (->> entities
         (reduce (fn [{:keys [seen data] :as acc} ent]
                   (let [name (:entity/name ent)
                         type (:entity/type ent)
                         key (get entity-type->catalog-key type)
                         trimmed (some-> name str/trim)
                         normalized (some-> trimmed str/lower-case)]
                     (if (and key (seq trimmed) (not (str/blank? trimmed))
                              (not (contains? (get seen key #{}) normalized)))
                       {:seen (update seen key (fnil conj #{}) normalized)
                        :data (update data key (fnil conj [])
                                      {:label trimmed :layer :catalog :source :catalog})}
                       acc)))
                 {:seen {} :data {}})
         :data)))

(defn analyze
  ([text]
   (intent/analyze text))
  ([text tokens]
   (intent/analyze text {:tokens tokens})))

(defn pos-tag [tokens]
  (loop [idx 0
         acc []]
    (if-let [token (get tokens idx)]
      (let [next-token (get tokens (inc idx))
            tag (cond
                  (= "'s" token)
                  (if (and next-token (re-matches #".+ing" next-token))
                    "VBZ"  ; Verb, present tense singular
                    "POS")  ; Possessive marker

                  (re-matches #"[A-Z][a-z]+" token) "NNP"
                  (re-matches #"[0-9]+" token)      "CD"
                  (re-matches #".+ing" token)       "VBG"
                  :else                          "NN")]
        (recur (inc idx) (conj acc [token tag])))
      acc)))

(defn parse-tree [tagged]
  (into [:utterance]
        (map (fn [[token tag]] [tag token]) tagged)))

(defn- ensure-token-context [ctx]
  (if (:tokens ctx)
    ctx
    (assoc ctx :tokens (tokenize (:text ctx)))))

(defn- ensure-tag-context [ctx]
  (if (:pos ctx)
    ctx
    (let [ctx' (ensure-token-context ctx)
          tagged (pos-tag (:tokens ctx'))]
      (assoc ctx'
             :pos tagged
             :tags (mapv second tagged)))))

(def ^:private noun-tags
  #{"NN" "NNS" "NNP" "NNPS"})

(def ^:private verb-tags
  #{"VB" "VBD" "VBG" "VBN" "VBP" "VBZ"})

(def ^:private command-verbs
  #{"meet" "list" "count" "add" "remove" "link" "record" "schedule"
    "tell" "show" "help" "summarize" "summarise" "display" "explain"
    "remind" "plan" "organize" "organise" "draft"})

(def ^:private month-tokens
  #{"jan" "january" "feb" "february" "mar" "march" "apr" "april"
    "may" "jun" "june" "jul" "july" "aug" "august" "sep" "sept"
    "september" "oct" "october" "nov" "november" "dec" "december"})

(defn- temporal-token?
  [token tag]
  (let [lower (some-> token str/lower-case)]
    (or (= "CD" tag)
        (contains? month-tokens lower)
        (re-matches #"^(\d{1,2}|\d{4})$" (or token "")))))

(defn- chunk-kind
  [token tag idx]
  (let [lower (some-> token str/lower-case)]
    (cond
      (contains? command-verbs lower) :verb
      (contains? verb-tags tag) :verb
      (temporal-token? token tag) :temporal
      (contains? noun-tags tag) :noun
      (= "POS" tag) :other
      (and (= "'s" token) (> idx 0)) :other
      :else :other)))

(defn- start-chunk [kind idx token tag]
  {:kind kind
   :start idx
   :tokens [token]
   :tags [tag]})

(defn- extend-chunk [chunk token tag]
  (-> chunk
      (update :tokens conj token)
      (update :tags conj tag)))

(defn- commit-chunk [chunks chunk idx]
  (if (and chunk (seq (:tokens chunk)))
    (let [text (str/join " " (:tokens chunk))]
      (conj chunks (-> chunk
                       (assoc :end idx)
                       (assoc :text text))))
    chunks))

(defn- chunk-tokens
  [tagged]
  (loop [idx 0
         acc []
         chunk nil]
    (if (= idx (count tagged))
      (commit-chunk acc chunk idx)
      (let [[token tag] (nth tagged idx)
            kind (chunk-kind token tag idx)]
        (cond
          (= kind :other)
          (recur (inc idx) (commit-chunk acc chunk idx) nil)

          (nil? chunk)
          (recur (inc idx) acc (start-chunk kind idx token tag))

          (= kind (:kind chunk))
          (recur (inc idx) acc (extend-chunk chunk token tag))

          :else
          (recur (inc idx)
                 (commit-chunk acc chunk idx)
                 (start-chunk kind idx token tag)))))))

(defn- tokenize-stage [ctx _opts]
  (ensure-token-context ctx))

(defn- tag-stage [ctx _opts]
  (ensure-tag-context ctx))

(defn- chunk-stage [ctx _opts]
  (if (and (:chunks ctx) (:parse-tree ctx))
    ctx
    (let [ctx' (ensure-tag-context ctx)
          tagged (:pos ctx')
          parse (parse-tree tagged)
          chunks (chunk-tokens tagged)]
      (assoc ctx'
             :parse-tree parse
             :chunks chunks))))

(defn- intent-stage [ctx opts]
  (if (:intent ctx)
    ctx
    (let [ctx' (ensure-tag-context ctx)
          tokens (:tokens ctx')
          extras (merge {:tokens tokens}
                        (select-keys ctx' [:chunks :tags])
                        (:intent opts))
          intent-result (intent/analyze (:text ctx') extras)]
      (assoc ctx' :intent intent-result))))

(def default-stages [:tokenize :tag :chunk :intent])

(def ^:private stage->fn
  {:tokenize tokenize-stage
   :tag tag-stage
   :chunk chunk-stage
   :intent intent-stage})

(defn run-pipeline
  "Execute the deterministic NLP pipeline and return a map describing each stage.

  `stage-order` defaults to `default-stages`. Pass a subset (e.g. `[:tokenize :tag]`)
  to test or reuse intermediate artefacts without touching the whole pipeline.

  Extra opts are forwarded to individual stages (currently only `:intent`)."
  ([text]
   (run-pipeline text default-stages {}))
  ([text stage-order]
   (run-pipeline text stage-order {}))
  ([text stage-order opts]
   (let [stages (if (seq stage-order)
                  stage-order
                  default-stages)]
     (reduce (fn [ctx stage]
               (if-let [f (get stage->fn stage)]
                 (f ctx opts)
                 ctx))
             {:text text}
             stages))))

(defn handle-input [db text ts]
  (let [{:keys [tokens tags pos parse-tree chunks intent]}
        (run-pipeline text)
        intent-raw intent
        intent (some-> intent-raw (dissoc :intent-candidates))
        intent-candidates (:intent-candidates intent-raw)
        utt-node (gm/add-utterance! db text ts {:intent intent-raw})
        intent-node (gm/add-intent! db intent-raw)
        link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
        entities-raw (ner-er/ner tokens tags (force gazetteer))
        entities (mapv (fn [ent]
                         (let [entity (gm/ensure-entity! db ent)]
                           (gm/add-mention! db (:id utt-node) (:id entity) (:span ent))
                           (assoc ent :id (:id entity))))
                       entities-raw)
        entity-index (into {} (map (fn [{:keys [name id]}] [name id]) entities))
        relations-raw (ner-er/relations {:tokens tokens
                                         :entities entities})]
    (doseq [{:keys [type src dst prov]} relations-raw]
      (when (and src dst)
        (when-let [src-id (get entity-index src)]
          (when-let [dst-id (get entity-index dst)]
            (gm/add-relation! db {:type type
                                  :src-id src-id
                                  :dst-id dst-id
                                  :prov prov})))))
    {:utterance utt-node
     :intent intent
     :intent-candidates intent-candidates
     :tokens tokens
     :pos pos
     :tags tags
     :parse-tree parse-tree
     :chunks chunks
     :entities (mapv (fn [{:keys [name type span]}]
                       {:name name :type type :span span})
                     entities)
     :relations (mapv #(select-keys % [:type :src :dst]) relations-raw)
     :links [link]}))

(defn- now-from-ts [ts]
  (ZonedDateTime/ofInstant (Instant/ofEpochMilli ts) (ZoneId/systemDefault)))

(defn handle-input-v4
  "Return enriched NLP data using the tiered v4 NER pipeline."
  ([db text ts]
   (handle-input-v4 db text ts {}))
  ([db text ts opts]
   (let [{:keys [tokens pos intent tags chunks]}
         (run-pipeline text)
         intent-raw intent
         intent (some-> intent-raw (dissoc :intent-candidates))
         intent-candidates (:intent-candidates intent-raw)
         utt-node (gm/add-utterance! db text ts {:intent intent-raw})
         intent-node (gm/add-intent! db intent-raw)
         link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
         tagged pos
         now (now-from-ts ts)
         catalog (entity-catalog db)
         ner-opts (assoc opts :catalog catalog)
         entities (ner-v4/recognize-entities tokens tagged text now ner-opts)
         ;; alias resolution
         entities (mapv (fn [ent]
                          (if (and (:label ent) (.equalsIgnoreCase "i" (:label ent)))
                            (assoc ent :label "Me" :type :person)
                            ent))
                        entities)
         stored (mapv (fn [ent]
                        (let [entity (gm/ensure-entity! db {:name (:label ent)
                                                           :type (:type ent)})]
                          (gm/add-mention! db (:id utt-node) (:id entity) (:span ent))
                          (assoc ent
                                 :name (:label ent)
                                 :entity-id (:id entity)
                                 :entity-db (:db/eid entity))))
                      entities)
         ordered (sort-by (comp :start :span) stored)
         relations (->> (partition 2 1 ordered)
                        (map (fn [[a b]]
                               (let [between (subs text (:end (:span a)) (:start (:span b)))
                                     lower (str/lower-case between)
                                     src (:name a)
                                     dst (:name b)]
                                 (cond
                                   (and (= (:type a) :person) (= (:type b) :place)
                                        (re-find #"\b(in|at|from|to)\b" lower))
                                   {:type :located-in :src src :dst dst}

                                   (and (= (:type b) :date)
                                        (re-find #"\b(on|by|before|after)\b" lower))
                                   {:type :scheduled :src src :dst dst}

                                   (re-find #"\bwith\b" lower)
                                   {:type :with :src src :dst dst}

                                   :else
                                   {:type :links-to :src src :dst dst}))))
                        (remove nil?)
                        vec)]
     {:utterance utt-node
      :intent intent
      :intent-candidates intent-candidates
      :tokens tokens
      :pos tagged
      :tags tags
      :chunks chunks
      :entities stored
      :relations relations
      :links [link]})))

(defn -main
  "I don't do a whole lot ... yet."
  [& _args]
  (println "nlp-interface ready"))
