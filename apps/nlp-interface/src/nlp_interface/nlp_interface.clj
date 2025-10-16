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

(defn handle-input [db text ts]
  (let [tokens (tokenize text)
        intent-raw (intent/analyze text {:tokens tokens})
        intent (dissoc intent-raw :intent-candidates)
        intent-candidates (:intent-candidates intent-raw)
        utt-node (gm/add-utterance! db text ts {:intent intent-raw})
        intent-node (gm/add-intent! db intent-raw)
        link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
        tagged (pos-tag tokens)
        tags (mapv second tagged)
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
     :pos tagged
     :parse-tree (parse-tree tagged)
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
   (let [tokens (tokenize text)
         intent-raw (intent/analyze text {:tokens tokens})
         intent (dissoc intent-raw :intent-candidates)
         intent-candidates (:intent-candidates intent-raw)
         utt-node (gm/add-utterance! db text ts {:intent intent-raw})
         intent-node (gm/add-intent! db intent-raw)
         link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
         tagged (pos-tag tokens)
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
      :entities stored
      :relations relations
      :links [link]})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "nlp-interface ready"))
