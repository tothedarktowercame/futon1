(ns nlp-interface.nlp-interface
  (:require [clojure.string :as str]
            [graph-memory.main :as gm]
            [nlp-interface.ner-er :as ner-er]
            [nlp-interface.ner-v4 :as ner-v4])
  (:import (java.time Instant ZoneId ZonedDateTime))
  (:gen-class))


(defn analyze [text]
  (cond
    (re-find #"(?i)\bhello\b" text) {:type :greet :conf 0.99}
    (re-find #"(?i)\bbye\b"   text) {:type :farewell :conf 0.99}
    :else                           {:type :unknown :conf 0.5}))

(def gazetteer (delay (ner-er/load-gazetteer)))

(defn tokenize [text]
  (->> (re-seq #"[\p{L}\p{Nd}]+(?:'[\p{L}\p{Nd}]+)?" text)
       (map #(apply str %))
       vec))

(defn pos-tag [tokens]
  (->> tokens
       (map (fn [t]
              (cond
                (re-matches #"[A-Z][a-z]+" t) [t "NNP"]
                (re-matches #"[0-9]+" t)      [t "CD"]
                (re-matches #".+ing" t)       [t "VBG"]
                :else                          [t "NN"])))
       vec))

(defn parse-tree [tagged]
  (into [:utterance]
        (map (fn [[token tag]] [tag token]) tagged)))

(defn handle-input [db text ts]
  (let [utt-node (gm/add-utterance! db text ts)
        intent (analyze text)
        intent-node (gm/add-intent! db intent)
        link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
        tokens (tokenize text)
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
   (let [utt-node (gm/add-utterance! db text ts)
         intent (analyze text)
         intent-node (gm/add-intent! db intent)
         link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
         tokens (tokenize text)
         tagged (pos-tag tokens)
         now (now-from-ts ts)
         entities (ner-v4/recognize-entities tokens tagged text now opts)
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
      :tokens tokens
      :pos tagged
      :entities stored
      :relations relations
      :links [link]})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "nlp-interface ready"))
