(ns protocols.basic-chat.v4
  (:require [clojure.string :as str]
            [graph-memory.main :as gm]
            [nlp-interface.nlp-interface :as nlp]))

(def intro
  ["Protocol basic-chat/v4 — tiered NER with gazetteer, patterns, titlecase, domain."
   "Recognises people/places/orgs/projects/tools, dates/times, versions, files, URLs."
   "Use --ner-fallback to allow conservative fallback matches."])

(def ^:private first-singular-pronouns
  #{"i" "me" "my" "mine" "myself" "one" "oneself"})

(def ^:private first-plural-pronouns
  #{"we" "us" "our" "ours" "ourselves"})

(def ^:private interlocutor-pronouns
  #{"you" "your" "yours" "yourself" "yourselves"})

(defn- rename-pronoun [{:keys [me you we]} name]
  (let [normalized (some-> name str/trim)
        lower (some-> normalized str/lower-case)]
    (cond
      (contains? first-singular-pronouns lower) me
      (contains? first-plural-pronouns lower) we
      (contains? interlocutor-pronouns lower) you
      :else normalized)))

(defn- normalize-pronouns [pronouns {:keys [entities relations] :as res}]
  (let [pronouns (merge {:me "Me" :you "You" :we "We"} pronouns)
        rename (partial rename-pronoun pronouns)
        entities' (mapv (fn [ent]
                          (let [original (:name ent)
                                lower (some-> original str/lower-case)
                                updated (rename original)]
                            (cond-> (assoc ent :name updated)
                              (contains? first-singular-pronouns lower) (assoc :type :person)
                              (contains? first-plural-pronouns lower) (assoc :type :group)
                              (contains? interlocutor-pronouns lower) (assoc :type :person))))
                        entities)
        relations' (->> relations
                        (map (fn [rel]
                               (let [src (rename (:src rel))
                                     dst (rename (:dst rel))]
                                 (-> rel
                                     (assoc :src src :dst dst)
                                     (update :type #(or % :links-to))))))
                        (remove #(= (:src %) (:dst %)))
                        distinct
                        vec)]
    (assoc res :entities entities' :relations relations')))

(defn init []
  {:db (gm/init-db)
   :opts {}})

(defn configure [ctx {:keys [ner-fallback?]}]
  (update ctx :opts assoc :enable-fallback? (boolean ner-fallback?)))

(defn handle [{:keys [db opts pronouns]} line ts]
  (let [{:keys [intent entities tokens pos links relations] :as res}
        (normalize-pronouns pronouns (nlp/handle-input-v4 db line ts opts))]
    {:in line
     :intent intent
     :entities (mapv (fn [ent]
                       (select-keys ent [:name :type :confidence :source :id :span :aliases :value :entity-id :entity-db]))
                     entities)
     :relations relations
     :tokens tokens
     :pos pos
     :links links}))
