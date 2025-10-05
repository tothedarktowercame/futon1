(ns protocols.basic-chat.v4
  (:require [graph-memory.main :as gm]
            [nlp-interface.nlp-interface :as nlp]))

(def intro
  ["Protocol basic-chat/v4 — tiered NER with gazetteer, patterns, titlecase, domain."
   "Recognises people/places/orgs/projects/tools, dates/times, versions, files, URLs."
   "Use --ner-fallback to allow conservative fallback matches."])

(defn init []
  {:db (gm/init-db)
   :opts {}})

(defn configure [ctx {:keys [ner-fallback?]}]
  (update ctx :opts assoc :enable-fallback? (boolean ner-fallback?)))

(defn handle [{:keys [db opts]} line ts]
  (let [{:keys [intent entities tokens pos links] :as res}
        (nlp/handle-input-v4 db line ts opts)]
    {:in line
     :intent intent
     :entities (mapv (fn [ent]
                       (select-keys ent [:name :type :confidence :source :id :span :aliases :value :entity-id :entity-db]))
                     entities)
     :relations (:relations res)
     :tokens tokens
     :pos pos
     :links links}))
