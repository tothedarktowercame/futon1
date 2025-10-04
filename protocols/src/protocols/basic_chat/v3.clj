(ns protocols.basic-chat.v3
  (:require [graph-memory.main :as gm]
            [nlp-interface.nlp-interface :as nlp]))

(def intro
  ["Protocol basic-chat/v3 — classical gazetteer NER + relation graph."
   "Gazetteer seeds: Lorane, Serena, Joe, Oxford, London, Boston, Vancouver." 
   "Use --list-entities or --links \"Name\" to explore matched entities."])

(defn init []
  (gm/init-db))

(defn handle [db line ts]
  (let [{:keys [intent entities relations]} (nlp/handle-input db line ts)]
    {:in line
     :intent intent
     :entities (mapv #(select-keys % [:name :type]) entities)
     :relations (mapv #(select-keys % [:type]) relations)}))
