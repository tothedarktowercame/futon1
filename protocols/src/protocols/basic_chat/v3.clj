(ns protocols.basic-chat.v3
  (:require [graph-memory.main :as gm]
            [nlp-interface.nlp-interface :as nlp]))

(defn init []
  (gm/init-db))

(defn handle [db line ts]
  (let [{:keys [intent entities relations]} (nlp/handle-input db line ts)]
    {:in line
     :intent intent
     :entities (mapv #(select-keys % [:name :type]) entities)
     :relations (mapv #(select-keys % [:type]) relations)}))
