(ns protocols.basic-chat.v1
  (:require [graph-memory.main :as gm]
            [nlp-interface.nlp-interface :as nlp]))

(def intro
  ["Protocol basic-chat/v1 — baseline keyword intents (:greet/:farewell)."
   "Try phrases like 'hello there' or 'ok bye' to see the intent map."])

(defn init []
  (gm/init-db))

(defn handle [db line ts]
  (let [res (nlp/handle-input db line ts)]
    {:in line
     :intent (:intent res)
     :links (:links res)}))
