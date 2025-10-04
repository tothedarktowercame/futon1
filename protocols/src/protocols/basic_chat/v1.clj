(ns protocols.basic-chat.v1
  (:require [graph-memory.main :as gm]
            [nlp-interface.nlp-interface :as nlp]))

(defn init []
  (gm/init-db))

(defn handle [db line ts]
  (let [res (nlp/handle-input db line ts)]
    {:in line
     :intent (:intent res)
     :links (:links res)}))
