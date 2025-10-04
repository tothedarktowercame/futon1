(ns nlp-interface.nlp-interface
  (:require [graph-memory.main :as gm])
  (:gen-class))


(defn analyze [text]
  (cond
    (re-find #"(?i)\bhello\b" text) {:type :greet :conf 0.99}
    (re-find #"(?i)\bbye\b"   text) {:type :farewell :conf 0.99}
    :else                           {:type :unknown :conf 0.5}))

(defn handle-input [db text ts]
  (let [utt-node (gm/add-utterance! db text ts)
        intent (analyze text)
        intent-node (gm/add-intent! db intent)
        link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)]
    {:utterance utt-node
     :intent intent
     :links [link]}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "nlp-interface ready"))
