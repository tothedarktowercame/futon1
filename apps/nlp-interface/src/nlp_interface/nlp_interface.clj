(ns nlp-interface.nlp-interface
  (:gen-class))


(defn analyze [text]
  (cond
    (re-find #"(?i)\bhello\b" text) {:type :greet :conf 0.99}
    (re-find #"(?i)\bbye\b"   text) {:type :farewell :conf 0.99}
    :else                           {:type :unknown :conf 0.5}))

(defn handle-input [db text ts]
  (let [utt (gm/add-utterance! db text ts)
        intent (analyze text)
        ent (gm/add-intent! db intent)
        link (gm/link! db (:id utt) (:id ent) :derives)]
    {:utterance utt :intent intent :links [link]}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))
