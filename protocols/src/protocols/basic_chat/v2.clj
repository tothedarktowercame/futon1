(ns protocols.basic-chat.v2
  (:require [clojure.set :as set]
            [graph-memory.main :as gm]
            [v2.graph-memory :as gm-v2]
            [v2.nlp-interface :as nlp-v2]))

(defn init []
  (gm-v2/reset-db!)
  (gm/init-db))

(defn handle [_db line _ts]
  (let [before (gm-v2/labels)
        summary (nlp-v2/answer line)
        entities (vec (nlp-v2/recent-entities 5))
        after (gm-v2/labels)
        new-labels (vec (sort (set/difference after before)))]
    {:in line
     :summary summary
     :entities entities
     :new-labels new-labels}))

(defn command-handler [_]
  (fn [cmd state]
    (cond
      (= cmd "diff")
      (let [labels (get-in state [:last-result :new-labels])]
        {:message (if (seq labels)
                    (str "new labels: " (pr-str labels))
                    "no new labels detected")})

      (= cmd "dump")
      {:message (pr-str (gm-v2/summary))}

      :else
      {:message (str "unknown command: /" cmd)})))
