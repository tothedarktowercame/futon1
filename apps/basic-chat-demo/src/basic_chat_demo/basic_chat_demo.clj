(ns basic-chat-demo.basic-chat-demo
  (:require [clojure.edn :as edn]
            [nlp-interface.nlp-interface :as nlp]
            [graph-memory.main :as gm]))  ;; adjust ns if you put API elsewhere

(defn run-line! [db line ts]
  (let [res (nlp/handle-input db line ts)]
    ;; return a small, stable map for testing (“golden”)
    {:in line
     :intent (:intent res)
     :links  (:links res)}))

(defn -main [& args]
  ;; modes:
  ;;  - no args  -> interactive REPL-ish loop (later)
  ;;  - --script path.edn -> read EDN vector of lines, print EDN of results
  (let [db (gm/init-db)]
    (cond
      (and (= (count args) 2) (= (first args) "--script"))
      (let [script (-> (second args) slurp edn/read-string)
            now    (System/currentTimeMillis)
            out    (map-indexed (fn [i line] (run-line! db line (+ now i))) script)]
        (println (pr-str (vec out))))

      :else
      (do (println "Usage: clojure -M:run-m -- --script path/to/script.edn")
          (System/exit 1)))))
