(ns basic-chat-demo.basic-chat-demo
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [nlp-interface.nlp-interface :as nlp]
            [graph-memory.main :as gm]))  ;; adjust ns if you put API elsewhere

(def default-protocol "basic-chat/v1")

(defn usage []
  (println "Usage: clojure -M:run-m [-- --protocol basic-chat/vN]" )
  (println "       clojure -M:run-m -- --protocol basic-chat/vN --script path/to/script.edn")
  (System/exit 1))

(defn parse-args [args]
  (loop [opts {:protocol default-protocol}
         [opt & more :as remaining] args]
    (if opt
      (case opt
        "--protocol"
        (if-let [value (first more)]
          (recur (assoc opts :protocol value) (rest more))
          (do (println "Missing value for --protocol") (usage)))

        "--script"
        (if-let [value (first more)]
          (recur (assoc opts :script value) (rest more))
          (do (println "Missing value for --script") (usage)))

        "--"
        (recur opts more)

        (do (println "Unknown option" opt) (usage)))
      opts)))

(defn resolve-protocol-file [protocol-id]
  (let [start (io/file (System/getProperty "user.dir"))
        target (str protocol-id ".edn")]
    (loop [dir start]
      (when dir
        (let [candidate (io/file dir "protocols" target)]
          (if (.exists candidate)
            candidate
            (recur (.getParentFile dir))))))))

(defn run-line! [db line ts]
  (let [res (nlp/handle-input db line ts)]
    ;; return a small, stable map for testing (“golden”)
    {:in line
     :intent (:intent res)
     :links  (:links res)}))

(def exit-commands #{":quit" ":exit" "quit" "exit"})

(defn interactive-loop! [db]
  (println "basic-chat-demo interactive mode")
  (println "Type your message and press enter. Use :quit to exit.")
  (loop []
    (print "you> ")
    (flush)
    (let [line (try (read-line)
                    (catch java.io.IOException _ nil))]
      (if (nil? line)
        (do (println "\nGoodbye!")
            (System/exit 0))
        (let [line (str/trim line)]
          (cond
            (exit-commands line)
            (do (println "Goodbye!")
                (System/exit 0))

            (str/blank? line)
            (recur)

            :else
            (let [ts (System/currentTimeMillis)
                  out (run-line! db line ts)]
              (println (str "bot> " (pr-str out)))
              (recur))))))))

(defn -main [& args]
  ;; modes:
  ;;  - no args  -> interactive REPL-ish loop (later)
  ;;  - flags    -> parse EDN scripts and replay them (current path)
  (let [{:keys [protocol script]} (parse-args args)
        protocol-file (resolve-protocol-file protocol)
        _ (when-not protocol-file
            (println "Unknown protocol" protocol)
            (usage))
        db (gm/init-db)]
    (if script
      (let [script-path script
            lines       (-> script-path slurp edn/read-string)
            now         (System/currentTimeMillis)
            out         (map-indexed (fn [i line] (run-line! db line (+ now i))) lines)]
        ;; protocol will guide branching logic in later versions.
        (println (pr-str (vec out))))
      (interactive-loop! db))))
