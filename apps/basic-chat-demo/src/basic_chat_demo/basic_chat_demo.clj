(ns basic-chat-demo.basic-chat-demo
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [nlp-interface.nlp-interface :as nlp]
            [graph-memory.main :as gm]
            [v2.graph-memory :as gm-v2]
            [v2.nlp-interface :as nlp-v2]))  ;; adjust ns if you put API elsewhere

(def default-protocol "basic-chat/v1")

(defn usage []
  (println "Usage: clojure -M:run-m [-- --protocol basic-chat/vN]")
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

(defn load-protocol [protocol-id]
  (when-let [file (resolve-protocol-file protocol-id)]
    (-> file slurp edn/read-string)))

(defn run-line! [db line ts]
  (let [res (nlp/handle-input db line ts)]
    ;; return a small, stable map for testing (“golden”)
    {:in line
     :intent (:intent res)
     :links  (:links res)}))

(defmulti process-line (fn [{:keys [protocol]}] (:protocol/id protocol)))

(defmethod process-line "basic-chat/v1" [{:keys [db text ts]}]
  (run-line! db text ts))

(defmethod process-line "basic-chat/v2" [{:keys [text]}]
  (let [before (gm-v2/labels)
        summary (nlp-v2/answer text)
        entities (vec (nlp-v2/recent-entities 5))
        after (gm-v2/labels)
        new-labels (vec (sort (set/difference after before)))]
    {:in text
     :summary summary
     :entities entities
     :new-labels new-labels}))

(defmethod process-line :default [{:keys [protocol]}]
  (throw (ex-info (str "Unsupported protocol " (:protocol/id protocol))
                  {:protocol protocol})))

(def exit-commands #{":quit" ":exit" "quit" "exit"})

(defn interactive-loop! [{:keys [runner command-handler]}]
  (println "basic-chat-demo interactive mode")
  (println "Type your message and press enter. Use :quit to exit.")
  (loop [state {}]
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
            (recur state)

            (str/starts-with? line "/")
            (let [cmd (subs line 1)
                  {:keys [message new-state] :or {new-state state}}
                  (if command-handler
                    (command-handler cmd state)
                    {:message "commands unavailable"})]
              (when message
                (println (str "bot> " message)))
              (recur new-state))

            :else
            (let [ts (System/currentTimeMillis)
                  out (runner line ts)
                  new-state (assoc state :last-result out)]
              (println (str "bot> " (pr-str out)))
              (recur new-state))))))))

(defn -main [& args]
  ;; modes:
  ;;  - no args  -> interactive REPL-ish loop (later)
  ;;  - flags    -> parse EDN scripts and replay them (current path)
  (let [{:keys [protocol script]} (parse-args args)
        protocol-data (load-protocol protocol)]
    (when-not protocol-data
      (println "Unknown protocol" protocol)
      (usage))
    (let [v2? (= (:protocol/id protocol-data) "basic-chat/v2")]
      (when v2?
        (gm-v2/reset-db!))
      (let [db (gm/init-db)
            runner (fn [text ts]
                     (process-line {:protocol protocol-data
                                    :db db
                                    :text text
                                    :ts ts}))
            command-handler (when v2?
                               (fn [cmd state]
                                 (case cmd
                                   "diff"
                                   (let [new-labels (get-in state [:last-result :new-labels])
                                         msg (if (seq new-labels)
                                               (str "new labels: " (pr-str new-labels))
                                               "no new labels detected")]
                                     {:message msg})

                                   "dump"
                                   {:message (pr-str (gm-v2/summary))}

                                   {:message (str "unknown command: /" cmd)})))]
        (if script
          (let [script-path script
                lines       (-> script-path slurp edn/read-string)
                now         (System/currentTimeMillis)
                out         (map-indexed (fn [i line] (runner line (+ now i))) lines)]
            (println (pr-str (vec out))))
          (interactive-loop! {:runner runner
                              :command-handler command-handler}))))))
