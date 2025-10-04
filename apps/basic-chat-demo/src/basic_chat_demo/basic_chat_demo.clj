(ns basic-chat-demo.basic-chat-demo
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [graph-memory.main :as gm]
            [protocols.registry :as registry]))

(def default-protocol "basic-chat/v1")

(def exit-commands #{":quit" ":exit" "quit" "exit"})

(defn usage []
  (println "Usage: clojure -M:run-m [-- --protocol basic-chat/vN]")
  (println "       clojure -M:run-m -- --protocol basic-chat/vN --script path/to/script.edn")
  (println "       clojure -M:run-m -- --protocol basic-chat/v3 --list-entities")
  (println "       clojure -M:run-m -- --protocol basic-chat/v3 --links 'Serena'")
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

        "--list-entities"
        (recur (assoc opts :list-entities? true) more)

        "--links"
        (if-let [value (first more)]
          (recur (assoc opts :links value) (rest more))
          (do (println "Missing value for --links") (usage)))

        "--"
        (recur opts more)

        (do (println "Unknown option" opt) (usage)))
      opts)))

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

            (and command-handler (str/starts-with? line "/"))
            (let [cmd (subs line 1)
                  {:keys [message new-state] :or {new-state state}}
                  (command-handler cmd state)]
              (when message
                (println (str "bot> " message)))
              (recur new-state))

            :else
            (let [ts (System/currentTimeMillis)
                  out (runner line ts)
                  new-state (assoc state :last-result out)]
              (println (str "bot> " (pr-str out)))
              (recur new-state))))))))

(defn supports-entity-commands? [protocol-id]
  (= protocol-id "basic-chat/v3"))

(defn list-entities! [conn]
  (let [entities (gm/entities-by-name conn nil)]
    (if (seq entities)
      (println (str "entities: "
                    (->> entities
                         (map (fn [ent]
                                (let [entity-name (:entity/name ent)
                                      type (:entity/type ent)]
                                  (if type
                                    (str entity-name " (" (clojure.core/name type) ")")
                                    entity-name))))
                         (str/join ", "))))
      (println "entities: none"))))

(defn list-links! [conn name]
  (let [entity (first (gm/entities-by-name conn name))]
    (if-let [entity-id (:entity/id entity)]
      (let [neighbors (gm/neighbors conn entity-id)]
        (if (seq neighbors)
          (println (str name " links: "
                        (->> neighbors
                             (map #(get-in % [:entity :entity/name]))
                             (distinct)
                             (str/join ", "))))
          (println (str "no links recorded for " name))))
      (println (str "entity not found: " name)))))

(defn maybe-run-exploration! [protocol-id conn {:keys [list-entities? links]}]
  (when (supports-entity-commands? protocol-id)
    (when list-entities?
      (list-entities! conn))
    (when links
      (list-links! conn links))))

(defn -main [& args]
  (let [{:keys [protocol script] :as opts} (parse-args args)
        entry (registry/fetch protocol)]
    (when-not entry
      (println "Unknown protocol" protocol)
      (usage))
    (let [ctx ((:init entry))
          handle (:handle entry)
          runner (fn [text ts]
                   (handle ctx text ts))
          command-handler (when-let [ch (:command-handler entry)]
                            (ch ctx))]
      (if script
        (let [lines (-> script slurp edn/read-string)
              now   (System/currentTimeMillis)
              out   (map-indexed (fn [i line]
                                   (runner line (+ now i)))
                                 lines)]
          (println (pr-str (vec out)))
          (maybe-run-exploration! protocol ctx opts))
        (do
          (maybe-run-exploration! protocol ctx opts)
          (interactive-loop! {:runner runner
                              :command-handler command-handler}))))))
