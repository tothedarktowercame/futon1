(ns basic-chat-demo.basic-chat-demo
  (:require [app.commands :as commands]
            [app.context :as context]
            [app.store :as store]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [graph-memory.main :as gm]
            [protocols.registry :as registry]))

(def default-protocol "basic-chat/v1")

(def exit-commands #{":quit" ":exit" "quit" "exit"})

(defonce !env (atom {:data-dir "data"
                     :snapshot-every 100}))

(defonce !conn (atom nil))

(defn boot! []
  (let [data-dir (:data-dir @!env)
        conn (store/restore! data-dir)]
    (reset! !conn conn)
    conn))

(defn ensure-booted! []
  (when (nil? @!conn)
    (boot!))
  @!conn)

(defn usage []
  (println "Usage: clojure -M:run-m [-- --protocol basic-chat/vN]")
  (println "       clojure -M:run-m -- --protocol basic-chat/vN --script path/to/script.edn")
  (println "       clojure -M:run-m -- --protocol basic-chat/v3 --list-entities")
  (println "       clojure -M:run-m -- --protocol basic-chat/v3 --links 'Serena'")
  (println "       clojure -M:run-m -- --protocol basic-chat/v4 --ner-fallback")
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

        "--ner-fallback"
        (recur (assoc opts :ner-fallback? true) more)

        "--context"
        (if-let [value (first more)]
          (let [val (str/lower-case value)
                flag (not (contains? #{"false" "0" "off" "no"} val))]
            (recur (assoc opts :context? flag) (rest more)))
          (recur (assoc opts :context? true) more))

        "--no-context"
        (recur (assoc opts :context? false) more)

        "--neighbors"
        (if-let [value (first more)]
          (recur (assoc opts :neighbors (Integer/parseInt value)) (rest more))
          (do (println "Missing value for --neighbors") (usage)))

        "--context-cap"
        (if-let [value (first more)]
          (recur (assoc opts :context-cap (Integer/parseInt value)) (rest more))
          (do (println "Missing value for --context-cap") (usage)))

        "--compact"
        (recur (assoc opts :compact? true) more)

        "--reset"
        (recur (assoc opts :reset? true) more)

        "--export"
        (if-let [value (first more)]
          (recur (assoc opts :export value) (rest more))
          (do (println "Missing value for --export") (usage)))

        "--"
        (recur opts more)

        (do (println "Unknown option" opt) (usage)))
      opts)))

(defn interactive-loop! [{:keys [runner command-handler bang-handler intro-lines after-turn]}]
  (println "basic-chat-demo interactive mode")
  (println "Type your message and press enter. Use :quit to exit.")
  (doseq [line intro-lines]
    (println line))
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

            (and bang-handler (str/starts-with? line "!"))
            (let [cmd (subs line 1)
                  {:keys [message new-state] :or {new-state state}}
                  (bang-handler cmd state)]
              (when message
                (println (str "bot> " message)))
              (recur new-state))

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
                  context-lines (:context out)
                  printable (if context-lines (dissoc out :context) out)
                  rendered (context/render-context context-lines)
                  new-state (assoc state :last-result out)]
              (println (str "bot> " (pr-str printable)))
              (when rendered
                (println (str "bot> " rendered)))
              (when after-turn
                (after-turn))
              (recur new-state))))))))

(defn supports-entity-commands? [protocol-id]
  (contains? #{"basic-chat/v3" "basic-chat/v4"} protocol-id))

(defn context->conn [ctx]
  (if (and (map? ctx) (:db ctx))
    (:db ctx)
    ctx))

(defn list-entities! [conn]
  (let [entities (gm/entities-by-name (context->conn conn) nil)]
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
  (let [entity (first (gm/entities-by-name (context->conn conn) name))]
    (if-let [entity-id (:entity/id entity)]
      (let [neighbors (gm/neighbors (context->conn conn) entity-id)]
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
  (let [raw-opts (parse-args args)
        opts (merge {:context? true
                     :neighbors 3
                     :context-cap 10}
                    raw-opts)
        {:keys [protocol script]} opts
        entry (registry/fetch protocol)]
    (when-not entry
      (println "Unknown protocol" protocol)
      (usage))
    (ensure-booted!)
    (let [data-dir (:data-dir @!env)
          data-file (io/file data-dir)]
      (when (:reset? opts)
        (when (.exists data-file)
          (io/delete-file data-file true))
        (.mkdirs data-file)
        (boot!)
        (println (str "Store reset: " (.getAbsolutePath data-file))))
      (when (:compact? opts)
        (store/compact! @!conn {:data-dir data-dir})
        (println "Snapshot saved."))
      (when-let [export (:export opts)]
        (case (str/lower-case export)
          "edn" (do (println (pr-str (store/export-edn @!conn)))
                    (System/exit 0))
          (do (println "Unsupported export format" export)
              (System/exit 1)))))
    (let [ctx-base ((:init entry))
          ctx-with-conn (if (map? ctx-base)
                          (cond-> ctx-base
                            @!conn (assoc :db @!conn))
                          (or @!conn ctx-base))
          ctx (if-let [configure (:configure entry)]
                (configure ctx-with-conn opts)
                ctx-with-conn)
          handle (:handle entry)
          context-config {:neighbors (:neighbors opts)
                          :context-cap (:context-cap opts)
                          :context? (:context? opts)}
          runner (fn [text ts]
                    (let [res (handle ctx text ts)
                          ensured (into {}
                                         (for [{:keys [name type]} (:entities res)
                                               :when (seq name)]
                                           [name (store/ensure-entity! @!conn @!env {:name name :type type})]))
                          _ (doseq [{:keys [type src dst]} (:relations res)
                                    :when (and type src dst)]
                              (let [src-spec (or (some-> (get ensured src)
                                                         (select-keys [:id :name :type]))
                                                 {:name src})
                                    dst-spec (or (some-> (get ensured dst)
                                                         (select-keys [:id :name :type]))
                                                 {:name dst})]
                                (store/upsert-relation! @!conn @!env {:type type
                                                                      :src src-spec
                                                                      :dst dst-spec})))
                          context-lines (context/enrich-with-neighbors @!conn (:entities res) context-config)]
                      (cond-> res context-lines (assoc :context context-lines))))
          command-handler (when-let [ch (:command-handler entry)]
                            (ch ctx))
          bang-handler (when (supports-entity-commands? protocol)
                         (fn [cmd state]
                           (try
                             (let [{:keys [message result]} (commands/handle @!conn @!env cmd)]
                               {:message message
                                :new-state (if result
                                             (assoc state :last-command result)
                                             state)})
                             (catch Exception e
                               {:message (or (ex-message e) "command failed")
                                :new-state state}))))]
      (if script
        (let [lines (-> script slurp edn/read-string)
              now   (System/currentTimeMillis)
              out   (map-indexed (fn [i line]
                                   (runner line (+ now i)))
                                 lines)]
          (println (pr-str (vec out)))
          (maybe-run-exploration! protocol ctx opts))
        (let [after-turn (when (and (supports-entity-commands? protocol)
                                    (or (:list-entities? opts)
                                        (:links opts)))
                           #(maybe-run-exploration! protocol ctx opts))]
          (maybe-run-exploration! protocol ctx opts)
          (interactive-loop! {:runner runner
                              :command-handler command-handler
                              :bang-handler bang-handler
                              :intro-lines (seq (:intro entry))
                              :after-turn after-turn}))))))
