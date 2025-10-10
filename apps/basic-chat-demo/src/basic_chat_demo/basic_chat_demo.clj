(ns basic-chat-demo.basic-chat-demo
  (:require [app.commands :as commands]
            [app.context :as context]
            [app.header :as header]
            [app.slash :as slash]
            [app.store :as store]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [graph-memory.main :as gm]
            [protocols.registry :as registry]))

(def default-protocol "basic-chat/v5")

(def exit-commands #{":quit" ":exit" "quit" "exit"})

(defn- focus-header-json-str
  [fh]
  (some-> fh header/focus-header-json str/trim not-empty))

(defn- focus-header-lines
  [fh]
  (let [lines (some-> fh header/focus-header-lines)]
    (when (seq lines)
      lines)))

(defn- getenv-nonblank [k]
  (let [v (System/getenv k)]
    (when (and v (not (str/blank? v)))
      v)))

(defn- xt-enabled-env? []
  (let [raw (some-> (System/getenv "BASIC_CHAT_XTDB_ENABLED") str/lower-case)]
    (not (contains? #{"false" "0" "off" "no"} raw))))

(defn- default-env []
  (let [data-dir (or (getenv-nonblank "BASIC_CHAT_DATA_DIR") "data")
        xt-resource (getenv-nonblank "BASIC_CHAT_XTDB_RESOURCE")
        xt-config (cond-> {:enabled? (xt-enabled-env?)}
                    xt-resource (assoc :resource xt-resource)
                    (and (nil? xt-resource)
                         (io/resource "xtdb.edn")) (assoc :config-path (some-> (io/resource "xtdb.edn") io/file .getAbsolutePath)))]
    {:data-dir (-> data-dir io/file .getAbsolutePath)
     :snapshot-every 100
     :xtdb xt-config}))

(defonce !env (atom (default-env)))

(defonce !conn (atom nil))

(defn boot! []
  (let [conn (store/restore! @!env)]
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
  (println "       clojure -M:run-m -- --fh")
  (println "       clojure -M:run-m -- --fh-only")
  (println "       clojure -M:run-m -- --fh-debug")
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

        "--focus-days"
        (if-let [value (first more)]
          (recur (assoc opts :focus-days (Integer/parseInt value)) (rest more))
          (do (println "Missing value for --focus-days") (usage)))

        "--allow-works"
        (if-let [value (first more)]
          (let [val (str/lower-case value)
                flag (contains? #{"on" "true" "1" "yes"} val)]
            (recur (assoc opts :allow-works? flag) (rest more)))
          (do (println "Missing value for --allow-works") (usage)))

        "--fh"
        (recur (assoc opts :focus-header? true) more)

        "--fh-only"
        (recur (assoc opts :focus-header? true
                           :focus-header-only? true) more)

        "--fh-debug"
        (recur (assoc opts :focus-header? true
                           :focus-header-debug? true) more)

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

(defn- label [value]
  (cond
    (keyword? value) (-> value name (str/replace #"[_-]" " "))
    (string? value) value
    (nil? value) nil
    :else (str value)))

(defn- confidence-str [conf]
  (when (number? conf)
    (format "%.2f" (double conf))))

(defn- format-entity-line [{:keys [name type source confidence value]}]
  (let [parts (->> [(some-> type label)
                    (some-> source label)
                    (some-> (confidence-str confidence) (format "conf %s"))
                    (when value (str "value " value))]
                   (remove str/blank?)
                   vec)]
    (str "  - " (or name "<unknown>")
         (when (seq parts)
           (str " — " (str/join ", " parts))))))

(defn- format-entities-block [entities]
  (when (seq entities)
    (into ["Entities:"]
          (map format-entity-line entities))))

(defn- format-relation-line [{:keys [src dst type direction]}]
  (let [left-arrow (if (= direction :in) "<-" "->")
        relation (some-> type label)]
    (str "  - " (or src "?") " -[" (or relation "?") "]" left-arrow " " (or dst "?"))))

(defn- format-relations-block [relations]
  (when (seq relations)
    (into ["Relations:"]
          (map format-relation-line relations))))

(defn- format-intent-line [{:keys [intent]}]
  (when (seq intent)
    (let [{:keys [type conf]} intent
          base (str "Intent: " (or (some-> type label) "unspecified"))
          conf-str (confidence-str conf)]
      (if conf-str
        (str base " (confidence " conf-str ")")
        base))))

(defn- format-context-block [printable context-text]
  (when-let [text (or (:focus-header printable)
                      context-text)]
    (let [lines (str/split-lines text)
          first-line (some-> lines first str/trim str/lower-case)]
      (when (seq lines)
        (if (= first-line "context:")
          (into ["Context:"]
                (map #(str "  " %) (rest lines)))
          lines)))))

(defn- human-lines [printable context-text]
  (let [blocks (->> [(format-entities-block (:entities printable))
                     (format-relations-block (:relations printable))
                     (when-let [intent-line (format-intent-line printable)]
                       [intent-line])
                     (format-context-block printable context-text)]
                    (remove nil?))]
    (if (seq blocks)
      (vec (mapcat identity (interpose [""] blocks)))
      [(if-let [input (:in printable)]
         (str "No structured data extracted for: " input)
         "No structured data extracted.")])))

(defn- print-bot-lines [lines]
  (when (seq lines)
    (let [[first-line & more] lines]
      (println (str "bot> " first-line))
      (doseq [line more]
        (println (str "     " line))))))


(defn- print-focus-header-lines!
  [fh-lines]
  (doseq [line fh-lines]
    (println (str "fh> " line))))


(defn interactive-loop! [{:keys [runner command-handler bang-handler intro-lines after-turn
                                 focus-header? focus-header-only?]}]

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
                (if (sequential? message)
                  (print-bot-lines message)
                  (println (str "bot> " message))))
              (recur new-state))

            :else
            (let [ts (System/currentTimeMillis)
                  out (runner line ts)
                  context-lines (:context out)
                  focus-header-lines* (:focus-header-lines out)
                  printable (-> out
                                (cond-> context-lines (dissoc :context))
                                (dissoc :focus-header))
                  rendered (context/render-context context-lines)
                  human (human-lines printable rendered)
                  new-state (assoc state :last-result out)]
              (when-not focus-header-only?
                (print-bot-lines human))
              (when (and focus-header? (seq focus-header-lines*))
                (print-focus-header-lines! focus-header-lines*))
              (when after-turn
                (after-turn))
              (recur new-state))))))))

(defn supports-entity-commands? [protocol-id]
  (contains? #{"basic-chat/v3" "basic-chat/v4" "basic-chat/v5"} protocol-id))

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

(defn- focus-policy-overrides
  [{:keys [neighbors context-cap allow-works? focus-days]}]
  (cond-> {}
    (some? neighbors) (assoc :k-per-anchor neighbors)
    (some? context-cap) (assoc :context-cap-total context-cap)
    (some? allow-works?) (assoc :allow-works? allow-works?)
    (some? focus-days) (assoc :focus-days focus-days)))

(defn- focus-header-data
  [{:keys [anchors intent time turn-id policy focus-limit debug?]}]
  (when-let [fh (header/focus-header nil {:anchors anchors
                                          :intent intent
                                          :time time
                                          :turn-id turn-id
                                          :policy policy
                                          :focus-limit focus-limit
                                          :debug? debug?})]
    (let [fh-json (focus-header-json-str fh)
          fh-lines (focus-header-lines fh)]
      (cond-> {:focus-header fh}
        fh-json (assoc :focus-header-json fh-json)
        fh-lines (assoc :focus-header-lines fh-lines)))))

(defn -main [& args]
  (let [raw-opts (parse-args args)
        base-opts (merge {:context? true
                          :neighbors 3
                          :context-cap 10
                          :focus-days 30
                          :allow-works? false}
                         raw-opts)
        opts (cond-> base-opts
               (:focus-header-only? base-opts) (assoc :focus-header? true))
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
                          :context? (:context? opts)
                          :focus-days (:focus-days opts)
                          :allow-works? (:allow-works? opts)}
          fh-policy (focus-policy-overrides opts)
          runner (fn [text ts]
                    (let [env-now (assoc @!env :now ts)
                          res (handle ctx text ts)
                         ensured (into {}
                                        (for [{:keys [name type]} (:entities res)
                                               :when (seq name)]
                                           [name (store/ensure-entity! @!conn env-now {:name name :type type})]))
                         _ (doseq [{:keys [type src dst]} (:relations res)
                                   :when (and type src dst)]
                              (let [src-spec (or (some-> (get ensured src)
                                                         (select-keys [:id :name :type]))
                                                 {:name src})
                                    dst-spec (or (some-> (get ensured dst)
                                                         (select-keys [:id :name :type]))
                                                 {:name dst})]

                                (store/upsert-relation! @!conn env-now {:type type
                                                                        :src src-spec
                                                                        :dst dst-spec})))
                          context-lines (context/enrich-with-neighbors @!conn (:entities res)
                                                                                  (assoc context-config
                                                                                         :anchors (vals ensured)
                                                                                         :timestamp ts))
                          fh-policy (focus-policy-overrides opts)
                          fh (when (:focus-header? opts)
                               (header/focus-header nil {:anchors (vals ensured)
                                                         :intent (:intent res)
                                                         :time ts
                                                         :policy fh-policy
                                                         :turn-id ts
                                                         :focus-limit (:context-cap opts)
                                                         :debug? (:focus-header-debug? opts)}))
                          fh-lines (when fh (focus-header-lines fh))
                          fh-json (when fh (focus-header-json-str fh))]
              (-> res
                  (cond-> context-lines (assoc :context context-lines))
                  (cond-> fh (assoc :focus-header fh))
                  (cond-> fh-json (assoc :focus-header-json fh-json))
                  (cond-> fh-lines (assoc :focus-header-lines fh-lines)))))

          entry-command-handler (when-let [ch (:command-handler entry)]
                                  (ch ctx))
          slash-command-handler (when (supports-entity-commands? protocol)
                                  (slash/handler @!conn))
          command-handler (cond
                            (and entry-command-handler slash-command-handler)
                            (fn [cmd state]
                              (let [result (entry-command-handler cmd state)]
                                (if (some? (:message result))
                                  result
                                  (slash-command-handler cmd state))))
                            slash-command-handler slash-command-handler
                            entry-command-handler entry-command-handler
                            :else nil)
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
          (when (:focus-header? opts)
            (let [now (System/currentTimeMillis)
                  fh-policy (focus-policy-overrides opts)
                  fh (header/focus-header nil {:anchors []
                                               :time now
                                               :turn-id now
                                               :policy fh-policy
                                               :focus-limit (:context-cap opts)
                                               :debug? (:focus-header-debug? opts)})
                  fh-lines (focus-header-lines fh)]
              (when fh-lines
                (print-focus-header-lines! fh-lines))))
          (interactive-loop! {:runner runner
                              :command-handler command-handler
                              :bang-handler bang-handler
                              :intro-lines (seq (:intro entry))
                              :after-turn after-turn
                              :focus-header? (:focus-header? opts)
                              :focus-header-only? (:focus-header-only? opts)}))))))
