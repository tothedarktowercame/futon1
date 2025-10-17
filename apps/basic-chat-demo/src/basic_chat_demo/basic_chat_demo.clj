(ns basic-chat-demo.basic-chat-demo
  (:require
   [app.commands :as commands]
   [app.context :as context]
   [app.header :as header]
   [app.slash :as slash]
   [app.bang :as bang]
   [app.store :as store]
   [app.store-manager :as store-manager]
   [app.cli-runner :as cli-run]
   [app.xt :as xt]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.stacktrace :as st]
   [graph-memory.main :as gm]
   [protocols.registry :as registry]))

(def default-protocol "basic-chat/v6")
(def exit-commands #{":quit" ":exit" "quit" "exit"})

(defn- focus-header-json-str [fh]
  (some-> fh header/focus-header-json str/trim not-empty))

(defn- focus-header-lines [fh]
  (let [lines (some-> fh header/focus-header-lines)]
    (when (seq lines) lines)))

(defn- getenv-nonblank [k]
  (let [v (System/getenv k)]
    (when (and v (not (str/blank? v))) v)))

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

(defn- resolve-data-dir []
  (let [from-env (getenv-nonblank "BASIC_CHAT_DATA_DIR")
        from-prop (some-> (System/getProperty "basic-chat.data-root") str/trim not-empty)
        repo-data (some-> (repo-root) (io/file "data") .getAbsolutePath)
        fallback (.getAbsolutePath (io/file "data"))
        chosen (or from-env from-prop repo-data fallback)
        absolute (.getAbsolutePath (io/file chosen))]
    (System/setProperty "basic-chat.data-root" absolute)
    absolute))

(defn- xt-enabled-env? []
  (let [raw (some-> (System/getenv "BASIC_CHAT_XTDB_ENABLED") str/lower-case)]
    (not (contains? #{"false" "0" "off" "no"} raw))))

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

        "--list-entities" (recur (assoc opts :list-entities? true) more)

        "--links"
        (if-let [value (first more)]
          (recur (assoc opts :links value) (rest more))
          (do (println "Missing value for --links") (usage)))

        "--ner-fallback" (recur (assoc opts :ner-fallback? true) more)

        "--context"
        (if-let [value (first more)]
          (let [val (str/lower-case value)
                flag (not (contains? #{"false" "0" "off" "no"} val))]
            (recur (assoc opts :context? flag) (rest more)))
          (recur (assoc opts :context? true) more))

        "--no-context" (recur (assoc opts :context? false) more)

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

        "--fh"      (recur (assoc opts :focus-header? true) more)
        "--fh-only" (recur (assoc opts :focus-header? true :focus-header-only? true) more)
        "--fh-debug" (recur (assoc opts :focus-header? true :focus-header-debug? true) more)
        "--compact" (recur (assoc opts :compact? true) more)
        "--reset"   (recur (assoc opts :reset? true) more)

        "--data-root"
        (if-let [value (first more)]
          (recur (assoc opts :data-root value) (rest more))
          (do (println "Missing value for --data-root") (usage)))

        "--export"
        (if-let [value (first more)]
          (recur (assoc opts :export value) (rest more))
          (do (println "Missing value for --export") (usage)))

        "--" (recur opts more)

        (do (println "Unknown option" opt) (usage)))
      opts)))

(defn- label [value]
  (cond
    (keyword? value) (-> value name (str/replace #"[_-]" " "))
    (string? value) value
    (nil? value) nil
    :else (str value)))

(defn- confidence-str [conf]
  (when (number? conf) (format "%.2f" (double conf))))

(defn- format-entity-line [{:keys [name type source confidence value]}]
  (let [parts (->> [(some-> type label)
                    (some-> source label)
                    (some-> (confidence-str confidence) (format "conf %s"))
                    (when value (str "value " value))]
                   (remove str/blank?) vec)]
    (str "  - " (or name "<unknown>")
         (when (seq parts)
           (str " — " (str/join ", " parts))))))

(defn- format-entities-block [entities]
  (when (seq entities)
    (into ["Entities:"] (map format-entity-line entities))))

(defn- format-relation-line [{:keys [src dst type direction]}]
  (let [left-arrow (if (= direction :in) "<-" "->")
        relation (some-> type label)]
    (str "  - " (or src "?") " -[" (or relation "?") "]" left-arrow " " (or dst "?"))))

(defn- format-relations-block [relations]
  (when (seq relations)
    (into ["Relations:"] (map format-relation-line relations))))

(defn- format-intent-line [{:keys [intent]}]
  (when (seq intent)
    (let [{:keys [type conf]} intent
          base (str "Intent: " (or (some-> type label) "unspecified"))
          conf-str (confidence-str conf)]
      (if conf-str (str base " (confidence " conf-str ")") base))))

(defn- format-context-block [printable context-text]
  (when-let [text (or (:focus-header printable) context-text)]
    (let [lines (str/split-lines text)
          first-line (some-> lines first str/trim str/lower-case)]
      (when (seq lines)
        (if (= first-line "context:")
          (into ["Context:"] (map #(str "  " %) (rest lines)))
          lines)))))

(defn- human-lines [printable context-text]
  (let [blocks (->> [(format-entities-block (:entities printable))
                     (format-relations-block (:relations printable))
                     (when-let [intent-line (format-intent-line printable)] [intent-line])
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
      (doseq [line more] (println (str "     " line))))))

(defn- print-focus-header-lines! [fh-lines]
  (doseq [line fh-lines] (println (str "fh> " line))))

(defn- call-runner [runner line ts state ctx]
  (try
    (runner line ts state ctx)                       ; preferred 4-arity
    (catch clojure.lang.ArityException _
      (runner line ts))))                            ; legacy 2-arity

(defn- call-cmd [f arg state ctx]
  (try
    (f arg state ctx)                                ; preferred
    (catch clojure.lang.ArityException _
      (f arg state))))                               ; legacy

(defn interactive-loop!
  [{:keys [runner command-handler bang-handler intro-lines after-turn
           focus-header? focus-header-only? ctx on-exit] :as _opts}]
  (let [runner          (or runner (fn [line ts & _] {:message (str "echo: " line)}))
        command-handler (or command-handler (fn [& _] {:message "Unknown command"}))
        bang-handler    (or bang-handler (fn [& _] {:message "Unknown bang"}))]
    (println "basic-chat-demo interactive mode")
    (println "Type your message and press enter. Use :quit to exit.")
    (doseq [line intro-lines] (println line))
    (loop [state {}
           prompt-printed? false]
      (when-not prompt-printed?
        (print "you> ") (flush))
      (let [line (try (read-line) (catch java.io.IOException _ nil))]
        (if (nil? line)
          (do
            (println "\nGoodbye!")
            (when on-exit (try (on-exit) (catch Throwable _)))
            (System/exit 0))
          (let [line (str/trim line)]
            (cond
              (exit-commands line)
              (do
                (println "Goodbye!")
                (when on-exit (try (on-exit) (catch Throwable _)))
                (System/exit 0))

          ;; BLANK: do not print another prompt right away; keep same line clean
              (str/blank? line)
              (recur state true)

              (and bang-handler (str/starts-with? line "!"))
              (let [cmd (subs line 1)
                    next-state
                    (try
                      (let [{:keys [message new-state] :or {new-state state}}
                            (call-cmd bang-handler cmd state ctx)]
                        (when message (println (str "bot> " message)))
                        new-state)
                      (catch Throwable t
                        (binding [*out* *err*] (st/print-cause-trace t))
                        state))]
                (recur next-state false))

              (and command-handler (str/starts-with? line "/"))
              (let [cmd (subs line 1)
                    next-state
                    (try
                      (let [{:keys [message new-state] :or {new-state state}}
                            (call-cmd command-handler cmd state ctx)]
                        (when message
                          (if (sequential? message)
                            (print-bot-lines message)
                            (println (str "bot> " message))))
                        new-state)
                      (catch Throwable t
                        (binding [*out* *err*] (st/print-cause-trace t))
                        state))]
                (recur next-state false))

              :else
              (let [ts (System/currentTimeMillis)
                    next-state
                    (try
                      (let [out (call-runner runner line ts state ctx)
                            context-lines (:context out)
                            focus-header-lines* (:focus-header-lines out)
                            printable (-> out
                                          (cond-> context-lines (dissoc :context))
                                          (dissoc :focus-header))
                            rendered (context/render-context context-lines)
                            human (human-lines printable rendered)
                            new-state (assoc state :last-result out)]
                    ;; show concise extraction banner from cli_runner
                        (doseq [l (:bot-lines out)] (println "bot>" l))
                    ;; only show the legacy fallback when it contains real blocks
                        (when-not (and (= 1 (count human))
                                       (re-find #"^No structured data extracted" (first human)))
                          (print-bot-lines human))
                        (when (and focus-header? (seq focus-header-lines*))
                          (print-focus-header-lines! focus-header-lines*))
                        (when after-turn
                          (try (after-turn)
                               (catch Throwable t
                                 (binding [*out* *err*] (st/print-cause-trace t)))))
                        new-state)
                      (catch Throwable t
                        (binding [*out* *err*] (st/print-cause-trace t))
                        state))]
                (recur next-state false)))))))))

(defn supports-entity-commands? [protocol-id]
  (contains? #{"basic-chat/v3" "basic-chat/v4" "basic-chat/v5" "basic-chat/v6"} protocol-id))

(defn context->conn [ctx]
  (if (and (map? ctx) (:db ctx)) (:db ctx) ctx))

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
    (when list-entities? (list-entities! conn))
    (when links (list-links! conn links))))

(defn- focus-policy-overrides [{:keys [neighbors context-cap allow-works? focus-days]}]
  (cond-> {}
    (some? neighbors) (assoc :k-per-anchor neighbors)
    (some? context-cap) (assoc :context-cap-total context-cap)
    (some? allow-works?) (assoc :allow-works? allow-works?)
    (some? focus-days) (assoc :focus-days focus-days)))

(defn- focus-header-data
  [{:keys [anchors intent time turn-id policy focus-limit debug?]}]
  (when-let [fh (header/focus-header (xt/node)                   ;; FIX: use alias, not app.xt/node
                                     {:anchors anchors
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

(defn -main [& _]
  (let [ctx      (store-manager/start!)
        profile  (store-manager/default-profile)
        conn     (store-manager/conn profile)
        node     (xt/node)                                   ;; one shared node
        slash-fn (slash/handler {:env (store-manager/env profile)
                                 :xt-node node}             ;; pass the node
                                (atom {:conn conn}))]
    (interactive-loop!
     {:intro-lines ["Protocol basic-chat/v6 — routes utterances through open-world ingest so"
                    "entities/relations reflect the OpenIE pipeline and persist in XTDB."]
      :ctx {:db conn :xtdb-node node}                       ;; make it available to runner
      :on-exit store-manager/shutdown!
      :runner cli-run/runner
      :command-handler (fn [cmd state ctx]
                         (slash-fn cmd state))              ;; reuse same slash router
      :focus-header? true})))

