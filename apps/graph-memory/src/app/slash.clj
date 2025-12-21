(ns app.slash
  (:require [app.bang :as bang]
            [app.command-service :as svc]
            [app.slash.format :as fmt]
            [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn command-handler [cmd state ctx] (bang/bang-handler cmd state ctx))

(def ^:private help-lines fmt/help-lines)

(defn- parse-type-token [token]
  (when-let [trimmed (some-> token str/trim not-empty)]
    (let [no-colon (if (str/starts-with? trimmed ":")
                     (subs trimmed 1)
                     trimmed)]
      (keyword (str/lower-case no-colon)))))

(defn- parse-edn-safe [s]
  (try
    (edn/read-string s)
    (catch Exception _ nil)))

(defn- parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn- safe-exec
  [thunk]
  (try
    (thunk)
    (catch clojure.lang.ExceptionInfo e
      (let [writer (java.io.StringWriter.)]
        (.printStackTrace e (java.io.PrintWriter. writer))
        (->> (.toString writer)
             str/split-lines
             (cons (str "ERROR: " (or (ex-message e) "command failed")))
             vec)))
    (catch Exception e
      (let [writer (java.io.StringWriter.)]
        (.printStackTrace e (java.io.PrintWriter. writer))
        (->> (.toString writer)
             str/split-lines
             (cons (str "ERROR: " (or (.getMessage e) "command failed")))
             vec)))))

(defn- current-xt-node [{:keys [xt-node xt-node-fn]}]
  (or (when (fn? xt-node-fn)
        (try (xt-node-fn)
             (catch Throwable _ nil)))
      (xt/node)
      xt-node))

(defn- entity-section [conn opts arg]
  (let [[name maybe-type & _] (->> (str/split arg #"\s+")
                                   (remove str/blank?)
                                   (take 2))]
    (if (str/blank? name)
      ["Usage: /entity <name> [type]"]
      (safe-exec
       #(let [spec (cond-> {:name name}
                     maybe-type (assoc :type (parse-type-token maybe-type)))
              {:keys [entity]} (svc/ensure-entity! {:conn conn :env opts} spec)]
          (fmt/entity-lines entity))))))

(defn- relation-section [conn opts arg]
  (let [[type-token src dst & _] (->> (str/split arg #"\s+")
                                      (remove str/blank?))]
    (if (or (str/blank? type-token)
            (str/blank? src)
            (str/blank? dst))
      ["Usage: /relation <type> <src> <dst>"]
      (safe-exec
       #(let [spec {:type (parse-type-token type-token)
                    :src {:name src}
                    :dst {:name dst}}
              {:keys [relation]} (svc/upsert-relation! {:conn conn :env opts} spec)]
          (fmt/relation-lines relation))))))

(defn- me-section [conn opts arg]
  (let [[sub & rest] (->> (str/split arg #"\s+") (remove str/blank?))
        sub (some-> sub str/lower-case)
        svc-ctx {:conn conn
                 :env  opts
                 :profile :me
                 :now (System/currentTimeMillis)
                 :xt-node (current-xt-node opts)}]
    (case sub
      "set" (let [payload-str (str/join " " rest)
                  payload (parse-edn-safe payload-str)]
              (if (map? payload)
                (safe-exec #(fmt/profile-lines (svc/upsert-profile! svc-ctx payload)))
                ["Usage: /me set {<edn map>}"]))
      "doc" (safe-exec #(fmt/pprint-lines (svc/fetch-profile svc-ctx)))
      (let [limit (if (= "summary" sub) (some-> (first rest) parse-int) nil)]
        (safe-exec #(fmt/profile-summary-lines (svc/profile-summary svc-ctx limit)))))))

(defn- types-section [conn arg]
  (let [[sub & rest] (->> (str/split arg #"\s+")
                          (remove str/blank?))
        sub (some-> sub str/lower-case)]
    (case sub
      "parent" (let [[type parent kind & _] rest
                     body {:type type
                           :parent parent
                           :kind kind}]
                 (safe-exec #(fmt/types-lines {:types [(svc/set-type-parent! body)]})))
      "merge" (let [[into & aliases] rest
                    body {:into into :aliases aliases}]
                (safe-exec #(fmt/types-lines {:types [(svc/merge-aliases! body)]})))
      ;; default -> list all types
      (safe-exec #(let [results (svc/list-types conn)
                        all-types (->> (:types results)
                                       vals
                                       (apply concat)
                                       (sort-by :id))]
                    (fmt/types-lines {:types all-types}))))))

(defn- ego-section [conn name]
  (if (str/blank? name)
    ["Usage: /ego <entity>"]
    (fmt/ego-lines (svc/ego conn name) name)))

(defn- cooccur-section [conn name]
  (if (str/blank? name)
    ["Usage: /cooccur <entity>"]
    (fmt/cooccur-lines (svc/cooccurring conn name) name)))

(defn- forget-section [conn opts name]
  (if (str/blank? name)
    ["Usage: /forget <entity>"]
    (safe-exec #(fmt/forget-lines (svc/forget-entity! conn opts name) name))))

(defn- expire-section [conn opts name]
  (if (str/blank? name)
    ["Usage: /expire <entity>"]
    (safe-exec #(fmt/expire-lines (svc/expire-entity! conn opts name) name))))

(def ^:private command-capabilities
  {"tail" [:links :list?]
   "ego" [:links :list?]
   "cooccur" [:events :query?]})

(defn- capability-supported? [ctx path]
  (get-in (:capabilities ctx) path true))

(defn- slash-capability? [ctx cmd]
  (if-let [path (get command-capabilities cmd)]
    (capability-supported? ctx path)
    true))

(defn handler
  "Return a slash-command handler bound to the Datascript connection."
  [opts !state]
  (fn [raw state]
    (let [trimmed (str/trim raw)
          conn    (:conn @!state)]
      (if (str/blank? trimmed)
        {:message help-lines}
        (let [space-idx (.indexOf trimmed " ")
              head      (if (neg? space-idx) trimmed (subs trimmed 0 space-idx))
              arg       (-> (if (neg? space-idx) "" (subs trimmed (inc space-idx))) str/trim)
              cmd       (-> head (str/replace #"^/" "") str/lower-case)]
          (if-not (slash-capability? opts cmd)
            {:message [(str "Command /" cmd " is not available on this profile.")]}
            (case cmd
            "tail"     (let [limit (parse-int (first (str/split arg #"\s+")))
                             tail-ctx {:conn conn
                                       :arxana-store (:arxana-store @!state)}
                             relations (svc/tail tail-ctx (or limit 5))
                             fallback (some-> state :last-turn :relations)
                             display (when (seq fallback) fallback)
                             rows (or (seq relations) display [])]
                         {:message (fmt/tail-lines (vec rows))})

            "trails"   {:message (fmt/trails-lines (svc/trails conn (or (parse-int arg) 10)))}

            "ego"      {:message (ego-section conn arg)}
            "cooccur"  {:message (cooccur-section conn arg)}
            "forget"   {:message (forget-section conn opts arg)}
            "expire"   {:message (expire-section conn opts arg)}
            "entity"   {:message (entity-section conn opts arg)}
            "relation" {:message (relation-section conn opts arg)}
            "me"       {:message (me-section conn opts arg)}
            "types"    {:message (types-section conn arg)}
            "help"     {:message help-lines}
            {:message (str "unknown command: /" cmd)})))))))
