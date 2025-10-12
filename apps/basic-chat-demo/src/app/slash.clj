(ns app.slash
  (:require [app.command-service :as svc]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import (java.time Instant)))

(def ^:private help-lines
  ["Slash commands:"
   "  /tail [n]      Show the last n relations (default 5)"
   "  /ego NAME      Display neighbors connected to the named entity"
   "  /cooccur NAME  Show entities that co-occur with the named entity"
   "  /forget NAME   Remove the named entity and attached relations"
   "  /expire NAME   Reset salience counters for the named entity"
   "  /entity NAME [TYPE]   Ensure an entity exists (optionally set type)"
   "  /relation TYPE SRC DST   Upsert a relation between SRC and DST"
   "  /me            Show the current profile document"
   "  /me summary [limit]  Render the profile summary (default 2000 chars)"
   "  /types         List registered entity/relation/intent types"
   "  /types parent TYPE [PARENT] [KIND]  Override a type's parent"
   "  /types merge INTO ALIAS...  Merge aliases into a canonical type"
   "  /help          Show this help message"])

(defn- type-label [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    (nil? t) nil
    :else (str t)))

(defn- name-with-type [name type]
  (if (and name type)
    (str name " (" (type-label type) ")")
    name))

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
      [(or (ex-message e) "command failed")])
    (catch Exception e
      [(or (.getMessage e) "command failed")])))

(defn- entity-lines [{:keys [name type seen-count last-seen pinned?]}]
  (let [header (str "Entity ensured: " (name-with-type name type))
        seen-line (when (some? seen-count)
                    (str "  - seen-count → " seen-count))
        last-line (when (some? last-seen)
                    (str "  - last-seen → " last-seen))
        pin-line (when (some? pinned?)
                   (str "  - pinned? → " pinned?))]
    (->> [header seen-line last-line pin-line]
         (remove nil?))))

(defn- relation-lines [{:keys [type src dst confidence last-seen]}]
  (let [src-name (name-with-type (get-in src [:name]) (get-in src [:type]))
        dst-name (name-with-type (get-in dst [:name]) (get-in dst [:type]))
        base (str "Relation ensured: " src-name " —" (type-label type) "→ " dst-name)
        conf-line (when (some? confidence)
                    (format "  - confidence → %.2f" (double confidence)))
        last-line (when (some? last-seen)
                    (str "  - last-seen → " last-seen))]
    (->> [base conf-line last-line]
         (remove nil?))))

(defn- pprint-lines [value]
  (let [rendered (with-out-str (pprint/pprint value))]
    (->> (str/split rendered #"\n")
         (map str/trimr)
         (remove str/blank?))))

(defn- profile-lines [{:keys [profile data]}]
  (into [(str "Profile: " profile)] (pprint-lines data)))

(defn- profile-summary-lines [{:keys [profile text generated-at]}]
  [(str "Profile: " profile)
   (str "Generated at: " generated-at)
   ""
   text])

(defn- types-lines [{:keys [types]}]
  (if (seq types)
    (into ["Registered types:"]
          (for [{:keys [id kind parent aliases alias_of inferred_parent]} types]
            (str "  - " id
                 " [" kind "]"
                 (when parent (str " parent=" parent))
                 (when alias_of (str " alias-of=" alias_of))
                 (when (seq aliases) (str " aliases=" (pr-str aliases)))
                 (when (some? inferred_parent) (str " inferred-parent=" inferred_parent)))))
    ["Registered types:" "  (none)"]))

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
          (entity-lines entity))))))

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
          (relation-lines relation))))))

(defn- me-section [arg]
  (let [[sub & rest] (->> (str/split arg #"\s+")
                          (remove str/blank?))
        sub (some-> sub str/lower-case)]
    (case sub
      "summary" (let [limit (some-> (first rest) parse-int)]
                   (safe-exec
                    #(profile-summary-lines
                      (svc/profile-summary {:profile nil :query-params {} :now (System/currentTimeMillis)}
                                            limit))))
      "set" (let [payload-str (str/join " " rest)
                   payload (parse-edn-safe payload-str)]
               (if (map? payload)
                 (safe-exec #(profile-lines (svc/upsert-profile! {:profile nil} payload)))
                 ["Usage: /me set {<edn map>}"]))
      ;; default -> show profile doc
      (safe-exec
       #(profile-lines (svc/fetch-profile {:profile nil
                                           :query-params {}
                                           :now (System/currentTimeMillis)}))))))

(defn- types-section [arg]
  (let [[sub & rest] (->> (str/split arg #"\s+")
                          (remove str/blank?))
        sub (some-> sub str/lower-case)]
    (case sub
      "parent" (let [[type parent kind & _] rest
                      body {:type type
                            :parent parent
                            :kind kind}]
                  (safe-exec #(types-lines {:types [(svc/set-type-parent! body)]})))
      "merge" (let [[into & aliases] rest
                     body {:into into :aliases aliases}]
                 (safe-exec #(types-lines {:types [(svc/merge-aliases! body)]})))
      ;; default -> list all types
      (safe-exec #(types-lines (svc/list-types))))))

(defn- format-timestamp [ts]
  (when (number? ts)
    (try
      (.toString (Instant/ofEpochMilli (long ts)))
      (catch Exception _ nil))))

(defn- relation-label [t]
  (or (type-label t) "?"))

(defn- format-tail-line [{:keys [src dst type confidence last-seen]}]
  (let [src-name (or (:name src) "?")
        dst-name (or (:name dst) "?")
        relation (relation-label type)
        extras (->> [(when confidence (format "conf %.2f" (double confidence)))
                     (when-let [ts (format-timestamp last-seen)] (str "seen " ts))]
                    (remove nil?)
                    (str/join ", "))]
    (str "  - " src-name " —" relation "→ " dst-name
         (when (seq extras)
           (str " (" extras ")")))))

(defn- format-ego-line [{:keys [relation direction entity]} focal]
  (let [relation (relation-label relation)
        neighbor-name (name-with-type (:entity/name entity) (:entity/type entity))
        focus-name (name-with-type (:name focal) (:type focal))]
    (if (= direction :out)
      (str "  - " focus-name " —" relation "→ " neighbor-name)
      (str "  - " neighbor-name " —" relation "→ " focus-name))))

(defn- format-removed-relation [{:keys [type src dst confidence]}]
  (let [relation (relation-label type)
        src-name (name-with-type (:name src) (:type src))
        dst-name (name-with-type (:name dst) (:type dst))]
    (str "  - " src-name " —" relation "→ " dst-name
         (when (number? confidence)
           (str " (conf " (format "%.2f" (double confidence)) ")")))))

(defn- removed-relations-block [relations]
  (if (seq relations)
    (into ["Detached relations:"] (map format-removed-relation relations))
    ["Detached relations:" "  (none)"]))

(defn- tail-section [relations]
  (if (seq relations)
    (into ["Recent relations:"] (map format-tail-line relations))
    ["Recent relations:" "  (none)"]))

(defn- ego-section [conn name]
  (if (str/blank? name)
    ["Usage: /ego <entity>"]
    (if-let [{:keys [entity outgoing incoming]} (svc/ego conn name)]
      (let [header (str "Entity: " (name-with-type (:name entity) (:type entity)))
            out-lines (if (seq outgoing)
                        (into ["Outgoing:"] (map #(format-ego-line % entity) outgoing))
                        ["Outgoing:" "  (none)"])
            in-lines (if (seq incoming)
                       (into ["Incoming:"] (map #(format-ego-line % entity) incoming))
                       ["Incoming:" "  (none)"])]
        (into [header] (concat out-lines in-lines)))
      [(str "entity not found: " name)])))

(defn- cooccur-section [conn name]
  (if (str/blank? name)
    ["Usage: /cooccur <entity>"]
    (if-let [{:keys [entity rows]} (svc/cooccurring conn name)]
      (let [header (str "Co-occurrences with " (:name entity) ":")
            lines (if (seq rows)
                    (map (fn [{:keys [name type count]}]
                           (str "  - " (name-with-type name type) " (" count ")"))
                         rows)
                    ["  (none)"])]
        (into [header] lines))
      [(str "entity not found: " name)])))

(defn- forget-section [conn opts name]
  (if (str/blank? name)
    ["Usage: /forget <entity>"]
    (if-let [{:keys [entity relations]} (svc/forget-entity! conn opts name)]
      (let [label (name-with-type (:name entity) (:type entity))]
        (into [(str "Removed " (or label name))]
              (removed-relations-block relations)))
      [(str "entity not found: " name)])))

(defn- expire-section [conn opts name]
  (if (str/blank? name)
    ["Usage: /expire <entity>"]
    (if-let [entity (svc/expire-entity! conn opts name)]
      (let [label (name-with-type (:name entity) (:type entity))
            seen (or (:seen-count entity) 0)
            last (long (or (:last-seen entity) 0))
            last-line (if (pos? last)
                        (if-let [ts (format-timestamp last)]
                          (str "  - last-seen → " ts)
                          (str "  - last-seen → " last))
                        "  - last-seen reset")
            pin (:pinned? entity)
            pin-line (when (some? pin)
                       (str "  - pinned? → " pin))]
        (cond-> [(str "Expired salience for " (or label name))
                 (str "  - seen-count → " seen)
                 last-line]
          pin-line (conj pin-line)))
      [(str "entity not found: " name)])))

(defn handler
  "Return a slash-command handler bound to the Datascript connection."
  [conn opts]
  (fn [raw state]
    (let [trimmed (str/trim raw)]
      (if (str/blank? trimmed)
        {:message help-lines :new-state state}
        (let [[cmd & _] (str/split trimmed #"\s+" 2)
              cmd (str/lower-case cmd)
              space-idx (.indexOf trimmed " ")
              arg (if (neg? space-idx) "" (subs trimmed (inc space-idx)))
              arg (str/trim arg)]
          (case cmd
            "tail" (let [limit (parse-int (first (str/split arg #"\s+")))
                         relations (svc/tail conn (or limit 5))]
                     {:message (tail-section relations)
                      :new-state state})
            "ego" {:message (ego-section conn arg)
                   :new-state state}
            "cooccur" {:message (cooccur-section conn arg)
                        :new-state state}
            "forget" {:message (forget-section conn opts arg)
                       :new-state state}
            "expire" {:message (expire-section conn opts arg)
                       :new-state state}
            "entity" {:message (entity-section conn opts arg)
                       :new-state state}
            "relation" {:message (relation-section conn opts arg)
                         :new-state state}
            "me" {:message (me-section arg)
                   :new-state state}
            "types" {:message (types-section arg)
                      :new-state state}
            "help" {:message help-lines
                     :new-state state}
            {:message (str "unknown command: /" cmd)
             :new-state state}))))))
