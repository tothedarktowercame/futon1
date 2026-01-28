(ns api.handlers.turns
  (:require [api.handlers.invariants :as invariants]
            [api.util.http :as http]
            [app.config :as config]
            [app.context :as context]
            [app.focus.header :as focus-header]
            [app.header :as header]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]
            [open-world-ingest.storage :as open-world]
            [protocols.registry :as registry]
            [xtdb.api :as xta]))

;; ----------------------------------------------------------------------------

(def default-protocol "basic-chat/v6")

(defonce ^:private !contexts (atom {}))

(def ^:private default-actor
  {:id :me :name "Me" :type :person})

(def ^:private focus-header-cache-ttl-ms 2000)
(defonce ^:private !focus-header-cache (atom {}))

(defn- open-world-ingest-enabled? []
  (not (false? (:open-world/ingest-on-turns? (config/config)))))

(defn- normalize-actor [value]
  (cond
    (map? value) value
    (keyword? value) {:name (name value)}
    (string? value) {:name value}
    :else nil))

(defn- resolve-actor [request body]
  (let [raw (or (:actor body)
                (get-in request [:headers "x-actor"])
                (get-in request [:headers "x-actor-name"]))
        actor (normalize-actor raw)
        name (some-> (:name actor) str str/trim not-empty)
        actor' (cond-> (merge default-actor actor)
                 name (assoc :name name))]
    (when (and actor name)
      actor')))

(defn- actor->ref [actor]
  (select-keys actor [:id :name :type]))

(defn- pronoun->actor [actor value]
  (let [lower (some-> value str str/lower-case)]
    (when (contains? #{"i" "me" "my" "mine" "myself"} lower)
      (actor->ref actor))))

(defn- fetch-entry [protocol-id]
  (or (registry/fetch protocol-id)
      (throw (ex-info (str "Unknown protocol " protocol-id) {:protocol protocol-id}))))

(defn- ensure-context [profile protocol-id]
  (let [cache-key [profile protocol-id]]
    (or (get @!contexts cache-key)
        (let [entry (fetch-entry protocol-id)
              base ((:init entry))
              configured (if-let [configure (:configure entry)]
                           (configure base {})
                           base)
              pronouns {:me (store-manager/profile-name profile)
                        :you (store-manager/profile-interlocutor-name profile)
                        :we (store-manager/profile-collective-name profile)}
              ctx {:entry entry
                   :state (assoc configured :pronouns pronouns)}]
          (swap! !contexts assoc cache-key ctx)
          ctx))))

(defn warm-profile!
  "Ensure the default protocol context is initialised for profile."
  [profile]
  (try
    (ensure-context profile default-protocol)
    (catch Throwable _
      nil)))

(defn- normalize-text [text]
  (some-> text str str/trim not-empty))

(defn- parse-ts [ts]
  (cond
    (instance? Number ts) (long ts)
    (string? ts) (try
                   (Long/parseLong (str ts))
                   (catch Exception _ nil))
    :else nil))

(defn- entity-spec [entity actor]
  (let [entity-name (normalize-text (:name entity))
        entity-type (:type entity)]
    (when entity-name
      (or (when-let [actor-ref (pronoun->actor actor entity-name)]
            (assoc actor-ref :pinned? true))
          {:name entity-name
           :type entity-type}))))

(defn- ensure-entities! [conn env-now entities actor]
  (reduce (fn [acc entity]
            (if-let [spec (entity-spec entity actor)]
              (let [stored (store/ensure-entity! conn env-now spec)]
                (assoc acc (:name stored) stored))
              acc))
          {}
          entities))

(defn- relation-spec [ensured rel actor]
  (let [relation-type (:type rel)
        src-name (normalize-text (:src rel))
        dst-name (normalize-text (:dst rel))]
    (when (and relation-type src-name dst-name)
      (let [src-ref (or (pronoun->actor actor src-name)
                        (some-> (get ensured src-name)
                                (select-keys [:id :name :type]))
                        {:name src-name})
            dst-ref (or (pronoun->actor actor dst-name)
                        (some-> (get ensured dst-name)
                                (select-keys [:id :name :type]))
                        {:name dst-name})
            final-type (cond
                         (keyword? relation-type) relation-type
                         (string? relation-type) (keyword relation-type)
                         :else relation-type)]
        {:type final-type
         :src src-ref
         :dst dst-ref}))))

(defn- persist-relations! [conn env-now ensured relations actor]
  (->> relations
       (map #(relation-spec ensured % actor))
       (remove nil?)
       (map #(store/upsert-relation! conn env-now %))
       (doall)))

(defn- safe-int [s]
  (when s
    (try
      (Integer/parseInt (str s))
      (catch Exception _ nil))))

(defn- context-options [request]
  (let [query (:query-params request)
        focus-days (safe-int (get query "focus_days"))
        allow-works (some-> (get query "allow_works") str/lower-case)]
    {:neighbors 3
     :context-cap 10
     :context? true
     :focus-days (or focus-days 30)
     :allow-works? (contains? #{"1" "true" "yes" "on"} allow-works)}))

(defn- cache-allowed? [request]
  (let [raw (some-> (or (get-in request [:query-params "cache"])
                        (get-in request [:query-params :cache]))
                    str/lower-case)]
    (not (contains? #{"0" "false" "no" "off"} raw))))

;; --- REPLACED: robust process-turn! with open-world fallback ----------------

(defn- safe-db
  "Return an XTDB db snapshot for node. If node is nil or closing, reacquire."
  [node]
  (let [node' (or node (xt/node))]
    (try
      (xta/db node')
      (catch IllegalStateException _
        (xta/db (xt/node))))))

(defn process-turn!
  ([request body]
   (process-turn! request body (resolve-actor request body)))
  ([request body actor]
   (let [{:keys [ctx]} request
         profile (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
                     (:default-profile ctx))
         protocol-id (or (some-> (:protocol body) str/trim not-empty)
                         default-protocol)
         turn-ctx (ensure-context profile protocol-id)
         text (normalize-text (:text body))]
     (when-not text
       (throw (ex-info "Missing text" {:status 400})))
     (let [ts        (or (parse-ts (:ts body)) (System/currentTimeMillis))
           {:keys [state entry]} turn-ctx
           handler   (:handle entry)
           conn      (store-manager/conn profile)
           env-now   (assoc (merge (store-manager/env profile) (:env ctx))
                            :now ts)
           options   (context-options request)
           xt-node   (:xtdb-node ctx)
           ;; --- TAKE ONE SNAPSHOT SAFELY AND REUSE IT ---
           xt-db     (safe-db xt-node)
           actor     (or actor
                         (throw (ex-info "actor required" {:status 400})))

           actor-entity (store/ensure-entity! conn env-now actor)
           actor-ref (actor->ref (merge actor actor-entity))
           result    (handler state text ts)
           _         (println (format "--- TURNS: Extracted %d entities, %d relations"
                                      (count (:entities result))
                                      (count (:relations result))))
           ensured   (ensure-entities! conn env-now (:entities result) actor-ref)
           rels      (persist-relations! conn env-now ensured (:relations result) actor-ref)
           open-world-result (when (and (open-world-ingest-enabled?)
                                        xt-node
                                        (map? (:open-world result)))
                               (open-world/store-analysis-with-node!
                                xt-node
                                text
                                (assoc (:open-world result)
                                       :ego-id (:id actor-ref)
                                       :actor-id (:id actor-ref)
                                       :actor-name (:name actor-ref)
                                       :actor-type (:type actor-ref)
                                       :conn conn
                                       :guard-opts env-now)))
           anchors   (->> ensured vals (remove nil?) vec)
           _         (store-manager/record-anchors! profile anchors)

           ;; context: pass xt-db (snapshot)
           context-lines (context/enrich-with-neighbors
                          xt-db conn (:entities result)
                          (assoc options
                                 :anchors anchors
                                 :timestamp ts))

           focus-debug (try
                         (focus-header/build conn
                                             {:entities (-> ensured vals vec)
                                              :relations rels}
                                             context-lines
                                             {:recent-limit (:context-cap options)})
                         (catch Throwable _ nil))

           ;; focus header: also pass xt-db (snapshot)
           fh (header/focus-header
               xt-db
               {:anchors anchors
                :intent (:intent result)
                :time ts
                :turn-id ts
                :policy {:focus-days  (:focus-days options)
                         :allow-works? (:allow-works? options)}
                :focus-limit 10
                :conn conn})]
       {:turn_id ts
       :entities  (->> ensured vals (map #(select-keys % [:id :name :type :seen-count :last-seen :pinned?])) vec)
       :relations (mapv #(select-keys % [:id :type :src :dst :confidence :last-seen]) rels)
       :intent    (:intent result)
        :open-world-ingest open-world-result
       :focus_header fh
        :focus_header_debug focus-debug
        :context   context-lines}))))

;; ----------------------------------------------------------------------------

(defn current-focus-header
  [request]
  (let [{:keys [ctx]} request
        profile (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
                    (:default-profile ctx))
        options (context-options request)
        cache-key [profile (:focus-days options) (:allow-works? options)]
        now (System/currentTimeMillis)
        cached (when (cache-allowed? request)
                 (get @!focus-header-cache cache-key))]
    (if (and cached (< (- now (:ts cached)) focus-header-cache-ttl-ms))
      (:resp cached)
      (let [anchors (store-manager/current-anchors profile)
            xt-db (safe-db (:xtdb-node ctx))
            fh (header/focus-header xt-db {:anchors anchors
                                           :time now
                                           :turn-id now
                                           :policy {:focus-days (:focus-days options)
                                                    :allow-works? (:allow-works? options)}
                                           :focus-limit 10
                                           :conn (store-manager/conn profile)})
            resp {:profile profile
                  :focus_header fh}]
        (when (cache-allowed? request)
          (swap! !focus-header-cache assoc cache-key {:ts now :resp resp}))
        resp))))

(defn process-turn-handler [request]
  (let [{:keys [ctx]} request
        body (:body request)
        profile (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
                    (:default-profile ctx))
        protocol-id (or (some-> (:protocol body) str/trim not-empty)
                        default-protocol)
        text (normalize-text (:text body))
        start (System/nanoTime)
        preview (when text (if (> (count text) 120)
                             (str (subs text 0 117) "…")
                           text))
        actor (resolve-actor request body)]
    (println (format "TURN POST start profile=%s protocol=%s chars=%s preview=%s"
                     profile protocol-id (or (some-> text count) 0) (or preview "<empty>")))
    (if (nil? actor)
      (let [elapsed (/ (- (System/nanoTime) start) 1e6)]
        (println (format "TURN POST invalid profile=%s %.1fms actor required"
                         profile elapsed))
        (http/ok-json {:error "actor required"
                       :hint "Include :actor in the JSON body or X-Actor-Name header."}
                      400))
      (try
        (let [resp (process-turn! request body actor)
              elapsed (/ (- (System/nanoTime) start) 1e6)
              entity-count (count (:entities resp))
              relation-count (count (:relations resp))
              verification (invariants/maybe-verify-core profile)
              open-world (get resp :open-world-ingest)]
          (println (format "TURN POST ok profile=%s %.1fms entities=%d relations=%d"
                           profile elapsed entity-count relation-count))
          (cond
            (and verification (not (:ok? verification)))
            (http/ok-json {:error "Model invariants failed"
                           :profile profile
                           :invariants verification
                           :result resp}
                          409)

            (and open-world (false? (:ok? open-world)))
            (http/ok-json {:error "Open-world ingest failed"
                           :profile profile
                           :open-world open-world
                           :result resp}
                          409)

            :else
            (http/ok-json (cond-> resp
                            verification (assoc :invariants verification)))))
        (catch Throwable ex
          (let [elapsed (/ (- (System/nanoTime) start) 1e6)]
            (println (format "TURN POST error profile=%s %.1fms %s"
                             profile elapsed (.getMessage ex)))
            (throw ex)))))))

(defn current-focus-header-handler [request]
  (http/ok-json (current-focus-header request)))
