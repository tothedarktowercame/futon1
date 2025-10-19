(ns api.handlers.turns
  (:require [api.util.http :as http]
            [app.context :as context]
            [app.header :as header]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]
            [protocols.registry :as registry]
            [xtdb.api :as xta]))

;; ----------------------------------------------------------------------------

(def default-protocol "basic-chat/v6")

(defonce ^:private !contexts (atom {}))

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

(defn- entity-spec [entity]
  (let [entity-name (normalize-text (:name entity))
        entity-type (:type entity)]
    (when entity-name
      {:name entity-name
       :type entity-type})))

(defn- ensure-entities! [conn env-now entities]
  (reduce (fn [acc entity]
            (if-let [spec (entity-spec entity)]
              (let [stored (store/ensure-entity! conn env-now spec)]
                (assoc acc (:name stored) stored))
              acc))
          {}
          entities))

(defn- relation-spec [ensured rel]
  (let [relation-type (:type rel)
        src-name (normalize-text (:src rel))
        dst-name (normalize-text (:dst rel))]
    (when (and relation-type src-name dst-name)
      (let [src-ref (or (some-> (get ensured src-name)
                                (select-keys [:id :name :type]))
                        {:name src-name})
            dst-ref (or (some-> (get ensured dst-name)
                                (select-keys [:id :name :type]))
                        {:name dst-name})]
        {:type relation-type
         :src src-ref
         :dst dst-ref}))))

(defn- persist-relations! [conn env-now ensured relations]
  (->> relations
       (map #(relation-spec ensured %))
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
  [request body]
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
          env-now   (assoc (:env ctx) :now ts)
          options   (context-options request)
          xt-node   (:xtdb-node ctx)
          ;; --- TAKE ONE SNAPSHOT SAFELY AND REUSE IT ---
          xt-db     (safe-db xt-node)

          result    (handler state text ts)
          _         (println (format "--- TURNS: Extracted %d entities, %d relations"
                                     (count (:entities result))
                                     (count (:relations result))))
          ensured   (ensure-entities! conn env-now (:entities result))
          rels      (persist-relations! conn env-now ensured (:relations result))
          anchors   (->> ensured vals (remove nil?) vec)
          _         (store-manager/record-anchors! profile anchors)

          ;; context: pass xt-db (snapshot)
          context-lines (context/enrich-with-neighbors
                         xt-db conn (:entities result)
                         (assoc options
                                :anchors anchors
                                :timestamp ts))

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
      :focus_header fh
       :context   context-lines})))

;; ----------------------------------------------------------------------------

(defn current-focus-header
  [request]
  (let [{:keys [ctx]} request
        profile (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
                    (:default-profile ctx))
        options (context-options request)
        anchors (store-manager/current-anchors profile)
        now (System/currentTimeMillis)
        fh (header/focus-header (:xtdb-node ctx) {:anchors anchors
                                                  :time now
                                                  :turn-id now
                                                  :policy {:focus-days (:focus-days options)
                                                           :allow-works? (:allow-works? options)}
                                                  :focus-limit 10
                                                  :conn (store-manager/conn profile)})]
    {:profile profile
     :focus_header fh}))

(defn process-turn-handler [request]
  (http/ok-json (process-turn! request (:body request))))

(defn current-focus-header-handler [request]
  (http/ok-json (current-focus-header request)))
