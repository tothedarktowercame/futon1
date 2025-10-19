(ns app.command-service
  "Shared command operations used by both the CLI slash handler and the API runtime."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xtcompat]
            [clojure.string :as str]
            [datascript.core :as d]
            [graph-memory.main :as gm]
            [graph-memory.me-profile :as me-profile]
            [graph-memory.types-registry :as types]
            [xtdb.api :as xta])
  (:import [java.util UUID]))

;; -- Relation/Entity helpers -------------------------------------------------

(defn tail
  "Return the most recent relations from the Datascript/XT store."
  ([conn]
   (tail conn 5))
  ([conn limit]
   (try
     (store/recent-relations conn (or limit 5))
     (catch Exception _
       []))))

(defn ego
  "Return outgoing and incoming relations for the named entity.
  Returns nil when the entity does not exist."
  [conn name]
  (when-let [entity (store/resolve-name->eid conn name)]
    (let [neighbors (gm/neighbors conn (:id entity))]
      {:entity entity
       :outgoing (filter #(= :out (:direction %)) neighbors)
       :incoming (filter #(= :in (:direction %)) neighbors)})))

(defn cooccurring
  "Return entities that co-occur with the named entity.
  Returns nil when the entity is not present."
  [conn name]
  (when-let [entity (store/resolve-name->eid conn name)]
    {:entity entity
     :rows (store/cooccurring-entities conn (:id entity))}))

(defn forget-entity!
  "Remove the named entity (and attached relations)."
  [conn opts name]
  (store/forget-entity! conn opts {:name name}))

(defn expire-entity!
  "Reset salience counters for the named entity."
  [conn opts name]
  (store/expire-entity! conn opts {:name name}))

(defn normalize-entity
  "Coerce an entity payload into the canonical {:name ... :type ...} map."
  [value]
  (cond
    (map? value)
    (let [name (some-> (:name value) str/trim not-empty)
          type (:type value)
          id (:id value)]
      (when name
        (cond-> {:name name}
          type (assoc :type type)
          id (assoc :id id))))

    (string? value)
    (let [trimmed (str/trim value)]
      (when (seq trimmed)
        {:name trimmed}))

    :else
    nil))

(defn ensure-entity!
  "Ensure an entity exists for the given profile/connection.

  ctx expects :conn, :env, and optionally :record-anchors! (fn [anchors]).
  Returns {:entity ... :anchors [...]}"
  [{:keys [conn env record-anchors!]} entity-spec]
  (let [spec (normalize-entity entity-spec)]
    (when-not spec
      (throw (ex-info "Entity name required" {:status 400 :entity entity-spec})))
    (let [env-now (assoc env :now (System/currentTimeMillis))
          stored (store/ensure-entity! conn env-now spec)
          anchors [(select-keys stored [:id :name :type :seen-count :last-seen :pinned?])]]
      (when record-anchors!
        (record-anchors! anchors))
      {:entity (first anchors)
       :anchors anchors})))

(defn upsert-relation!
  "Ensure a relation exists between the provided src/dst entities.

  ctx expects :conn, :env, and optionally :record-anchors! (fn [anchors]).
  relation-spec should match the API payload format."
  [{:keys [conn env record-anchors!] :as ctx} relation-spec]
  (when-not (map? relation-spec)
    (throw (ex-info "Relation payload must be an object" {:status 400})))
  (let [type (some-> (:type relation-spec) keyword)
        src-spec (normalize-entity (:src relation-spec))
        dst-spec (normalize-entity (:dst relation-spec))]
    (when-not type
      (throw (ex-info "Relation type required" {:status 400})))
    (when (or (nil? src-spec) (nil? dst-spec))
      (throw (ex-info "Relation requires src and dst" {:status 400})))
    (let [env-now (assoc env :now (System/currentTimeMillis))
          src-entity (:entity (ensure-entity! ctx src-spec))
          dst-entity (:entity (ensure-entity! ctx dst-spec))
          relation (store/upsert-relation! conn env-now
                                           {:type type
                                            :src (select-keys src-entity [:id :name :type])
                                            :dst (select-keys dst-entity [:id :name :type])})
          anchors [src-entity dst-entity]]
      (when record-anchors!
        (record-anchors! anchors))
      {:relation (select-keys relation [:id :type :src :dst :confidence :last-seen])
       :anchors anchors})))

;; -- Profile helpers ---------------------------------------------------------

(defn- safe-int [value]
  (when value
    (try
      (Integer/parseInt (str value))
      (catch Exception _ nil))))

(defn- truthy-param? [value]
  (contains? #{"1" "true" "yes" "on"}
             (some-> value str/lower-case)))

(defn- falsy-param? [value]
  (contains? #{"0" "false" "no" "off"}
             (some-> value str/lower-case)))

(defn- keyword-list [csv]
  (when (and csv (not (str/blank? csv)))
    (->> (str/split csv #",")
         (map str/trim)
         (remove str/blank?)
         (map keyword)
         vec)))

(defn- manual-option-overrides [manual]
  (when (map? manual)
    (let [window (or (safe-int (:window-days manual))
                     (safe-int (:focus-window manual)))
          limit (or (safe-int (:neighbor-limit manual))
                    (safe-int (:hot-limit manual)))
          allow (get manual :allow-works?)
          allowed (:allowed-types manual)]
      (cond-> {}
        window (assoc :window-days window)
        limit (assoc :neighbor-limit limit)
        (contains? manual :allow-works?) (assoc :allow-works? (boolean allow))
        (seq allowed) (assoc :allowed-types (vec (map keyword allowed)))))))

(defn- request-options [{:keys [query-params manual now]}]
  (let [manual-overrides (manual-option-overrides manual)
        preferences (me-profile/preferences-from-manual manual)
        query-window (safe-int (get query-params "window_days"))
        query-limit (safe-int (get query-params "hot_limit"))
        allow-param (get query-params "allow_works")
        allowed-param (get query-params "allowed_types")]
    (merge {:now (or now (System/currentTimeMillis))}
           manual-overrides
           preferences
           (cond-> {}
             query-window (assoc :window-days query-window)
             query-limit (assoc :neighbor-limit query-limit)
             (truthy-param? allow-param) (assoc :allow-works? true)
             (falsy-param? allow-param) (assoc :allow-works? false)
             allowed-param (assoc :allowed-types (keyword-list allowed-param))))))

(defn fetch-profile
  "Return the profile document for the given profile name.

  ctx expects :profile and optionally :query-params.
  The Datascript connection is established via store-manager."
  [{:keys [profile query-params now conn xtdb-node]}]
  (let [profile-name (or profile (store-manager/default-profile))
        conn (or conn (store-manager/conn profile-name))
        manual (store-manager/profile-doc profile-name)
        options (request-options {:query-params (or query-params {})
                                  :manual manual
                                  :now now})
        node (or xtdb-node (xtcompat/node))
        db (xta/db node)
        graph (try
                (me-profile/profile (assoc options :db conn :xt-db db))
                (catch clojure.lang.ExceptionInfo e
                  (let [status (:status (ex-data e))]
                    (when-not (= 404 status)
                      (throw e))
                    nil)))]
    {:profile profile-name
     :data (-> {:generated-at (:now options)
                :relations []
                :topics []}
               (merge (or graph {}))
               (cond-> (seq manual) (assoc :manual manual)))}))

(defn upsert-profile!
  "Shallow merge the provided patch into the profile document."
  [{:keys [profile]} patch]
  (when-not (map? patch)
    (throw (ex-info "Profile payload must be an object" {:status 400})))
  (let [profile-name (or profile (store-manager/default-profile))
        updated (store-manager/upsert-profile! profile-name patch)]
    {:profile profile-name
     :data updated}))

(defn profile-summary
  "Render the profile summary text."
  [{:keys [profile query-params now conn xt-node]} limit]
  (let [node (or xt-node (xtcompat/node))
        db   (xta/db node)
        profile-name (or profile (store-manager/default-profile))
        manual (store-manager/profile-doc profile-name)
        options (request-options {:query-params (or query-params {})
                                  :manual manual
                                  :now now})
        profile-map (or (try
                          (me-profile/profile (assoc options :db conn :xt-db db))
                          (catch clojure.lang.ExceptionInfo e
                            (let [status (:status (ex-data e))]
                              (when-not (= 404 status)
                                (throw e))
                              nil)))
                        {:entity nil
                         :salience {:window {:days (or (:window-days options) 45)
                                             :now (:now options)}}
                         :relations []
                         :topics []
                         :generated-at (:now options)})
        length (or limit 2000)
        text (me-profile/summary profile-map {:manual manual
                                              :limit length})]
    {:profile profile-name
     :text text
     :generated-at (:generated-at profile-map)}))

(defn ^:private limit-from [ctx]
  (let [lim (or (:limit ctx)
                (some-> ctx :query-params :limit))]
    (-> (or (some-> lim str Long/parseLong) 5)
        (max 1) (min 50))))

(defn ^:private time-hint-from [ctx]
  (let [now (:now ctx)]
    (or (:time-hint ctx)
        (when-let [days (some-> ctx :query-params :window-days)]
          (- now (* (Long/parseLong (str days)) 24 60 60 1000)))
        (- now (* 45 24 60 60 1000))))) ; default 45d

;; ---------- helpers ---------------------------------------------------------

(defn- uuid-or-nil [x]
  (cond
    (instance? UUID x) x
    (string? x) (try (UUID/fromString x) (catch Throwable _ nil))
    :else nil))

;; (defn- latest-by-attr-ds
;;   "Return the entity id with the max value of attr in DS."
;;   [db attr]
;;   (when db
;;     (let [q '[:find ?e ?v
;;               :where [?e ?a ?v]
;;               [(= ?a attr)]]
;;           rows (for [[e v] (d/q q db :in '$ ?attr :args [attr])]
;;                  [e v])]
;;       (->> rows
;;            (sort-by (fn [[_ v]] (long (or v 0))) >)
;;            ffirst))))

(defn- first-ds-entity-of-type
  [db types]
  (when db
    (let [q '[:find ?e ?ls
              :in $ [?t ...]
              :where
              [?e :entity/type ?t]
              [(get-else $ ?e :relation/last-seen 0) ?ls]]
          rows (d/q q db types)]
      (some->> rows (sort-by second >) ffirst))))

(defn- latest-relation-src-ds [db]
  (when db
    (let [q '[:find ?src ?ls
              :where
              [?r :relation/src ?src]
              [?r :relation/last-seen ?ls]]
          rows (d/q q db)]
      (some->> rows (sort-by second >) ffirst))))

(defn- xt-api []
  (try (requiring-resolve 'xtdb.api/q) (catch Throwable _ nil)))

(defn- xt-q [xtdb q-map]
  (when-let [qf (xt-api)]
    (try (qf xtdb q-map) (catch Throwable _ nil))))

(defn- xt-entity [xtdb eid]
  (when-let [ef (try (requiring-resolve 'xtdb.api/entity) (catch Throwable _ nil))]
    (try (ef xtdb eid) (catch Throwable _ nil))))

(defn- first-xt-entity-of-type
  [xtdb types]
  (when xtdb
    (let [q {:find  '[e ls]
             :where '[[e :entity/type t]
                      [(get-else e :relation/last-seen 0) ls]]
             :in    '[t ...]}
          rows (xt-q xtdb q)]
      (some->> rows
               (filter (fn [[e _]]
                         (some #{(:entity/type (xt-entity xtdb e))} types)))
               (sort-by second >)
               ffirst))))

(defn- latest-relation-src-xt [xtdb]
  (when xtdb
    (let [q {:find  '[src ls]
             :where '[[r :relation/src src]
                      [r :relation/last-seen ls]]}
          rows (xt-q xtdb q)]
      (some->> rows (sort-by second >) ffirst))))

;; ---------- public ----------------------------------------------------------

(defn resolve-me-id
  "Resolve the UUID of the 'me' entity for this ctx.
   Order:
   1) ctx :me/id
   2) XT mapping on profile: (:profile/id) -> :profile/me-id
   3) DS entity of type :i or :me (most recent)
   4) XT entity of type :i or :me (most recent)
   5) src of most recent relation (DS, then XT)
   Returns UUID or nil."
  [ctx]
  (let [explicit (uuid-or-nil (:me/id ctx))]
    (or explicit
        ;; 2) XT profile mapping
        (let [xtdb (:xt/db ctx)
              pid  (or (:profile/id ctx)
                       (some-> (:profile ctx) str/trim not-empty))]
          (when (and xtdb pid)
            (let [q {:find  '[me-id]
                     :in    '[pid]
                     :where '[[p :profile/id pid]
                              [p :profile/me-id me-id]]}
                  rows (xt-q xtdb q)]
              (some-> rows ffirst uuid-or-nil))))
        ;; 3) DS :i / :me (most recent)
        (some-> (first-ds-entity-of-type (:ds/db ctx) [:i :me]) uuid-or-nil)
        ;; 4) XT :i / :me (most recent)
        (some-> (first-xt-entity-of-type (:xt/db ctx) [:i :me]) uuid-or-nil)
        ;; 5) latest relation src (DS then XT)
        (some-> (latest-relation-src-ds (:ds/db ctx)) uuid-or-nil)
        (some-> (latest-relation-src-xt (:xt/db ctx)) uuid-or-nil))))

;; apps/api/src/api/handlers/me.clj

;; -- Types helpers -----------------------------------------------------------

(defn- type->string [kw]
  (when kw
    (if-let [ns (namespace kw)]
      (str ns "/" (name kw))
      (name kw))))

(defn- doc->response [{:keys [id kind parent alias-of aliases inferred?]}]
  (cond-> {:id (type->string id)
           :kind (name kind)}
    parent (assoc :parent (type->string parent))
    alias-of (assoc :alias_of (type->string alias-of))
    (seq aliases) (assoc :aliases (->> aliases
                                       (map type->string)
                                       (remove nil?)
                                       sort
                                       vec))
    (some? inferred?) (assoc :inferred_parent inferred?)))

(defn- sort-docs [docs kind]
  (->> docs
       (filter #(= kind (:kind %)))
       (sort-by (comp type->string :id))
       (map doc->response)
       vec))

(defn list-types
  "Return the registered entity/relation types grouped by kind."
  [_conn]
  (let [docs (:docs (types/load-cache!))]
    {:types {:entity (sort-docs docs :entity)
             :relation (sort-docs docs :relation)
             :intent (sort-docs docs :intent)}}))

(defn- parse-type [value]
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (if (str/starts-with? trimmed ":")
                          (when (> (count trimmed) 1)
                            (keyword (subs trimmed 1)))
                          (keyword trimmed))))
    :else nil))

(defn- normalize-kind [value]
  (let [raw (some-> value name str/lower-case)]
    (case raw
      ("relation" "relations" "rel") :relation
      ("intent" "intents") :intent
      :entity)))

(defn set-type-parent!
  "Set or clear the parent for the given type."
  [{:keys [type parent kind]}]
  (let [type-kw (parse-type type)
        parent-kw (some-> parent parse-type)
        kind (normalize-kind kind)]
    (when-not type-kw
      (throw (ex-info "Type identifier required" {:status 400})))
    (let [kind (or kind :entity)
          doc (or (types/set-parent! kind type-kw parent-kw)
                  (get-in (types/load-cache!) [:types [kind type-kw]]))]
      (doc->response doc))))

(defn merge-aliases!
  "Merge aliases into the canonical type."
  [{:keys [into aliases kind]}]
  (let [target (parse-type into)
        alias-vec (->> aliases
                       (map parse-type)
                       (remove nil?)
                       vec)
        kind (or (normalize-kind kind) :entity)]
    (when-not target
      (throw (ex-info "Canonical type required" {:status 400})))
    (when (empty? alias-vec)
      (throw (ex-info "At least one alias required" {:status 400})))
    (types/merge! kind target alias-vec)
    (let [doc (or (get-in (types/load-cache!) [:types [kind target]])
                  {:id target
                   :kind kind
                   :aliases (set alias-vec)})]
      (doc->response doc))))
