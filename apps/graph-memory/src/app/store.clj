(ns app.store
  "Persistent Datascript-backed event store with append-only journal and snapshots."
  (:require [app.xt :as xt]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [graph-memory.main :as gm]
            [graph-memory.types-registry :as types]
            [futon1.id :as fid]
            [xtdb.api :as xtdb])
  (:import (java.io PushbackReader)
           (java.time Instant)
           (java.util UUID)))

(defmethod print-dup Instant [^Instant inst ^java.io.Writer w]
  (.write w (str "#inst \"" (.toString inst) "\"")))

(def schema
  "Datascript schema used by the runtime store."
  gm/schema)

(def ^:private events-filename "events.ndjson")
(def ^:private snapshot-filename "snapshot.edn")

;; Keeps track of pending event counts per data directory to trigger compaction.
(defonce ^:private !event-count (atom {}))

(def ^:private trace-relations?
  (boolean (some-> (System/getenv "TRACE_RELATIONS") str/trim not-empty)))

(defn- trace-short-entity [entity]
  (when (map? entity)
    (select-keys entity [:id :name :type :entity/id :entity/name :entity/type])))

(defn- trace-relation [stage payload]
  (when trace-relations?
    (let [data (if (map? payload)
                 (-> payload
                     (update :src trace-short-entity)
                     (update :dst trace-short-entity)
                     (select-keys [:id :relation/id :type :relation/type :src :dst :provenance :confidence :props]))
                 payload)]
      (println (format "[trace relation %s] %s" stage (pr-str data))))))

(declare apply-event! coerce-double coerce-long entity-by-id entity-by-name entity->public relation->public normalize-type)

(defn- ensure-dir!
  [dir]
  (doto (io/file dir)
    (.mkdirs)))

(defn- events-path [dir]
  (io/file dir events-filename))

(defn- snapshot-path [dir]
  (io/file dir snapshot-filename))

(defn- write-json-line! [^java.io.Writer w data]
  (.write w (json/write-str {:edn (pr-str data)}))
  (.write w "\n"))

(defn- append-event!
  [dir event]
  (let [file (events-path dir)]
    (ensure-dir! dir)
    (with-open [w (io/writer file :append true)]
      (write-json-line! w event))))

(defn- read-events
  [dir]
  (let [file (events-path dir)]
    (if (.exists file)
      (with-open [r (io/reader file)]
        (->> (line-seq r)
             (keep (fn [line]
                     (let [line' (str/trim line)]
                       (when (seq line')
                         (let [{:keys [edn]} (json/read-str line' :key-fn keyword)]
                           (when edn
                             (binding [*read-eval* false
                                       *data-readers* (merge *data-readers* d/data-readers)]
                               (edn/read-string edn))))))))
             doall))
      [])))

(defn- load-snapshot
  [dir]
  (let [file (snapshot-path dir)]
    (when (.exists file)
      (try
        (with-open [r (io/reader file)]
          (binding [*read-eval* false
                    *data-readers* (merge *data-readers* d/data-readers)]
            (edn/read (PushbackReader. r))))
        (catch Exception _
          nil)))))

(defn- ->absolute-path [v]
  (when v
    (-> (io/file v) .getAbsolutePath)))

(defn- xt-config-path [{:keys [config-path resource]}]
  (or (->absolute-path config-path)
      (some-> resource io/resource ->absolute-path)
      (some-> (io/resource "xtdb.edn") ->absolute-path)))

(defn- xt-enabled?
  [xtdb-opts]
  (get xtdb-opts :enabled? true))

(defn- healthy-node [node]
  (when node
    (try
      ;; touch the DB to ensure the executor is alive
      (xtdb/db node)
      node
      (catch Throwable _
        nil))))

(defn- ensure-xt-node!
  [xtdb-opts default-data-dir]
  (when (xt-enabled? xtdb-opts)
    (let [cfg (xt-config-path xtdb-opts)]
      (when-not cfg
        (throw (ex-info "XTDB config not found" {:opts xtdb-opts})))
      (let [resolved-data-dir (or (:data-dir xtdb-opts)
                                  (when default-data-dir
                                    (.getAbsolutePath (io/file default-data-dir "xtdb"))))
            start-opts (cond-> {:xt/created-by "app.store/ensure-xt-node!"}
                         resolved-data-dir (assoc :data-dir (->absolute-path resolved-data-dir)))
            existing (healthy-node (when (xt/started?) (xt/node)))]
        (when-not existing
          (when (xt/started?)
            (xt/stop!))
          (xt/start! cfg start-opts))))))

(defn- xt-entity->tx
  [doc]
  (when-let [id (:entity/id doc)]
    (let [name (:entity/name doc)
          type (:entity/type doc)
          last-seen (:entity/last-seen doc)
          seen-count (:entity/seen-count doc)
          pinned? (contains? doc :entity/pinned?)
        external-id (:entity/external-id doc)
        source (:entity/source doc)
        sha (:media/sha256 doc)]
      (when (or (some? name)
                (some? type)
                (some? last-seen)
                (some? seen-count)
                pinned?
                (some? external-id)
                (some? source)
                (some? sha))
        (cond-> {:entity/id id}
          (some? name) (assoc :entity/name name)
          (some? type) (assoc :entity/type type)
          (some? last-seen) (assoc :entity/last-seen last-seen)
          (some? seen-count) (assoc :entity/seen-count seen-count)
          pinned? (assoc :entity/pinned? (:entity/pinned? doc))
          (some? external-id) (assoc :entity/external-id external-id)
          (some? source) (assoc :entity/source source)
          (some? sha) (assoc :media/sha256 sha))))))

(defn- xt-relation->tx
  [doc]
  (let [id (:relation/id doc)
        src (:relation/src doc)
        dst (:relation/dst doc)
        prov (:relation/provenance doc)
        props (:relation/props doc)]
    (when (and id src dst)
      (let [src-ref [:entity/id src]
            dst-ref [:entity/id dst]]
        (cond-> {:relation/id id
                 :relation/type (:relation/type doc)
                 :relation/src src-ref
                 :relation/dst dst-ref}
          prov (assoc :relation/provenance prov)
          props (assoc :relation/props props)
          (:relation/last-seen doc) (assoc :relation/last-seen (:relation/last-seen doc))
          (:relation/confidence doc) (assoc :relation/confidence (:relation/confidence doc)))))))

(defn- log-skip!
  [label {:keys [count ids]}]
  (when (pos? count)
    (binding [*out* *err*]
      (println (format "[store] skipped %s hydration for %d item(s)%s"
                       label
                       count
                       (if (seq ids)
                         (str ": " (str/join ", " ids))
                         ""))))))

(defn hydrate-from-xtdb!
  [conn]
  (let [entity-docs (->> (xt/q '[:find (pull ?e [*])
                                 :where
                                 [?e :entity/id _]])
                         (map first))
        rel-docs    (->> (xt/q '[:find (pull ?r [*])
                                 :where
                                 [?r :relation/id _]])
                         (map first))
        entity-txs  (->> entity-docs
                         (map xt-entity->tx)
                         (remove (comp nil? :entity/id))
                         (vec))
        known-ids   (atom #{})
        stubbed!    (atom #{})
        entity-count (atom 0)]
    (doseq [tx entity-txs]
      (let [eid (:entity/id tx)
            tx-data (cond-> tx
                      (nil? (:entity/name tx)) (dissoc :entity/name))]
        (try
          (d/transact! conn [tx-data])
          (swap! known-ids conj eid)
          (swap! entity-count inc)
          (catch Exception _
            (let [stub {:entity/id eid}]
              (d/transact! conn [stub])
              (swap! known-ids conj eid)
              (swap! stubbed! conj eid)
              (swap! entity-count inc))))))
        (letfn [(present? [eid]
              (when eid
                (if (@known-ids eid)
                  (if (some? (d/pull @conn [:entity/id] [:entity/id eid]))
                    true
                    (do
                      (swap! known-ids disj eid)
                      false))
                  (let [found? (some? (d/pull @conn [:entity/id] [:entity/id eid]))]
                    (when found?
                      (swap! known-ids conj eid))
                    found?))))
            (ensure-entity! [eid]
              (when (and eid (not (present? eid)))
                (when-let [doc (xt/entity eid)]
                  (let [tx (or (xt-entity->tx doc) {:entity/id eid})
                        tx-data (cond-> tx
                                  (nil? (:entity/name tx)) (dissoc :entity/name))]
                    (try
                      (d/transact! conn [tx-data])
                      (catch Exception _
                        ;; Fall back to a minimal stub to keep relation hydration stable.
                        (d/transact! conn [{:entity/id eid}]))))
                  (swap! known-ids conj eid))))]
      (let [relation-count (atom 0)
            skipped (atom [])]
        (doseq [doc rel-docs]
          (let [src (:relation/src doc)
                dst (:relation/dst doc)]
            (ensure-entity! src)
            (ensure-entity! dst)
            (if (and (present? src)
                     (present? dst))
              (if-let [tx (xt-relation->tx doc)]
                (try
                  (d/transact! conn [tx])
                  (swap! relation-count inc)
                  (catch Exception err
                    (swap! skipped conj (:relation/id doc))
                    (binding [*out* *err*]
                      (println (format "[store] skipped relation %s (%s)"
                                       (:relation/id doc)
                                       (ex-message err))))))
                (swap! skipped conj (:relation/id doc)))
              (swap! skipped conj (:relation/id doc)))))
        (let [missing (remove nil? @skipped)
              stubbed (seq @stubbed!)]
          (log-skip! "relation" {:count (count missing)
                                 :ids missing})
          (log-skip! "entity" {:count (count stubbed)
                               :ids stubbed}))
        {:entity-count @entity-count
         :relation-count @relation-count}))))

(defn- hydrate-from-legacy!
  [conn data-dir]
  (ensure-dir! data-dir)
  (let [snapshot (load-snapshot data-dir)
        events (read-events data-dir)]
    (when snapshot
      (if (:datoms snapshot)
        (do
          (reset! conn (d/empty-db schema))
          (d/transact! conn (:datoms snapshot)))
        (reset! conn snapshot)))
    (doseq [event events]
      (apply-event! conn event))
    {:event-count (count events)
     :snapshot? (boolean snapshot)
     :has-data? (or snapshot (seq events))}))

(defn- ds-entities
  [conn]
  (map first (d/q '[:find (pull ?e [:entity/id :entity/name :entity/type
                                    :entity/last-seen :entity/seen-count :entity/pinned?
                                    :entity/external-id :entity/source :media/sha256])
                    :where
                    [?e :entity/id _]]
                  @conn)))

(defn- ds-relations
  [conn]
  (map first (d/q '[:find (pull ?r [:relation/id
                                    :relation/type
                                    {:relation/src [:entity/id]}
                                    {:relation/dst [:entity/id]}
                                    :relation/provenance
                                    :relation/props
                                    :relation/last-seen
                                    :relation/confidence])
                    :where
                    [?r :relation/id _]]
                  @conn)))

(defn- ds-entity->xt-doc
  [entity]
  (when-let [id (:entity/id entity)]
    (cond-> {:xt/id id
             :entity/id id
             :entity/name (:entity/name entity)}
      (:entity/type entity) (assoc :entity/type (:entity/type entity))
      (:entity/last-seen entity) (assoc :entity/last-seen (:entity/last-seen entity))
      (:entity/seen-count entity) (assoc :entity/seen-count (:entity/seen-count entity))
      (contains? entity :entity/pinned?) (assoc :entity/pinned? (boolean (:entity/pinned? entity)))
      (:entity/external-id entity) (assoc :entity/external-id (:entity/external-id entity))
      (:entity/source entity) (assoc :entity/source (:entity/source entity))
      (:media/sha256 entity) (assoc :media/sha256 (:media/sha256 entity)))))

(defn- ds-relation->xt-doc
  [relation]
  (let [id (:relation/id relation)
        src-id (get-in relation [:relation/src :entity/id])
        dst-id (get-in relation [:relation/dst :entity/id])
        prov (:relation/provenance relation)
        props (:relation/props relation)]
    (when (and id src-id dst-id)
      (cond-> {:xt/id id
               :relation/id id
               :relation/type (:relation/type relation)
               :relation/src src-id
               :relation/dst dst-id}
        prov (assoc :relation/provenance prov)
        props (assoc :relation/props props)
        (:relation/last-seen relation) (assoc :relation/last-seen (:relation/last-seen relation))
        (:relation/confidence relation) (assoc :relation/confidence (:relation/confidence relation))))))

(defn- sync-to-xtdb!
  [conn]
  (let [entity-docs (->> (ds-entities conn) (map ds-entity->xt-doc) (remove nil?))
        rel-docs    (->> (ds-relations conn) (map ds-relation->xt-doc) (remove nil?))
        ops         (vec (concat (map (fn [doc] [::xtdb/put doc]) entity-docs)
                                 (map (fn [doc] [::xtdb/put doc]) rel-docs)))]
    (when (seq ops)
      (xt/submit! ops))
    {:entity-count (count entity-docs)
     :relation-count (count rel-docs)}))

(defn- entity->xt-doc
  [entity]
  (let [id (or (:entity/id entity) (:id entity))
        name (or (:entity/name entity) (:name entity))
        type (or (:entity/type entity) (:type entity))]
    (when (and id name)
      (cond-> {:xt/id id
               :entity/id id
               :entity/name name}
        type (assoc :entity/type (normalize-type type))
        (:last-seen entity) (assoc :entity/last-seen (:last-seen entity))
        (:seen-count entity) (assoc :entity/seen-count (:seen-count entity))
        (contains? entity :pinned?) (assoc :entity/pinned? (boolean (:pinned? entity)))
        (:external-id entity) (assoc :entity/external-id (:external-id entity))
        (:entity/external-id entity) (assoc :entity/external-id (:entity/external-id entity))
        (:source entity) (assoc :entity/source (:source entity))
        (:entity/source entity) (assoc :entity/source (:entity/source entity))
        (:media/sha256 entity) (assoc :media/sha256 (:media/sha256 entity))
        (:entity/media-sha256 entity) (assoc :media/sha256 (:entity/media-sha256 entity))))))

(defn- relation->xt-doc
  [relation]
  (let [id (or (:relation/id relation) (:id relation))
        type (or (:relation/type relation) (:type relation))
        src (or (:relation/src relation) (:src relation))
        dst (or (:relation/dst relation) (:dst relation))
        src-id (or (:entity/id src) (:id src))
        dst-id (or (:entity/id dst) (:id dst))
        provenance (or (:relation/provenance relation) (:provenance relation))
        props (or (:relation/props relation) (:props relation))
        confidence (or (coerce-double (:relation/confidence relation))
                       (coerce-double (:confidence relation)))
        last-seen (or (coerce-long (:relation/last-seen relation))
                      (coerce-long (:last-seen relation)))]
    (when (and id type src-id dst-id)
      (cond-> {:xt/id id
               :relation/id id
               :relation/type type
               :relation/src src-id
               :relation/dst dst-id}
        provenance (assoc :relation/provenance provenance)
        props (assoc :relation/props props)
        last-seen (assoc :relation/last-seen last-seen)
        confidence (assoc :relation/confidence confidence)))))

(defn- log-mirror-error! [label ex]
  (binding [*out* *err*]
    (println (format "[xtdb-mirror] %s failed: %s" label (.getMessage ex)))))

(defn- maybe-mirror-entity!
  [opts entity]
  (let [xtdb-opts (:xtdb opts)]
    (when (and (xt-enabled? xtdb-opts) (xt/started?))
      (when-let [doc (entity->xt-doc entity)]
        (try
          (xt/put-entity-async! doc)
          (catch Exception ex
            (log-mirror-error! "entity" ex)))))))

(defn- hydrate-endpoint-for-xt
  [conn endpoint]
  (let [eid (or (:entity/id endpoint)
                (:id endpoint))
        stored (or (entity-by-id conn eid)
                   (when-let [name (:name endpoint)]
                     (entity-by-name conn name)))
        public (some-> stored entity->public)]
    (cond-> endpoint
      public (merge public))))

(defn- maybe-mirror-relation!
  [opts relation conn]
  (let [xtdb-opts (:xtdb opts)]
    (when (and (xt-enabled? xtdb-opts) (xt/started?))
      (doseq [endpoint (->> [(get relation :src) (get relation :dst)]
                            (remove nil?))]
        (let [hydrated (hydrate-endpoint-for-xt conn endpoint)]
          (maybe-mirror-entity! opts hydrated)))
      (when-let [doc (relation->xt-doc relation)]
        (try
          (xt/put-rel-async! doc nil nil)
          (catch Exception ex
            (log-mirror-error! "relation" ex)))))))

(defn- maybe-delete-entity!
  [opts entity-id]
  (let [xtdb-opts (:xtdb opts)]
    (when (and entity-id (xt-enabled? xtdb-opts) (xt/started?))
      (try
        (xt/delete-entity! entity-id)
        (catch Exception ex
          (log-mirror-error! "entity delete" ex))))))

(defn- maybe-delete-relation!
  [opts relation-id]
  (let [xtdb-opts (:xtdb opts)]
    (when (and relation-id (xt-enabled? xtdb-opts) (xt/started?))
      (try
        (xt/delete-rel! relation-id)
        (catch Exception ex
          (log-mirror-error! "relation delete" ex))))))

(defn export-edn
  "Produce a serializable EDN snapshot for the current connection."
  [conn]
  (let [db (d/db conn)]
    {:datoms (->> (d/datoms db :eavt)
                  (filter :added)
                  (map (fn [datom]
                         [:db/add (:e datom) (:a datom) (:v datom)]))
                  vec)}))

(defn save-snapshot!
  "Persist the current db value for the provided connection to snapshot.edn.
   Returns the snapshot file path."
  [conn dir]
  (ensure-dir! dir)
  (let [file (snapshot-path dir)
        snapshot (export-edn conn)]
    (with-open [w (io/writer file)]
      (binding [*out* w
                *print-dup* true]
        (pr snapshot)))
    (.getAbsolutePath file)))

(defn- reset-event-log!
  [dir]
  (let [file (events-path dir)]
    (when (.exists file)
      (spit file ""))))

(defn maybe-compact!
  "Run snapshot+log compaction if the event count meets the configured threshold.
   opts expects {:snapshot-every n}."
  [conn dir {:keys [snapshot-every]}]
  (when (and snapshot-every (pos? snapshot-every))
    (let [count (get @!event-count dir 0)]
      (when (>= count snapshot-every)
        (save-snapshot! conn dir)
        (reset-event-log! dir)
        (swap! !event-count assoc dir 0)))))

(defn- coerce-uuid [v]
  (cond
    (nil? v) nil
    (instance? UUID v) v
    (string? v) (let [trimmed (str/trim v)]
                 (when (seq trimmed)
                   (try
                     (UUID/fromString trimmed)
                     (catch IllegalArgumentException _
                       trimmed))))
    :else v))

(defn- coerce-long [v]
  (cond
    (nil? v) nil
    (instance? Number v) (long v)
    (string? v) (try
                  (Long/parseLong (str/trim v))
                  (catch NumberFormatException _ nil))
    :else nil))

(defn- coerce-double [v]
  (cond
    (nil? v) nil
    (instance? Number v) (double v)
    (string? v) (try
                  (Double/parseDouble (str/trim v))
                  (catch NumberFormatException _ nil))
    :else nil))

(defn- normalize-type [t]
  (cond
    (keyword? t) t
    (nil? t) nil
    (string? t) (-> t str/lower-case keyword)
    (symbol? t) (-> t name str/lower-case keyword)
    :else t))

(defn- clean-string [v]
  (when v
    (let [value (cond
                  (string? v) v
                  (keyword? v) (name v)
                  :else (str v))
          trimmed (str/trim value)]
      (when (seq trimmed) trimmed))))

(defn- parse-trail-ts [value]
  (cond
    (nil? value) nil
    (instance? Number value) (long value)
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (or (coerce-long trimmed)
                            (try
                              (.toEpochMilli (Instant/parse trimmed))
                              (catch Exception _ nil)))))
    :else nil))

(defn- normalize-trail-entry [trail]
  (let [id (or (some-> (:id trail) coerce-uuid) (UUID/randomUUID))
        ts (or (parse-trail-ts (:timestamp trail)) (System/currentTimeMillis))
        session (or (clean-string (:session-id trail))
                    (clean-string (:session trail)))
        turn (clean-string (:turn-id trail))
        profile (clean-string (:profile trail))
        intent (clean-string (:intent trail))
        source (clean-string (:source trail))
        fruit (some-> (:fruit trail)
                      clean-string
                      str/lower-case
                      keyword)
        orb (some-> (:orb trail)
                    clean-string
                    str/lower-case
                    keyword)
        salience (coerce-double (:salience trail))
        payload (not-empty (dissoc trail :id :timestamp))]
    (cond-> {:trail/id id
             :trail/timestamp ts}
      session (assoc :trail/session-id session)
      turn (assoc :trail/turn-id turn)
      profile (assoc :trail/profile profile)
      intent (assoc :trail/intent intent)
      source (assoc :trail/source source)
      fruit (assoc :trail/rule (merge (:trail/rule payload {}) {:fruit/id fruit}))
      orb (update :trail/rule #(merge (or % {}) {:orb/id orb}))
      salience (assoc :trail/salience salience)
      (seq (:patterns trail)) (assoc :trail/patterns (vec (:patterns trail)))
      (seq (:events trail)) (assoc :trail/events (vec (:events trail)))
      (seq (:fruits trail)) (assoc :trail/fruits (vec (:fruits trail)))
      (seq (:paramitas trail)) (assoc :trail/paramitas (vec (:paramitas trail)))
      (seq (:futons trail)) (assoc :trail/futons (vec (:futons trail)))
      (seq (:prototypes trail)) (assoc :trail/prototypes (vec (:prototypes trail)))
      payload (assoc :trail/payload payload))))

(defn- trail->public [doc]
  (when doc
    {:id (:trail/id doc)
     :session-id (:trail/session-id doc)
     :turn-id (:trail/turn-id doc)
     :profile (:trail/profile doc)
     :timestamp (:trail/timestamp doc)
     :intent (:trail/intent doc)
     :fruits (:trail/fruits doc)
     :paramitas (:trail/paramitas doc)
     :rule (:trail/rule doc)
     :salience (:trail/salience doc)
     :patterns (:trail/patterns doc)
     :events (:trail/events doc)
     :futons (:trail/futons doc)
     :prototypes (:trail/prototypes doc)
     :source (:trail/source doc)}))

(def ^:private version-pull-pattern
  [:db/id
   :entity.version/id
   :entity.version/created-at
   :entity.version/updated-at
   :entity.version/data
   {:entity.version/identity [:entity/id]}
   {:entity.version/prev [:entity.version/id]}])

(def ^:private entity-pull-pattern
  (conj '[:db/id :entity/id :entity/name :entity/type
          :entity/last-seen :entity/seen-count :entity/pinned?
          :entity/external-id :entity/source :media/sha256]
        {:entity/current-version version-pull-pattern}))

(defn- version->public [m]
  (when m
    {:id (:entity.version/id m)
     :identity-id (get-in m [:entity.version/identity :entity/id])
     :created-at (:entity.version/created-at m)
     :updated-at (:entity.version/updated-at m)
     :prev-version (some-> m :entity.version/prev :entity.version/id)
     :data (:entity.version/data m)}))

(defn- entity->public [m]
  (when m
    (let [version (some-> (:entity/current-version m) version->public)]
      (cond-> {:id (:entity/id m)
               :name (:entity/name m)
               :type (:entity/type m)
               :last-seen (:entity/last-seen m)
               :seen-count (:entity/seen-count m)
               :pinned? (:entity/pinned? m)
               :external-id (:entity/external-id m)
               :source (:entity/source m)
               :media/sha256 (:media/sha256 m)
               :db/eid (:db/id m)}
        version (assoc :version version)))))

(defn- relation->public [m]
  (when m
    {:id (:relation/id m)
     :type (:relation/type m)
     :src (some-> (:relation/src m) entity->public)
     :dst (some-> (:relation/dst m) entity->public)
     :confidence (:relation/confidence m)
     :last-seen (:relation/last-seen m)
     :provenance (:relation/provenance m)
     :props (:relation/props m)
     :db/eid (:db/id m)}))

(defn- version-by-id [conn vid]
  (when vid
    (d/pull @conn version-pull-pattern [:entity.version/id vid])))

(defn- entity-by-id [conn id]
  (when id
    (d/pull @conn entity-pull-pattern [:entity/id id])))

(defn- entity-by-name [conn name]
  (when (seq (str/trim (or name "")))
    (let [lower (str/lower-case name)]
      (->> (gm/entities-by-name @conn name)
           (some (fn [ent]
                   (when (= (str/lower-case (:entity/name ent)) lower)
                     (entity-by-id conn (:entity/id ent)))))))))

(defn- entity-by-external
  [conn external-id source]
  (let [token (clean-string external-id)
        src (clean-string source)]
    (when (seq token)
      (let [db @conn
            match (if (seq src)
                    (ffirst (d/q '[:find ?eid
                                   :in $ ?external ?source
                                   :where
                                   [?e :entity/external-id ?external]
                                   [?e :entity/source ?source]
                                   [?e :entity/id ?eid]]
                                 db token src))
                    (ffirst (d/q '[:find ?eid
                                   :in $ ?external
                                   :where
                                   [?e :entity/external-id ?external]
                                   [?e :entity/id ?eid]]
                                 db token)))]
        (when match (entity-by-id conn match))))))

(defn- resolve-entity-ref
  [conn spec]
  (cond
    (nil? spec) nil
    (instance? UUID spec) (entity-by-id conn spec)
    (string? spec) (entity-by-name conn spec)
    (map? spec)
    (let [raw-id (or (:id spec) (:entity/id spec))
          raw-name (or (:name spec) (:entity/name spec))
          type (or (:type spec) (:entity/type spec))
          source (or (:source spec) (:entity/source spec) (:external-source spec))
          provided-external (or (:external-id spec) (:entity/external-id spec)
                               (when (and (string? raw-id) (not (fid/uuid-string? raw-id))) raw-id))
          uuid-id (cond
                    (instance? UUID raw-id) raw-id
                    (and (string? raw-id) (fid/uuid-string? raw-id)) (UUID/fromString (str/trim raw-id))
                    :else nil)
          canonical-id (or uuid-id
                           (when (and provided-external type)
                             (:entity/id (fid/coerce-id {:external-id provided-external
                                                         :type type
                                                         :external-source source}))))]
      (or (when canonical-id (entity-by-id conn canonical-id))
          (entity-by-external conn provided-external source)
          (when raw-name (entity-by-name conn raw-name))))
    :else nil))

(defn- normalize-source-value [value]
  (cond
    (map? value) value
    :else (let [clean (clean-string value)]
            (when (and clean (not= clean "external"))
              clean))))

(defn- normalize-entity-spec [spec]
  (cond
    (string? spec)
    {:name (str/trim spec)}

    (symbol? spec)
    {:name (str/trim (name spec))}

    (map? spec)
    (let [raw-name (or (:name spec) (:label spec) (:entity/name spec))
          name (some-> raw-name str str/trim)
          last-seen (coerce-long (or (:last-seen spec) (:entity/last-seen spec)))
          seen-count (coerce-long (or (:seen-count spec) (:entity/seen-count spec)))
          pinned? (some-> (or (:pinned? spec) (:entity/pinned? spec)) boolean)
          normalized-type (some-> (or (:type spec) (:entity/type spec)) normalize-type)
          uuid-from-name (when (and name (fid/uuid-string? name))
                           (UUID/fromString name))
          raw-id (or (:id spec) (:entity/id spec) uuid-from-name)
          raw-external (or (:external-id spec) (:entity/external-id spec))
          raw-source (or (:source spec) (:entity/source spec) (:external-source spec))
          raw-sha (or (:media/sha256 spec) (:entity/media-sha256 spec))
          id-info (fid/coerce-id {:id raw-id
                                  :external-id raw-external
                                  :type normalized-type
                                  :external-source raw-source})
          entity-id (:entity/id id-info)
          external-id (or (clean-string raw-external)
                          (:entity/external-id id-info))
          source (normalize-source-value raw-source)
          sha256 (clean-string raw-sha)
          base (cond-> {:name name}
                 entity-id (assoc :id entity-id)
                 normalized-type (assoc :type normalized-type)
                 last-seen (assoc :last-seen last-seen)
                 seen-count (assoc :seen-count seen-count)
                 (some? pinned?) (assoc :pinned? pinned?)
                 external-id (assoc :external-id external-id)
                 source (assoc :source source)
                 sha256 (assoc :media/sha256 sha256))]
      (if (str/blank? name)
        (throw (ex-info "Entity name required" {:spec spec}))
        base))
    :else
    (throw (ex-info "Unsupported entity spec" {:spec spec}))))

(defn- persist-version!
  [conn identity snapshot {:keys [created-at updated-at]}]
  (when (and identity snapshot)
    (let [identity-id (:entity/id identity)
          identity-db-id (:db/id identity)
          prev (some-> identity :entity/current-version)
          prev-id (some-> prev :entity.version/id)
          version-id (UUID/randomUUID)
          created (long (or created-at (System/currentTimeMillis)))
          updated (long (or updated-at created))
          base {:db/id -1
                :entity.version/id version-id
                :entity.version/identity [:entity/id identity-id]
                :entity.version/created-at created
                :entity.version/updated-at updated
                :entity.version/data snapshot}
          version-tx (if prev-id
                       (assoc base :entity.version/prev [:entity.version/id prev-id])
                       base)
          tx [version-tx
              [:db/add identity-db-id :entity/current-version [:entity.version/id version-id]]]]
      (d/transact! conn tx)
      (version->public (version-by-id conn version-id)))))

(defn- identity->snapshot [identity]
  (when identity
    (cond-> {:name (:entity/name identity)
             :type (:entity/type identity)
             :last-seen (:entity/last-seen identity)
             :seen-count (:entity/seen-count identity)}
      (contains? identity :entity/pinned?) (assoc :pinned? (:entity/pinned? identity))
      (:entity/external-id identity) (assoc :external-id (:entity/external-id identity))
      (:entity/source identity) (assoc :source (:entity/source identity))
      (:media/sha256 identity) (assoc :media/sha256 (:media/sha256 identity)))))

(defn- upsert-entity!
  [conn {:keys [id name type last-seen seen-count pinned? external-id source] :as spec}]
  (when (str/blank? name)
    (throw (ex-info "Entity name required" {:spec spec})))
  (let [normalized-type (normalize-type type)
        existing (or (entity-by-id conn id)
                     (entity-by-name conn name)
                     (when (and name (fid/uuid-string? name))
                       (entity-by-id conn (UUID/fromString name))))
        now (or (coerce-long last-seen) (System/currentTimeMillis))
        requested-count (coerce-long seen-count)
        current-count (:entity/seen-count existing)
        final-count (or requested-count
                        (if existing (inc (long (or current-count 0))) 1))
        pinned-flag (if (contains? spec :pinned?) (boolean pinned?) (:entity/pinned? existing))
        ext-id (clean-string external-id)
        src (normalize-source-value source)
        sha (clean-string (:media/sha256 spec))
        entity-id (or (:entity/id existing) id (UUID/randomUUID))]
    (if existing
      (let [final-type (or normalized-type (:entity/type existing))
            incoming-name (some-> name str/trim)
            current-name (:entity/name existing)
            name-update (when (and incoming-name
                                   (seq incoming-name)
                                   (not= (str/lower-case incoming-name)
                                         (str/lower-case current-name))
                                   (not= incoming-name (some-> (:entity/id existing) str)))
                          [:db/add (:db/id existing) :entity/name incoming-name])]
        (when (and normalized-type (not= (:entity/type existing) normalized-type))
          (d/transact! conn [[:db/add (:db/id existing) :entity/type normalized-type]]))
        (d/transact! conn (->> [[:db/add (:db/id existing) :entity/last-seen now]
                                [:db/add (:db/id existing) :entity/seen-count final-count]
                                name-update
                                (when (and ext-id (not= (:entity/external-id existing) ext-id))
                                  [:db/add (:db/id existing) :entity/external-id ext-id])
                                (when (and src (not= (:entity/source existing) src))
                                  [:db/add (:db/id existing) :entity/source src])
                                (when (and sha (not= (:media/sha256 existing) sha))
                                  [:db/add (:db/id existing) :media/sha256 sha])
                                (when (contains? spec :pinned?)
                                  [:db/add (:db/id existing) :entity/pinned? pinned-flag])]
                               (remove nil?))))
      (let [tx (cond-> {:db/id -1
                        :entity/id entity-id
                        :entity/name name
                        :entity/last-seen now
                        :entity/seen-count final-count}
                 normalized-type (assoc :entity/type normalized-type)
                 ext-id (assoc :entity/external-id ext-id)
                 src (assoc :entity/source src)
                 sha (assoc :media/sha256 sha)
                 (contains? spec :pinned?) (assoc :entity/pinned? pinned-flag))]
        (d/transact! conn [tx])))
    (let [identity (entity-by-id conn entity-id)
          snapshot (identity->snapshot identity)
          _ (persist-version! conn identity snapshot {:created-at now})
          refreshed (entity-by-id conn entity-id)]
      (entity->public refreshed))))

(defn- relation-by-id [conn rid]
  (when rid
    (d/pull @conn '[:db/id :relation/id :relation/type :relation/provenance
                    :relation/confidence :relation/last-seen
                    {:relation/src [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?]}
                    {:relation/dst [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?]}]
            [:relation/id rid])))

(defn- find-relation [conn rel-type src-id dst-id]
  (when (and rel-type src-id dst-id)
    (let [db @conn
          result (d/q '[:find (pull ?r [:db/id :relation/id :relation/type :relation/provenance
                                        :relation/confidence :relation/last-seen])
                        :in $ ?type ?src-id ?dst-id
                        :where
                        [?r :relation/type ?type]
                        [?r :relation/src ?src-e]
                        [?src-e :entity/id ?src-id]
                        [?r :relation/dst ?dst-e]
                        [?dst-e :entity/id ?dst-id]]
                      db rel-type src-id dst-id)]
      (-> result ffirst))))

(defn- relations-touching-entity
  [conn entity-id]
  (let [pattern '[:db/id :relation/id :relation/type :relation/provenance
                  :relation/confidence :relation/last-seen
                  {:relation/src [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?]}
                  {:relation/dst [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?]}]
        db @conn
        outgoing (d/q '[:find (pull ?r pattern)
                        :in $ pattern ?eid
                        :where
                        [?src :entity/id ?eid]
                        [?r :relation/src ?src]]
                      db pattern entity-id)
        incoming (d/q '[:find (pull ?r pattern)
                        :in $ pattern ?eid
                        :where
                        [?dst :entity/id ?eid]
                        [?r :relation/dst ?dst]]
                      db pattern entity-id)]
    (->> (concat outgoing incoming)
         (map first)
         (reduce (fn [acc rel]
                   (assoc acc (:relation/id rel) rel))
                 {})
         vals
         vec)))

(def ^:private relation-provenance-keys #{:since :until :note})

(defn- normalize-provenance [prov]
  (when (map? prov)
    (->> prov
         (map (fn [[k v]]
                (let [kw (-> k name str/lower-case keyword)
                      value (some-> v str str/trim)]
                  (when (and (relation-provenance-keys kw)
                             (seq value))
                    [kw value]))))
         (remove nil?)
         (into {})
         not-empty)))

(defmulti apply-event!
  "Apply a single event map to the Datascript connection.
   Event maps must include :type."
  (fn [_ event]
    (:type event)))

(defmethod apply-event! :default
  [_conn event]
  (throw (ex-info "Unknown event type" {:event event})))

(defmethod apply-event! :entity/upsert
  [conn {:keys [entity]}]
  (let [spec (normalize-entity-spec entity)
        upserted (upsert-entity! conn spec)
        event' {:type :entity/upsert
                :entity (cond-> {:id (:id upserted)
                                 :name (:name upserted)}
                          (:type upserted) (assoc :type (:type upserted))
                          (:last-seen upserted) (assoc :last-seen (:last-seen upserted))
                          (:seen-count upserted) (assoc :seen-count (:seen-count upserted))
                          (contains? upserted :pinned?) (assoc :pinned? (:pinned? upserted))
                          (:external-id upserted) (assoc :external-id (:external-id upserted))
                          (:source upserted) (assoc :source (:source upserted))
                          (:media/sha256 upserted) (assoc :media/sha256 (:media/sha256 upserted)))}]
    {:event event'
     :result upserted}))

(defmethod apply-event! :relation/upsert
  [conn {:keys [relation]}]
  (trace-relation "store.apply.input" relation)
  (let [{:keys [type src dst provenance id props]} relation
        rel-type (normalize-type type)]
    (when-not rel-type
      (throw (ex-info "Relation type required" {:relation relation})))
    (let [src-spec (normalize-entity-spec src)
          dst-spec (normalize-entity-spec dst)
          src-entity (upsert-entity! conn src-spec)
          dst-entity (upsert-entity! conn dst-spec)
          resolved-id (coerce-uuid id)
          existing-by-pair (find-relation conn rel-type (:id src-entity) (:id dst-entity))
          relation-id (or resolved-id (:relation/id existing-by-pair) (UUID/randomUUID))
          existing (or (relation-by-id conn relation-id) existing-by-pair)
          prov (normalize-provenance provenance)
          conf (or (coerce-double (:confidence relation))
                   (coerce-double (:relation/confidence relation))
                   (:relation/confidence existing)
                   1.0)
          now (or (coerce-long (:last-seen relation))
                  (coerce-long (:relation/last-seen relation))
                  (System/currentTimeMillis))
          props (or props (:relation/props relation))
          base-rel {:id relation-id
                    :type rel-type
                    :src (select-keys src-entity [:id :name :type])
                    :dst (select-keys dst-entity [:id :name :type])
                    :confidence conf
                    :last-seen now}
          event-rel (cond-> base-rel
                      prov (assoc :provenance prov)
                      props (assoc :props props))]
      (trace-relation "store.apply.base" base-rel)
      (if existing
        (do
          (when prov
            (d/transact! conn [[:db/add (:db/id existing) :relation/provenance prov]]))
          (when props
            (d/transact! conn [[:db/add (:db/id existing) :relation/props props]]))
          (d/transact! conn (->> [[:db/add (:db/id existing) :relation/confidence conf]
                                  [:db/add (:db/id existing) :relation/last-seen now]]
                                 (remove nil?)))
          (trace-relation "store.apply.result" (assoc event-rel :db/eid (:db/id existing)))
          {:event {:type :relation/upsert
                   :relation (cond-> base-rel
                               prov (assoc :provenance prov)
                               props (assoc :props props))}
           :result (assoc event-rel
                          :provenance (or prov (:relation/provenance existing))
                          :props (or props (:relation/props existing))
                          :confidence conf
                          :last-seen now
                          :db/eid (:db/id existing))})
        (let [tx (cond-> {:db/id -1
                          :relation/id relation-id
                          :relation/type rel-type
                          :relation/src [:entity/id (:id src-entity)]
                          :relation/dst [:entity/id (:id dst-entity)]
                          :relation/confidence conf
                          :relation/last-seen now}
                   props (assoc :relation/props props)
                   prov (assoc :relation/provenance prov))
              {:keys [db-after tempids]} (d/transact! conn [tx])
              eid (d/resolve-tempid db-after tempids -1)]
          (trace-relation "store.apply.persisted" (assoc event-rel :db/eid eid))
          {:event {:type :relation/upsert
                   :relation event-rel}
           :result (assoc event-rel :db/eid eid)})))))

(defmethod apply-event! :relation/retract
  [conn {:keys [relation]}]
  (let [raw-id (or (:id relation) (:relation/id relation))
        relation-id (some-> raw-id coerce-uuid)
        existing (when relation-id (relation-by-id conn relation-id))]
    (when existing
      (d/transact! conn [[:db.fn/retractEntity (:db/id existing)]])
      {:event {:type :relation/retract
               :relation {:id (:relation/id existing)}}
       :result (relation->public existing)})))

(defmethod apply-event! :entity/retract
  [conn {:keys [entity]}]
  (let [raw-id (or (:id entity) (:entity/id entity))
        name (:name entity)
        id (some-> raw-id coerce-uuid)
        target (or (when id (entity-by-id conn id))
                   (when name (entity-by-name conn name)))]
    (when target
      (let [public (entity->public target)
            relations (relations-touching-entity conn (:entity/id target))
            relation-txs (mapv (fn [rel]
                                 [:db.fn/retractEntity (:db/id rel)])
                               relations)
            txs (conj relation-txs [:db.fn/retractEntity (:db/id target)])]
        (d/transact! conn txs)
        {:event {:type :entity/retract
                 :entity (select-keys public [:id :name :type])}
         :result {:entity public
                  :relations (mapv relation->public relations)}}))))

(defmethod apply-event! :entity/expire
  [conn {:keys [entity]}]
  (let [raw-id (or (:id entity) (:entity/id entity))
        name (:name entity)
        id (some-> raw-id coerce-uuid)
        target (or (when id (entity-by-id conn id))
                   (when name (entity-by-name conn name)))]
    (when target
      (let [seen (max 0 (long (or (coerce-long (:seen-count entity))
                                  (coerce-long (:entity/seen-count entity))
                                  0)))
            last-seen (or (coerce-long (:last-seen entity))
                          (coerce-long (:entity/last-seen entity))
                          0)
            unpin? (if (contains? entity :unpinned?)
                     (boolean (:unpinned? entity))
                     true)
            txs (cond-> [[:db/add (:db/id target) :entity/seen-count seen]
                         [:db/add (:db/id target) :entity/last-seen last-seen]]
                  unpin? (conj [:db/add (:db/id target) :entity/pinned? false]))
            _ (d/transact! conn txs)
            updated (-> target
                        (assoc :entity/seen-count seen
                               :entity/last-seen last-seen
                               :entity/pinned? (if unpin? false (:entity/pinned? target)))
                        entity->public)]
        {:event {:type :entity/expire
                 :entity {:id (:id updated)
                          :seen-count seen
                          :last-seen last-seen
                          :unpinned? unpin?}}
         :result updated}))))

(defmethod apply-event! :trail/record
  [conn {:keys [trail]}]
  (let [normalized (normalize-trail-entry trail)
        tx (assoc normalized :db/id -1)
        {:keys [db-after tempids]} (d/transact! conn [tx])
        eid (d/resolve-tempid db-after tempids -1)
        stored (assoc normalized :db/eid eid)]
    {:event {:type :trail/record
             :trail {:id (:trail/id stored)
                     :session-id (:trail/session-id stored)
                     :turn-id (:trail/turn-id stored)
                     :timestamp (:trail/timestamp stored)}}
     :result stored}))

(defn register-types-from-db!
  [conn]
  (let [db @conn
        entity-types (->> (d/q '[:find ?t :where [?e :entity/type ?t]] db)
                          (map first)
                          (remove nil?)
                          set)
        relation-types (->> (d/q '[:find ?t :where [?r :relation/type ?t]] db)
                            (map first)
                            (remove nil?)
                            set)
        utterance-intents (->> (d/q '[:find ?t :where [?u :utterance/intent ?t]] db)
                               (map first)
                               (remove nil?)
                               set)
        stored-intents (->> (d/q '[:find ?t :where [?i :intent/type ?t]] db)
                            (map first)
                            (remove nil?)
                            set)
        intent-types (set (concat utterance-intents stored-intents))]
    (when (seq entity-types)
      (types/ensure! :entity entity-types))
    (when (seq relation-types)
      (types/ensure! :relation relation-types))
    (when (seq intent-types)
      (types/ensure! :intent intent-types))))

(defn tx!
  "Apply event to conn and append it to the event log.
   opts should include :data-dir and optional :snapshot-every to control compaction.
   Returns the applied event."
  [conn {:keys [data-dir] :as opts} event]
  (when-not data-dir
    (throw (ex-info "Missing data-dir in opts" {:opts opts})))
  (when (:skip-verify? opts)
    (throw (ex-info "skip-verify? is disallowed; invariants must be enforced"
                    {:opts (dissoc opts :verify-fn)})))
  (when-let [verify-fn (:verify-fn opts)]
    (let [preview (d/create-conn schema)]
      (reset! preview @conn)
      (apply-event! preview event)
      (let [verify-opts (assoc opts :baseline-conn conn)
            result (try
                     (verify-fn preview event verify-opts)
                     (catch clojure.lang.ArityException _
                       (verify-fn preview event)))]
        (when (and (map? result) (false? (:ok? result)))
          (throw (ex-info "Model invariants failed"
                          {:result result
                           :event event})))))) 
  (ensure-dir! data-dir)
  (let [applied (apply-event! conn event)
        final-event (if (and (map? applied) (:event applied))
                      (:event applied)
                      event)
        result (if (and (map? applied) (contains? applied :result))
                 (:result applied)
                 applied)]
    (append-event! data-dir final-event)
    (swap! !event-count update data-dir (fnil inc 0))
    (maybe-compact! conn data-dir opts)
    (or result final-event)))

(defn restore!
  "Restore a Datascript connection using XTDB when available, falling back to legacy files.
   Accepts either a data-dir string or an opts map containing :data-dir and optional :xtdb map."
  ([cfg]
   (restore! cfg nil))
  ([cfg me-doc]
   (let [{:keys [data-dir xtdb] :as opts} (if (string? cfg)
                                            {:data-dir cfg}
                                            cfg)
         xtdb-opts (or xtdb {})]
     (when-not data-dir
       (throw (ex-info "Missing data-dir" {:opts opts})))
     (ensure-dir! data-dir)
     (ensure-xt-node! xtdb-opts data-dir)
     (let [conn (d/create-conn schema)
           _ (when (and (xt-enabled? xtdb-opts) (xt/started?))
               (println "[store] Hydrating from XTDB..."))
           xt-result (when (and (xt-enabled? xtdb-opts) (xt/started?))
                       (hydrate-from-xtdb! conn))
           xt-count  (when xt-result
                       (+ (:entity-count xt-result 0)
                          (:relation-count xt-result 0)))]
       (if (pos? (or xt-count 0))
         (do
           (println (str "[store] Hydrated " xt-count " entities from XTDB"))
           (swap! !event-count assoc data-dir 0)
           (register-types-from-db! conn))
         (let [legacy (hydrate-from-legacy! conn data-dir)]
           (println (str "[store] Hydrated from legacy (events: " (:event-count legacy 0) ")"))
           (swap! !event-count assoc data-dir (:event-count legacy 0))
           (when (and (xt-enabled? xtdb-opts) (:has-data? legacy))
             (sync-to-xtdb! conn))))
       (when (:entity/id me-doc)
         (when-not (entity-by-id conn :me)
           (let [name (:name me-doc "Me")
                 tx {:entity/id :me, :entity/name name, :entity/type :person, :entity/pinned? true}]
             (d/transact! conn [tx]))))
       (register-types-from-db! conn)
       conn))))

(defn compact!
  "Force a snapshot and event-log reset for the given data directory."
  [conn {:keys [data-dir]}]
  (when-not data-dir
    (throw (ex-info "Missing data-dir" {})))
  (save-snapshot! conn data-dir)
  (reset-event-log! data-dir)
  (swap! !event-count assoc data-dir 0)
  :ok)

(defn resolve-name->eid
  "Return entity metadata for the exact name match, or nil when absent."
  [conn name]
  (some-> (entity-by-name conn name)
          entity->public))

(defn- version-rows-desc [conn identity-db-id]
  (let [db @conn]
    (->> (d/q '[:find ?vid ?ts
                :in $ ?identity
                :where
                [?v :entity.version/identity ?identity]
                [?v :entity.version/id ?vid]
                [?v :entity.version/created-at ?ts]]
              db identity-db-id)
         (sort-by second >))))

(defn- versions-desc [conn identity-db-id]
  (->> (version-rows-desc conn identity-db-id)
       (map (fn [[vid _]] (some-> (version-by-id conn vid) version->public)))
       (remove nil?)
       vec))

(defn entity-history
  "Return {:entity ... :versions [...]} for the provided identity spec.
   opts may include :limit to cap the number of versions returned."
  [conn entity-spec {:keys [limit]}]
  (when-let [identity (resolve-entity-ref conn entity-spec)]
    (let [versions (versions-desc conn (:db/id identity))
          limited (if (and limit (pos? limit))
                    (vec (take limit versions))
                    versions)]
      {:entity (entity->public identity)
       :versions limited})))
(defn- version-matching-as-of [conn identity as-of]
  (when (and identity (:db/id identity) as-of)
    (some (fn [version]
            (when (<= (long (:created-at version)) (long as-of))
              version))
          (versions-desc conn (:db/id identity)))))

(defn- version-matching-id [conn identity version-id]
  (when (and identity version-id)
    (when-let [version (some-> (version-by-id conn version-id) version->public)]
      (when (= (:entity/id identity) (:identity-id version))
        version))))

(defn fetch-entity
  "Return the entity view for the provided spec with optional version filters.
   opts may include :version-id (UUID) or :as-of timestamp (ms)."
  [conn entity-spec {:keys [version-id as-of]}]
  (when-let [identity (resolve-entity-ref conn entity-spec)]
    (let [base (entity->public identity)
          requested (cond
                      version-id (version-matching-id conn identity version-id)
                      as-of (version-matching-as-of conn identity as-of)
                      :else (:version base))]
      (cond-> base
        requested (assoc :version requested)))))

(defn ensure-entity!
  "Ensure an entity with the provided attributes exists, recording the event log entry.
   entity-spec may include :name, optional :type keyword, and :id (UUID or string)."
  [conn opts entity-spec]
  (let [spec (normalize-entity-spec entity-spec)
        {:keys [name id]} spec
        existing (entity-by-name conn name)
        entity-id (or (:entity/id existing) id (UUID/randomUUID))
        now (or (:now opts) (:last-seen spec) (System/currentTimeMillis))
        final-type (or (:type spec) (:entity/type existing))
        final-count (or (:seen-count spec)
                        (if existing (inc (long (or (:entity/seen-count existing) 0))) 1))
        has-pinned? (contains? spec :pinned?)
        final-pinned (if has-pinned? (boolean (:pinned? spec)) (:entity/pinned? existing))
        external-id (:external-id spec)
        source (:source spec)
        sha (:media/sha256 spec)
        payload (cond-> {:id entity-id
                         :name name
                         :last-seen now
                         :seen-count final-count}
                  final-type (assoc :type final-type)
                  has-pinned? (assoc :pinned? final-pinned)
                  external-id (assoc :external-id external-id)
                  source (assoc :source source)
                  sha (assoc :media/sha256 sha))]
    (when final-type
      (types/ensure! :entity final-type))
    (let [applied (tx! conn opts {:type :entity/upsert
                                  :entity payload})
          entity-id (:id applied)
          stored (or (entity-by-id conn entity-id)
                     (entity-by-name conn name))
          public (or (some-> stored entity->public)
                     (select-keys applied [:id :name :type :db/eid :last-seen :seen-count :pinned? :external-id :source :media/sha256]))]
      (maybe-mirror-entity! opts public)
      public)))

(defn forget-entity!
  "Remove an entity and any relations that reference it.
   Returns details about the removed entity and relations, or nil when not found."
  [conn opts entity-spec]
  (when-let [target (resolve-entity-ref conn entity-spec)]
    (let [event {:type :entity/retract
                 :entity {:id (:entity/id target)
                          :name (:entity/name target)}}
          result (tx! conn opts event)
          {:keys [entity relations]} result]
      (doseq [rel relations]
        (maybe-delete-relation! opts (:id rel)))
      (when entity
        (maybe-delete-entity! opts (:id entity)))
      result)))

(defn expire-entity!
  "Reset an entity's salience counters.
   Options map may include :seen-count, :last-seen (ms since epoch), and :preserve-pin?."
  ([conn opts entity-spec]
   (expire-entity! conn opts entity-spec {}))
  ([conn opts entity-spec {:keys [seen-count last-seen preserve-pin?]}]
   (when-let [target (resolve-entity-ref conn entity-spec)]
     (let [event {:type :entity/expire
                  :entity {:id (:entity/id target)
                           :name (:entity/name target)
                           :seen-count (or seen-count 0)
                           :last-seen (or last-seen 0)
                           :unpinned? (not preserve-pin?)}}
           result (tx! conn opts event)]
       (when (map? result)
         (maybe-mirror-entity! opts result))
       result))))

(defn upsert-relation!
  "Upsert a relation edge between two entities using the event log.
   relation-spec expects {:type keyword :src {...} :dst {...}}.
   Optionally include :provenance map, :confidence double, :last-seen ms, :props map,
   and :id (UUID or string)."
  [conn opts relation-spec]
  (trace-relation "store.upsert.spec" relation-spec)
  (let [{:keys [type src dst provenance id confidence last-seen props]} relation-spec
        rel-type (normalize-type type)]
    (when-not rel-type
      (throw (ex-info "Relation type required" {:relation relation-spec})))
    (types/ensure! :relation rel-type)
    (let [src-spec (normalize-entity-spec src)
          dst-spec (normalize-entity-spec dst)
          prov (normalize-provenance provenance)
          now (or (coerce-long last-seen) (:now opts) (System/currentTimeMillis))
          conf (or (coerce-double confidence) 1.0)
          payload (cond-> {:type rel-type
                           :src src-spec
                           :dst dst-spec
                           :last-seen now
                           :confidence conf}
                    prov (assoc :provenance prov)
                    props (assoc :props props)
                    id (assoc :id (coerce-uuid id)))]
      (trace-relation "store.upsert.payload" payload)
      (let [result (tx! conn opts {:type :relation/upsert
                                   :relation payload})]
        (trace-relation "store.upsert.result" result)
        (maybe-mirror-relation! opts result conn)
        result))))

(defn delete-relation!
  "Remove the relation identified by relation-ref (UUID/map) from the store.
   Returns the public relation map when found, nil otherwise."
  [conn opts relation-ref]
  (let [raw-id (if (map? relation-ref)
                 (or (:id relation-ref) (:relation/id relation-ref))
                 relation-ref)
        relation-id (some-> raw-id coerce-uuid)]
    (when relation-id
      (let [result (tx! conn opts {:type :relation/retract
                                   :relation {:id relation-id}})]
        (when result
          (maybe-delete-relation! opts (:id result)))
        result))))

(defn latest-by-attr
  [db attr limit]
  (let [primary (vec (d/datoms db :avet attr))
        fallback (when (empty? primary)
                   (if (= attr :relation/last-seen)
                     (vec (d/datoms db :aevt :relation/type))
                     (vec (d/datoms db :aevt attr))))
        source (if (seq primary) primary (or fallback []))]
    (loop [idx (dec (count source))
           seen #{}
           acc []]
      (if (or (< idx 0) (>= (count acc) limit))
        acc
        (let [datom (nth source idx)
              eid (:e datom)]
          (if (seen eid)
            (recur (dec idx) seen acc)
            (recur (dec idx) (conj seen eid) (conj acc eid))))))))

(defn recent-relations
  "Return the most recent relation edges recorded in Datascript."
  ([conn]
   (recent-relations conn 5))
  ([conn limit]
   (let [db    @conn
         limit (max 1 (long (or limit 5)))
         eids  (latest-by-attr db :relation/last-seen limit)]
     (->> eids
          (keep (fn [eid]
                  (when-let [rel (d/pull db
                                         '[:relation/id :relation/type :relation/last-seen :relation/confidence
                                           {:relation/src [:entity/id :entity/name :entity/type]}
                                           {:relation/dst [:entity/id :entity/name :entity/type]}]
                                         eid)]
                    {:id         (:relation/id rel)
                     :type       (:relation/type rel)
                     :last-seen  (:relation/last-seen rel)
                     :confidence (:relation/confidence rel)
                     :src {:id   (get-in rel [:relation/src :entity/id])
                           :name (get-in rel [:relation/src :entity/name])
                           :type (get-in rel [:relation/src :entity/type])}
                     :dst {:id   (get-in rel [:relation/dst :entity/id])
                           :name (get-in rel [:relation/dst :entity/name])
                           :type (get-in rel [:relation/dst :entity/type])}})))
          (into []))))) ; materialise a vector only if you really need one

(defn recent-trails
  "Return the most recent recorded trail entries."
  ([conn]
   (recent-trails conn 10))
  ([conn limit]
   (let [db    @conn
         limit (max 1 (long (or limit 10)))
         eids  (latest-by-attr db :trail/timestamp limit)]
     (->> eids
          (keep (fn [eid]
                  (when-let [doc (d/pull db
                                         '[:trail/id :trail/session-id :trail/turn-id
                                           :trail/profile :trail/timestamp :trail/intent
                                           :trail/fruits :trail/paramitas :trail/rule
                                           :trail/salience :trail/patterns :trail/events
                                           :trail/source]
                                         eid)]
                    (trail->public doc))))
          (remove nil?)
          vec))))

(defn latest-entities-by-type
  "Return up to LIMIT entities of the given TYPE ordered by last-seen timestamp."
  [conn type limit]
  (when (and conn type)
    (let [db @conn
          limit (max 1 (long (or limit 1)))
          ;; pull more IDs than requested so we can filter by type without missing matches
          sample (* limit 8)
          candidates (latest-by-attr db :entity/last-seen sample)]
      (->> candidates
           (keep (fn [eid]
                   (let [doc (d/pull db entity-pull-pattern eid)]
                     (when (= (:entity/type doc) (normalize-type type))
                       (entity->public doc)))))
           (take limit)
           vec))))

(defn cooccurring-entities
  "Return entities that share relations with the provided entity-id.
   Results include appearance counts sorted by frequency."
  ([conn entity-id]
   (cooccurring-entities conn entity-id {:limit 10}))
  ([conn entity-id {:keys [limit] :or {limit 10}}]
   (when entity-id
     (let [db @conn
           collect (fn [query]
                     (d/q query db entity-id))
           outgoing (collect '[:find ?other-id ?other-name ?other-type (count ?rel)
                               :in $ ?target
                               :where
                               [?e :entity/id ?target]
                               [?rel :relation/src ?e]
                               [?rel :relation/dst ?other]
                               [?other :entity/id ?other-id]
                               [?other :entity/name ?other-name]
                               [(get-else $ ?other :entity/type nil) ?other-type]
                               [(not= ?other ?e)]])
           incoming (collect '[:find ?other-id ?other-name ?other-type (count ?rel)
                               :in $ ?target
                               :where
                               [?e :entity/id ?target]
                               [?rel :relation/dst ?e]
                               [?rel :relation/src ?other]
                               [?other :entity/id ?other-id]
                               [?other :entity/name ?other-name]
                               [(get-else $ ?other :entity/type nil) ?other-type]
                               [(not= ?other ?e)]])
           merged (reduce (fn [acc [id name type cnt]]
                            (let [entry (get acc id {:id id :name name :count 0})
                                  entry (-> entry
                                            (update :count + cnt)
                                            (cond-> (and type (nil? (:type entry))) (assoc :type type)))]
                              (assoc acc id entry)))
                          {}
                          (concat outgoing incoming))
           sorted (->> merged
                       vals
                       (sort-by (juxt (comp - :count) :name))
                       (take (max 1 limit))
                       vec)]
       sorted))))

(defn record-trail!
  "Persist a trail entry via the event log."
  [conn opts trail]
  (tx! conn opts {:type :trail/record
                  :trail trail}))
