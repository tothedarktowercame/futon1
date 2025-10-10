(ns app.store
  "Persistent Datascript-backed event store with append-only journal and snapshots."
  (:require [app.xt :as xt]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [graph-memory.main :as gm]
            [xtdb.api :as xtdb])
  (:import (java.io PushbackReader)
           (java.util UUID))
  )

(def schema
  "Datascript schema used by the runtime store."
  gm/schema)

(def ^:private events-filename "events.ndjson")
(def ^:private snapshot-filename "snapshot.edn")

;; Keeps track of pending event counts per data directory to trigger compaction.
(defonce ^:private !event-count (atom {}))

(declare apply-event! coerce-double coerce-long)

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

(defn- ensure-xt-node!
  [xtdb-opts default-data-dir]
  (when (xt-enabled? xtdb-opts)
    (let [cfg (xt-config-path xtdb-opts)]
      (when-not cfg
        (throw (ex-info "XTDB config not found" {:opts xtdb-opts})))
      (let [resolved-data-dir (or (:data-dir xtdb-opts)
                                  (when default-data-dir
                                    (.getAbsolutePath (io/file default-data-dir "xtdb"))))
            start-opts (cond-> {}
                          resolved-data-dir (assoc :data-dir (->absolute-path resolved-data-dir)))]
        (when (xt/started?)
          (xt/stop!))
        (xt/start! cfg start-opts)))))

(defn- xt-entity->tx
  [doc]
  (when-let [id (:entity/id doc)]
    (let [name (:entity/name doc)]
      (cond-> {:entity/id id}
        (some? name) (assoc :entity/name name)
      (:entity/type doc) (assoc :entity/type (:entity/type doc))
      (:entity/last-seen doc) (assoc :entity/last-seen (:entity/last-seen doc))
      (:entity/seen-count doc) (assoc :entity/seen-count (:entity/seen-count doc))
      (contains? doc :entity/pinned?) (assoc :entity/pinned? (:entity/pinned? doc))))))

(defn- xt-relation->tx
  [doc]
  (let [id (:relation/id doc)
        src (:relation/src doc)
        dst (:relation/dst doc)
        prov (:relation/provenance doc)]
    (when (and id src dst)
      (let [src-ref [:entity/id src]
            dst-ref [:entity/id dst]]
        (cond-> {:relation/id id
                 :relation/type (:relation/type doc)
                 :relation/src src-ref
                 :relation/dst dst-ref}
          prov (assoc :relation/provenance prov)
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

(defn- hydrate-from-xtdb!
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
                         (remove nil?)
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
                (or (@known-ids eid)
                    (let [found? (some? (d/pull @conn [:entity/id] [:entity/id eid]))]
                      (when found?
                        (swap! known-ids conj eid))
                      found?))))
            (ensure-entity! [eid]
              (when (and eid (not (present? eid)))
                (if-let [doc (xt/entity eid)]
                  (when-let [tx (xt-entity->tx doc)]
                    (d/transact! conn [(cond-> tx
                                          (nil? (:entity/name tx)) (dissoc :entity/name))])
                    (when-let [id (:entity/id tx)]
                      (swap! known-ids conj id)))
                  (let [stub {:entity/id eid}]
                    (d/transact! conn [stub])
                    (swap! known-ids conj eid)
                    (swap! stubbed! conj eid)))))]
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
                (do
                  (d/transact! conn [tx])
                  (swap! relation-count inc))
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
                                    :entity/last-seen :entity/seen-count :entity/pinned?])
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
      (contains? entity :entity/pinned?) (assoc :entity/pinned? (boolean (:entity/pinned? entity))))))

(defn- ds-relation->xt-doc
  [relation]
  (let [id (:relation/id relation)
        src-id (get-in relation [:relation/src :entity/id])
        dst-id (get-in relation [:relation/dst :entity/id])
        prov (:relation/provenance relation)]
    (when (and id src-id dst-id)
      (cond-> {:xt/id id
               :relation/id id
               :relation/type (:relation/type relation)
               :relation/src src-id
               :relation/dst dst-id}
        prov (assoc :relation/provenance prov)
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
        type (assoc :entity/type type)
        (:last-seen entity) (assoc :entity/last-seen (:last-seen entity))
        (:seen-count entity) (assoc :entity/seen-count (:seen-count entity))
        (contains? entity :pinned?) (assoc :entity/pinned? (boolean (:pinned? entity)))))))

(defn- relation->xt-doc
  [relation]
  (let [id (or (:relation/id relation) (:id relation))
        type (or (:relation/type relation) (:type relation))
        src (or (:relation/src relation) (:src relation))
        dst (or (:relation/dst relation) (:dst relation))
        src-id (or (:entity/id src) (:id src))
        dst-id (or (:entity/id dst) (:id dst))
        provenance (or (:relation/provenance relation) (:provenance relation))
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
          (xt/put-entity! doc)
          (catch Exception ex
            (log-mirror-error! "entity" ex)))))))

(defn- maybe-mirror-relation!
  [opts relation]
  (let [xtdb-opts (:xtdb opts)]
    (when (and (xt-enabled? xtdb-opts) (xt/started?))
      (doseq [endpoint (->> [(get relation :src) (get relation :dst)]
                            (remove nil?))]
        (maybe-mirror-entity! opts endpoint))
      (when-let [doc (relation->xt-doc relation)]
        (try
          (xt/put-rel! doc nil nil)
          (catch Exception ex
            (log-mirror-error! "relation" ex)))))))

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
    (instance? UUID v) v
    (string? v) (UUID/fromString v)
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

(defn- entity->public [m]
  (when m
    {:id (:entity/id m)
     :name (:entity/name m)
     :type (:entity/type m)
     :last-seen (:entity/last-seen m)
     :seen-count (:entity/seen-count m)
     :pinned? (:entity/pinned? m)
     :db/eid (:db/id m)}))

(defn- entity-by-id [conn id]
  (when id
    (d/pull @conn '[:db/id :entity/id :entity/name :entity/type
                    :entity/last-seen :entity/seen-count :entity/pinned?]
            [:entity/id id])))

(defn- entity-by-name [conn name]
  (when (seq (str/trim (or name "")))
    (let [lower (str/lower-case name)]
      (->> (gm/entities-by-name @conn name)
           (some (fn [ent]
                   (when (= (str/lower-case (:entity/name ent)) lower)
                     (entity-by-id conn (:entity/id ent)))))))))

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
          base (cond-> {:name name}
                 (:id spec) (assoc :id (coerce-uuid (:id spec)))
                 (:type spec) (assoc :type (normalize-type (:type spec)))
                 last-seen (assoc :last-seen last-seen)
                 seen-count (assoc :seen-count seen-count)
                 (some? pinned?) (assoc :pinned? pinned?))]
      (if (str/blank? name)
        (throw (ex-info "Entity name required" {:spec spec}))
        base))
    :else
    (throw (ex-info "Unsupported entity spec" {:spec spec}))))

(defn- upsert-entity!
  [conn {:keys [id name type last-seen seen-count pinned?] :as spec}]
  (when (str/blank? name)
    (throw (ex-info "Entity name required" {:spec spec})))
  (let [normalized-type (normalize-type type)
        existing (or (entity-by-id conn id)
                     (entity-by-name conn name))
        now (or (coerce-long last-seen) (System/currentTimeMillis))
        requested-count (coerce-long seen-count)
        current-count (:entity/seen-count existing)
        final-count (or requested-count
                        (if existing (inc (long (or current-count 0))) 1))
        pinned-flag (if (contains? spec :pinned?) (boolean pinned?) (:entity/pinned? existing))]
    (if existing
      (let [final-type (or normalized-type (:entity/type existing))]
        (when (and normalized-type (not= (:entity/type existing) normalized-type))
          (d/transact! conn [[:db/add (:db/id existing) :entity/type normalized-type]]))
        (d/transact! conn (->> [[:db/add (:db/id existing) :entity/last-seen now]
                                [:db/add (:db/id existing) :entity/seen-count final-count]
                                (when (contains? spec :pinned?)
                                  [:db/add (:db/id existing) :entity/pinned? pinned-flag])]
                               (remove nil?)))
        (-> existing
            (assoc :entity/type final-type
                   :entity/last-seen now
                   :entity/seen-count final-count
                   :entity/pinned? pinned-flag)
            entity->public))
      (let [entity-id (or id (UUID/randomUUID))
            tx (cond-> {:db/id -1
                        :entity/id entity-id
                        :entity/name name
                        :entity/last-seen now
                        :entity/seen-count final-count}
                normalized-type (assoc :entity/type normalized-type)
                (contains? spec :pinned?) (assoc :entity/pinned? pinned-flag))
            {:keys [db-after tempids]} (d/transact! conn [tx])
            eid (d/resolve-tempid db-after tempids -1)]
        {:id entity-id
         :name name
         :type normalized-type
         :last-seen now
         :seen-count final-count
         :pinned? (when (contains? spec :pinned?) pinned-flag)
         :db/eid eid}))))

(defn- relation-by-id [conn rid]
  (when rid
    (d/pull @conn '[:db/id :relation/id :relation/type :relation/provenance
                    :relation/confidence :relation/last-seen]
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
  [conn event]
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
                           (contains? upserted :pinned?) (assoc :pinned? (:pinned? upserted)))}]
    {:event event'
     :result upserted}))

(defmethod apply-event! :relation/upsert
  [conn {:keys [relation]}]
  (let [{:keys [type src dst provenance id]} relation
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
          base-rel {:id relation-id
                    :type rel-type
                    :src (select-keys src-entity [:id :name :type])
                    :dst (select-keys dst-entity [:id :name :type])
                    :confidence conf
                    :last-seen now}
          event-rel (cond-> base-rel
                      prov (assoc :provenance prov))]
      (if existing
        (do
          (when prov
            (d/transact! conn [[:db/add (:db/id existing) :relation/provenance prov]]))
          (d/transact! conn (->> [[:db/add (:db/id existing) :relation/confidence conf]
                                  [:db/add (:db/id existing) :relation/last-seen now]]
                                 (remove nil?)))
          {:event {:type :relation/upsert
                   :relation (cond-> base-rel
                                prov (assoc :provenance prov))}
           :result (assoc event-rel
                          :provenance (or prov (:relation/provenance existing))
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
                   prov (assoc :relation/provenance prov))
              {:keys [db-after tempids]} (d/transact! conn [tx])
              eid (d/resolve-tempid db-after tempids -1)]
          {:event {:type :relation/upsert
                   :relation event-rel}
           :result (assoc event-rel :db/eid eid)})))))

(defn tx!
  "Apply event to conn and append it to the event log.
   opts should include :data-dir and optional :snapshot-every to control compaction.
   Returns the applied event."
  [conn {:keys [data-dir] :as opts} event]
  (when-not data-dir
    (throw (ex-info "Missing data-dir in opts" {:opts opts})))
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
  [cfg]
  (let [{:keys [data-dir xtdb] :as opts} (if (string? cfg)
                                           {:data-dir cfg}
                                           cfg)
        xtdb-opts (or xtdb {})]
    (when-not data-dir
      (throw (ex-info "Missing data-dir" {:opts opts})))
    (ensure-dir! data-dir)
    (ensure-xt-node! xtdb-opts data-dir)
    (let [conn (d/create-conn schema)
          xt-result (when (and (xt-enabled? xtdb-opts) (xt/started?))
                      (hydrate-from-xtdb! conn))
          xt-count  (when xt-result
                      (+ (:entity-count xt-result 0)
                         (:relation-count xt-result 0)))]
      (if (pos? (or xt-count 0))
        (do
          (swap! !event-count assoc data-dir 0)
          conn)
        (let [legacy (hydrate-from-legacy! conn data-dir)]
          (swap! !event-count assoc data-dir (:event-count legacy 0))
          (when (and (xt-enabled? xtdb-opts) (:has-data? legacy))
            (sync-to-xtdb! conn))
          conn)))))

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

(defn ensure-entity!
  "Ensure an entity with the provided attributes exists, recording the event log entry.
   entity-spec may include :name, optional :type keyword, and :id UUID."
  [conn opts entity-spec]
  (let [spec (normalize-entity-spec entity-spec)
        {:keys [name id]} spec
        existing (entity-by-name conn name)
        entity-id (or id (:entity/id existing) (UUID/randomUUID))
        now (or (:now opts) (:last-seen spec) (System/currentTimeMillis))
        final-type (or (:type spec) (:entity/type existing))
        final-count (or (:seen-count spec)
                        (if existing (inc (long (or (:entity/seen-count existing) 0))) 1))
        has-pinned? (contains? spec :pinned?)
        final-pinned (if has-pinned? (boolean (:pinned? spec)) (:entity/pinned? existing))
        payload (cond-> {:id entity-id
                         :name name
                         :last-seen now
                         :seen-count final-count}
                  final-type (assoc :type final-type)
                  has-pinned? (assoc :pinned? final-pinned))]
    (let [result (-> (tx! conn opts {:type :entity/upsert
                                     :entity payload})
                     (select-keys [:id :name :type :db/eid :last-seen :seen-count :pinned?]))]
      (maybe-mirror-entity! opts result)
      result)))

(defn upsert-relation!
  "Upsert a relation edge between two entities using the event log.
   relation-spec expects {:type keyword :src {...} :dst {...}}.
   Optionally include :provenance map, :confidence double, :last-seen ms, and :id UUID."
  [conn opts relation-spec]
  (let [{:keys [type src dst provenance id confidence last-seen]} relation-spec
        rel-type (normalize-type type)]
    (when-not rel-type
      (throw (ex-info "Relation type required" {:relation relation-spec})))
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
                     id (assoc :id (coerce-uuid id)))]
      (let [result (tx! conn opts {:type :relation/upsert
                                   :relation payload})]
        (maybe-mirror-relation! opts result)
        result))))

(defn- latest-by-attr
  [db attr limit]
  (let [primary (vec (d/datoms db :avet attr))
        fallback (when (empty? primary)
                   (if (= attr :relation/last-seen)
                     (vec (d/datoms db :aevt :relation/type))
                     (vec (d/datoms db :aevt attr))))
        source (if (seq primary) primary (or fallback []))]
    (loop [idx (dec (count source))
           seen #{ }
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
   (let [db @conn
         limit (max 1 (long (or limit 5)))
         eids (latest-by-attr db :relation/last-seen limit)]
     (->> eids
          (map (fn [eid]
                 (let [rel (d/pull db '[:relation/id :relation/type :relation/last-seen :relation/confidence
                                         {:relation/src [:entity/id :entity/name :entity/type]}
                                         {:relation/dst [:entity/id :entity/name :entity/type]}]
                                    eid)]
                   (when rel
                     {:id (:relation/id rel)
                      :type (:relation/type rel)
                      :last-seen (:relation/last-seen rel)
                      :confidence (:relation/confidence rel)
                      :src {:id (get-in rel [:relation/src :entity/id])
                            :name (get-in rel [:relation/src :entity/name])
                            :type (get-in rel [:relation/src :entity/type])}
                      :dst {:id (get-in rel [:relation/dst :entity/id])
                            :name (get-in rel [:relation/dst :entity/name])
                            :type (get-in rel [:relation/dst :entity/type])}})))
          (remove nil?)
          vec)))))

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

