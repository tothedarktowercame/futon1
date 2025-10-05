(ns app.store
  "Persistent Datascript-backed event store with append-only journal and snapshots."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [graph-memory.main :as gm])
  (:import (java.io PushbackReader)
           (java.util UUID)))

(def schema
  "Datascript schema used by the runtime store."
  gm/schema)

(def ^:private events-filename "events.ndjson")
(def ^:private snapshot-filename "snapshot.edn")

;; Keeps track of pending event counts per data directory to trigger compaction.
(defonce ^:private !event-count (atom {}))

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
     :db/eid (:db/id m)}))

(defn- entity-by-id [conn id]
  (when id
    (d/pull @conn '[:db/id :entity/id :entity/name :entity/type]
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
          name (some-> raw-name str str/trim)]
      (when (str/blank? name)
        (throw (ex-info "Entity name required" {:spec spec})))
      (cond-> {:name name}
        (:id spec) (assoc :id (coerce-uuid (:id spec)))
        (:type spec) (assoc :type (normalize-type (:type spec)))))

    :else
    (throw (ex-info "Unsupported entity spec" {:spec spec}))))

(defn- upsert-entity!
  [conn {:keys [id name type] :as spec}]
  (when (str/blank? name)
    (throw (ex-info "Entity name required" {:spec spec})))
  (let [normalized-type (normalize-type type)
        existing (or (entity-by-id conn id)
                     (entity-by-name conn name))]
    (if existing
      (let [final-type (or normalized-type (:entity/type existing))]
        (when (and normalized-type (not= (:entity/type existing) normalized-type))
          (d/transact! conn [[:db/add (:db/id existing) :entity/type normalized-type]]))
        (-> existing
            (assoc :entity/type final-type)
            entity->public))
      (let [entity-id (or id (UUID/randomUUID))
            tx (cond-> {:db/id -1
                        :entity/id entity-id
                        :entity/name name}
                 normalized-type (assoc :entity/type normalized-type))
            {:keys [db-after tempids]} (d/transact! conn [tx])
            eid (d/resolve-tempid db-after tempids -1)]
        {:id entity-id
         :name name
         :type normalized-type
         :db/eid eid}))))

(defn- relation-by-id [conn rid]
  (when rid
    (d/pull @conn '[:db/id :relation/id :relation/type :relation/provenance]
            [:relation/id rid])))

(defn- find-relation [conn rel-type src-id dst-id]
  (when (and rel-type src-id dst-id)
    (let [db @conn
          result (d/q '[:find (pull ?r [:db/id :relation/id :relation/type :relation/provenance])
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
                           (:type upserted) (assoc :type (:type upserted)))}]
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
          base-rel {:id relation-id
                    :type rel-type
                    :src (select-keys src-entity [:id :name :type])
                    :dst (select-keys dst-entity [:id :name :type])}
          event-rel (cond-> base-rel
                      prov (assoc :provenance prov))]
      (if existing
        (do
          (when prov
            (d/transact! conn [[:db/add (:db/id existing) :relation/provenance prov]]))
          {:event {:type :relation/upsert
                   :relation (cond-> base-rel
                                prov (assoc :provenance prov))}
           :result (assoc event-rel
                          :provenance (or prov (:relation/provenance existing))
                          :db/eid (:db/id existing))})
        (let [tx (cond-> {:db/id -1
                          :relation/id relation-id
                          :relation/type rel-type
                          :relation/src [:entity/id (:id src-entity)]
                          :relation/dst [:entity/id (:id dst-entity)]}
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
  "Restore a Datascript connection from snapshot + event log in data-dir."
  [data-dir]
  (ensure-dir! data-dir)
  (let [conn (d/create-conn schema)
        snapshot (load-snapshot data-dir)
        events (read-events data-dir)]
    (when snapshot
      (if (:datoms snapshot)
        (do
          (reset! conn (d/empty-db schema))
          (d/transact! conn (:datoms snapshot)))
        (reset! conn snapshot)))
    (doseq [event events]
      (apply-event! conn event))
    (swap! !event-count assoc data-dir (count events))
    conn))

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
        {:keys [name type id]} spec
        existing (entity-by-name conn name)
        entity-id (or id (:entity/id existing) (UUID/randomUUID))
        payload {:id entity-id
                 :name name
                 :type (or type (:entity/type existing))}]
    (-> (tx! conn opts {:type :entity/upsert
                        :entity payload})
        (select-keys [:id :name :type :db/eid]))))

(defn upsert-relation!
  "Upsert a relation edge between two entities using the event log.
   relation-spec expects {:type keyword :src {...} :dst {...}}.
   Optionally include :provenance map and :id UUID."
  [conn opts relation-spec]
  (let [{:keys [type src dst provenance id]} relation-spec
        rel-type (normalize-type type)]
    (when-not rel-type
      (throw (ex-info "Relation type required" {:relation relation-spec})))
    (let [src-spec (normalize-entity-spec src)
          dst-spec (normalize-entity-spec dst)
          prov (normalize-provenance provenance)
          payload (cond-> {:type rel-type
                           :src src-spec
                           :dst dst-spec}
                     prov (assoc :provenance prov)
                     id (assoc :id (coerce-uuid id)))]
      (tx! conn opts {:type :relation/upsert
                      :relation payload}))))
