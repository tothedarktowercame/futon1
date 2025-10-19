(ns app.xt
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [xtdb.api :as xt]))

(defonce ^:private !node (atom nil))

(defn node []
  @!node)

(defn started? []
  (some? (node)))

(defn stop!
  []
  (when-let [n (and (started?) (node))]
    (.close ^java.lang.AutoCloseable n)
    (reset! !node nil)))

(defn- ensure-dir! [path]
  (when path
    (let [f (io/file path)]
      (when-let [parent (.getParentFile f)]
        (.mkdirs parent))
      (.mkdirs f)
      (.getAbsolutePath f))))

(defn- resolve-db-dir [data-dir path]
  (let [file (if (and data-dir (not (.isAbsolute (io/file path))))
               (io/file data-dir path)
               (io/file path))]
    (.getAbsolutePath file)))

(defn- prepare-config [cfg {:keys [data-dir]}]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (:db-dir x))
       (let [resolved (resolve-db-dir data-dir (:db-dir x))]
         (ensure-dir! resolved)
         (assoc x :db-dir resolved))
       x))
   cfg))

(defn start!
  ([cfg-edn-path]
   (start! cfg-edn-path {}))
  ([cfg-edn-path opts]
   (let [cfg-file (io/file cfg-edn-path)]
     (when-not (.exists cfg-file)
       (throw (ex-info "XTDB config file not found" {:path cfg-edn-path})))
     (let [config (-> cfg-file slurp edn/read-string (prepare-config opts))
           node (xt/start-node config)]
       (reset! !node node)
       node))))

(defn restart!
  ([cfg-edn-path]
   (restart! cfg-edn-path {}))
  ([cfg-edn-path opts]
   (stop!)
   (start! cfg-edn-path opts)))

(defn- ensure-node []
  (or (node)
      (throw (ex-info "XTDB node not started" {}))))

(defn submit!
  ([ops]
   (submit! ops {:await? true}))
  ([ops {:keys [await?] :or {await? true}}]
   (let [node (ensure-node)
         tx-res (xt/submit-tx node ops)]
     (when await?
       (xt/await-tx node tx-res))
     tx-res)))

(defn sync-node!
  "Force the node to index all pending transactions."
  []
  (let [node (ensure-node)]
    (.sync node nil)))

(defn- coerce-id [doc]
  (cond
    (:xt/id doc) doc
    (:entity/id doc) (assoc doc :xt/id (:entity/id doc))
    (:relation/id doc) (assoc doc :xt/id (:relation/id doc))
    :else doc))

(defn put-entity!
  [doc]
  (let [doc' (-> doc coerce-id)
        tx [[::xt/put doc']]]
    (submit! tx)))

(defn put-rel!
  [doc valid-from-ms valid-to-ms]
  (let [doc' (-> doc coerce-id)
        tx [[::xt/put doc'
             (when valid-from-ms (java.util.Date. valid-from-ms))
             (when valid-to-ms (java.util.Date. valid-to-ms))]]]
    (submit! tx)))

(defn delete-doc!
  "Remove a document (entity or relation) from XTDB by id."
  [id]
  (when id
    (submit! [[::xt/delete id]])))

(defn delete-entity!
  [id]
  (delete-doc! id))

(defn delete-rel!
  [id]
  (delete-doc! id))

(def ^:private datasource-classes
  (delay
   (keep (fn [class-name]
           (try (Class/forName class-name)
                (catch Throwable _ nil)))
         ["xtdb.api.XtdbDatasource"
          "xtdb.api.IXtdbDatasource"
          "xtdb.api.query.XtdbDatasource"
          "xtdb.query.QueryDatasource"])))

(defn- xtdb-datasource? [x]
  (boolean
   (some #(instance? % x)
         @datasource-classes)))

(defn q
  "Convenience wrapper around `xt/q`.
   - With a single query argument, runs it against the shared node.
   - When the first argument is an XT datasource (e.g. `(xt/db node)`),
     delegates directly to `xt/q` using that datasource."
  ([query]
   (xt/q (xt/db (ensure-node)) query))
  ([x y]
   (if (xtdb-datasource? x)
     (xt/q x y)
     (xt/q (xt/db (ensure-node)) x y)))
  ([x y & more]
   (if (xtdb-datasource? x)
     (apply xt/q x y more)
     (apply xt/q (xt/db (ensure-node)) x y more))))

(defn entity
  ([eid]
   (when-not (= :me eid)
     (xt/entity (xt/db (ensure-node)) eid)))
  ([eid valid-time]
   (when-not (= :me eid)
     (xt/entity (xt/db (ensure-node) valid-time) eid))))

(defn db
  ([] (xt/db (ensure-node)))
  ([valid-time] (xt/db (ensure-node) valid-time)))
