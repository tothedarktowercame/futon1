(ns scripts.xtdb-support
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xt]))

(def ^:private store-segments ["tx-log" "doc-store" "index-store"])

(defn default-data-dir []
  (or (System/getenv "BASIC_CHAT_DATA_DIR") "data/default"))

(defn- store-present? [root]
  (every? #(.exists (io/file root %)) store-segments))

(defn resolve-store-root [data-dir]
  (let [direct (.getAbsolutePath (io/file data-dir))
        nested (.getAbsolutePath (io/file data-dir "xtdb"))]
    (cond
      (store-present? direct) direct
      (store-present? nested) nested
      :else nested)))

(defn- marker-names [store-root]
  (->> store-segments
       (mapcat (fn [segment]
                 (let [dir (io/file store-root segment)]
                   (when (.exists dir)
                     (map #(.getName ^java.io.File %)
                          (or (.listFiles dir) []))))))
       set))

(defn detect-backend [store-root]
  (let [names (marker-names store-root)]
    (cond
      (or (contains? names "data.mdb")
          (contains? names "lock.mdb")) :lmdb
      (or (contains? names "CURRENT")
          (some #(str/ends-with? % ".sst") names)) :rocksdb
      :else (keyword (or (System/getenv "XTDB_WINDOWS_BACKEND") "rocksdb")))))

(defn- db-dir-locator [^java.io.File file]
  (if (str/starts-with? (System/getProperty "os.name") "Windows")
    (.toString (.toURI file))
    (.getAbsolutePath file)))

(defn node-config [store-root backend]
  (let [module (case backend
                 :lmdb 'xtdb.lmdb/->kv-store
                 :rocksdb 'xtdb.rocksdb/->kv-store
                 (throw (ex-info "Unsupported XTDB backend" {:backend backend})))
        dir-for (fn [segment]
                  (db-dir-locator (io/file store-root segment)))]
    {:xtdb/tx-log {:kv-store {:xtdb/module module
                              :db-dir (dir-for "tx-log")}}
     :xtdb/document-store {:kv-store {:xtdb/module module
                                      :db-dir (dir-for "doc-store")}}
     :xtdb/index-store {:kv-store {:xtdb/module module
                                   :db-dir (dir-for "index-store")}}}))

(defn start-node!
  ([] (start-node! (default-data-dir)))
  ([data-dir]
   (let [store-root (resolve-store-root data-dir)
         backend (detect-backend store-root)]
     {:data-dir data-dir
      :store-root store-root
      :backend backend
      :node (xt/start-node (node-config store-root backend))})))

(defn close-node! [node]
  (.close ^java.lang.AutoCloseable node))

(defn first-entity-ids [db limit]
  (->> (xt/q db '{:find [?e]
                  :where [[?e :xt/id _]]})
       (map first)
       distinct
       (take limit)))

(defn parse-id [s]
  (when s
    (try
      (edn/read-string s)
      (catch Exception _
        s))))
