;; scripts/pattern_dump.clj
(ns scripts.pattern-dump
  "Dump pattern entities + relations to a linearized EDN log."
  (:require [app.store-manager :as store-manager]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private pattern-types
  #{:pattern/library :pattern/component})

(def ^:private pattern-relation-types
  #{:pattern/includes :pattern/component-parent :pattern/has-sigil})

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.pattern-dump [--out PATH]"
    ""
    "Defaults to storage/patterns-db-dump-<timestamp>.edn in the repo root."
    ""]))

(defn- parse-args [args]
  (loop [args args opts {:out nil}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--out" arg) (recur (nnext args) (assoc opts :out (second args)))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

(defn- default-output-path []
  (let [root (or (repo-root) (io/file "."))
        storage (io/file root "storage")
        ts (-> (java.time.LocalDateTime/now)
               (.format (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")))
        filename (format "patterns-db-dump-%s.edn" ts)]
    (.getAbsolutePath (io/file storage filename))))

(def ^:private entity-pull
  [:entity/id :entity/name :entity/type :entity/external-id :entity/source
   :entity/last-seen :entity/seen-count :entity/pinned?])

(def ^:private relation-pull
  [:relation/id :relation/type :relation/props :relation/provenance
   :relation/last-seen :relation/confidence
   {:relation/src [:entity/id :entity/name :entity/type]}
   {:relation/dst [:entity/id :entity/name :entity/type]}])

(defn- pattern-entities [db]
  (mapcat (fn [t]
            (->> (d/q '[:find ?e
                        :in $ ?type
                        :where
                        [?e :entity/type ?type]]
                      db t)
                 (map first)
                 (map #(d/pull db entity-pull %))))
          pattern-types))

(defn- relation-ids [db]
  (map first
       (d/q '[:find ?r :where [?r :relation/id _]] db)))

(defn- pattern-relations [db pattern-ids]
  (->> (relation-ids db)
       (map #(d/pull db relation-pull %))
       (filter (fn [rel]
                 (let [rtype (:relation/type rel)
                       src-id (get-in rel [:relation/src :entity/id])
                       dst-id (get-in rel [:relation/dst :entity/id])]
                   (and (contains? pattern-relation-types rtype)
                        (or (contains? pattern-ids src-id)
                            (contains? pattern-ids dst-id))))))))

(defn- write-line! [^java.io.Writer w value]
  (.write w (pr-str value))
  (.write w "\n"))

(defn -main [& args]
  (let [{:keys [out]} (parse-args args)
        path (or out (default-output-path))
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)]
    (try
      (let [db @conn
            entities (vec (pattern-entities db))
            pattern-ids (set (map :entity/id entities))
            relations (vec (pattern-relations db pattern-ids))]
        (.mkdirs (.getParentFile (io/file path)))
        (with-open [w (io/writer path)]
          (write-line! w {:type :dump/meta
                          :profile profile
                          :timestamp (System/currentTimeMillis)
                          :entity-count (count entities)
                          :relation-count (count relations)})
          (doseq [entity entities]
            (write-line! w {:type :pattern/entity
                            :entity entity}))
          (doseq [rel relations]
            (write-line! w {:type :pattern/relation
                            :relation rel})))
        (println (format "Wrote %d pattern entities and %d relations to %s"
                         (count entities) (count relations) path)))
      (finally
        (store-manager/shutdown!)))))
