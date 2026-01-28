;; scripts/xtdb_prune_empty_entities_and_relations.clj
(ns scripts.xtdb-prune-empty-entities-and-relations
  "Remove relations pointing at empty/missing entities and delete empty entities in XTDB."
  (:require [app.charon-guard :as charon-guard]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:gen-class))

(def ^:private entity-fields
  [:entity/id
   :entity/name
   :entity/type
   :entity/last-seen
   :entity/seen-count
   :entity/pinned?
   :entity/external-id
   :entity/source])

(def ^:private relation-fields
  [:relation/id :relation/type :relation/src :relation/dst])

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.xtdb-prune-empty-entities-and-relations [options]"
    ""
    "Options:"
    "  --apply           Delete relations/entities (default: dry-run)"
    "  --dry-run         Report without deleting (default)"
    "  --limit <n>       Limit detailed output (default: 50)"
    ""]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false :limit 50}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--apply" arg) (recur (rest args) (assoc opts :apply? true))
          (= "--dry-run" arg) (recur (rest args) (assoc opts :apply? false))
          (= "--limit" arg) (recur (nnext args) (assoc opts :limit (Long/parseLong (second args))))
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

(defn- resolve-config-path [path]
  (let [candidate (io/file path)]
    (cond
      (.exists candidate) (.getAbsolutePath candidate)
      :else (when-let [root (repo-root)]
              (let [alt (io/file root path)]
                (when (.exists alt)
                  (.getAbsolutePath alt)))))))

(defn- cli-penholder [env]
  (or (:penholder env)
      (System/getenv "MODEL_PENHOLDER")
      (System/getenv "BASIC_CHAT_PENHOLDER")
      (System/getenv "USER")
      (System/getenv "LOGNAME")
      "cli"))

(defn- profile-data-dir []
  (let [{:keys [data-root]} (store-manager/config)
        profile (store-manager/default-profile)
        profile-id (if (= :me profile) (store-manager/default-profile) profile)]
    (str (io/file data-root (str profile-id)))))

(defn- start-xt! []
  (let [{:keys [xtdb]} (store-manager/config)
        cfg-path (or (resolve-config-path (:config-path xtdb "apps/graph-memory/resources/xtdb.edn"))
                     (throw (ex-info "XTDB config file not found" {:path (:config-path xtdb)})))
        data-dir (profile-data-dir)]
    (xt/start! cfg-path {:data-dir (str (io/file data-dir "xtdb"))
                         :xt/created-by "scripts.xtdb-prune-empty-entities-and-relations"})))

(defn- hydratable? [doc]
  (when doc
    (or (some? (:entity/name doc))
        (some? (:entity/type doc))
        (some? (:entity/last-seen doc))
        (some? (:entity/seen-count doc))
        (contains? doc :entity/pinned?)
        (some? (:entity/external-id doc))
        (some? (:entity/source doc)))))

(defn- entity-docs [db]
  (->> (xtdb/q db '{:find [(pull ?e [:entity/id
                                     :entity/name
                                     :entity/type
                                     :entity/last-seen
                                     :entity/seen-count
                                     :entity/pinned?
                                     :entity/external-id
                                     :entity/source])]
                    :where [[?e :entity/id _]]})
       (map first)))

(defn- relation-docs [db]
  (->> (xtdb/q db '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst])]
                    :where [[?r :relation/id _]]})
       (map first)))

(defn -main [& args]
  (let [{:keys [apply? limit]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (let [base (store-manager/env profile)]
              (assoc base :penholder (cli-penholder base)))
        models [:patterns :media :meta-model :open-world-ingest :docbook :penholder]]
    (start-xt!)
    (try
      (charon-guard/guard-models! conn models env :xtdb/prune)
      (let [db (xt/db)
            entities (entity-docs db)
            empty-ids (->> entities
                           (remove hydratable?)
                           (keep :entity/id)
                           set)
            missing-ids (atom #{})
            rels (relation-docs db)
            doomed-rels (atom [])]
        (doseq [rel rels]
          (let [src (:relation/src rel)
                dst (:relation/dst rel)
                src-doc (when src (xtdb/entity db src))
                dst-doc (when dst (xtdb/entity db dst))
                missing-src? (or (nil? src) (nil? src-doc) (empty-ids src))
                missing-dst? (or (nil? dst) (nil? dst-doc) (empty-ids dst))]
            (when missing-src? (swap! missing-ids conj src))
            (when missing-dst? (swap! missing-ids conj dst))
            (when (or missing-src? missing-dst?)
              (swap! doomed-rels conj rel))))
        (println "Empty entities:" (count empty-ids))
        (doseq [eid (take limit empty-ids)]
          (println "empty:" eid))
        (println "Relations with missing endpoints:" (count @doomed-rels))
        (doseq [rel (take limit @doomed-rels)]
          (println "rel:" (:relation/id rel) (:relation/type rel)
                   "src" (:relation/src rel) "dst" (:relation/dst rel)))
        (when (and apply? (seq @doomed-rels))
          (doseq [rel @doomed-rels]
            (xt/delete-rel! (:relation/id rel)))
          (println "Deleted relations:" (count @doomed-rels)))
        (when (and apply? (seq empty-ids))
          (doseq [eid empty-ids]
            (xt/delete-entity! eid))
          (println "Deleted empty entities:" (count empty-ids))))
      (finally
        (xt/stop!)))))
