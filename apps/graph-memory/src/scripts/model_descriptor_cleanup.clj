;; scripts/model_descriptor_cleanup.clj
(ns scripts.model-descriptor-cleanup
  "Remove model/descriptor entities missing required source metadata."
  (:require [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:gen-class))

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.model-descriptor-cleanup [options]"
    ""
    "Options:"
    "  --apply           Delete legacy descriptor docs (default: dry-run)"
    "  --dry-run         Report deletions without applying (default)"
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
                         :xt/created-by "scripts.model-descriptor-cleanup"})))

(defn- missing-descriptor-source? [source]
  (let [parsed (cond
                 (map? source) source
                 (string? source) (try (edn/read-string source)
                                       (catch Exception _ nil))
                 :else nil)]
    (or (not (map? parsed))
        (not (contains? parsed :schema/version))
        (not (contains? parsed :model/scope))
        (not (contains? parsed :schema/certificate)))))

(defn- legacy-descriptor-docs [db]
  (->> (xtdb/q db '{:find [?e ?id ?name]
                    :where [[?e :entity/type :model/descriptor]
                            [?e :entity/id ?id]
                            [?e :entity/name ?name]]})
       (map (fn [[xt-id id name]]
              (let [doc (xtdb/entity db xt-id)]
                {:xt/id xt-id
                 :entity/id id
                 :entity/name name
                 :entity/source (:entity/source doc)})))
       (filter (fn [doc]
                 (and (instance? java.util.UUID (:entity/id doc))
                      (missing-descriptor-source? (:entity/source doc)))))))

(defn -main [& args]
  (let [{:keys [apply? limit]} (parse-args args)]
    (start-xt!)
    (try
      (let [db (xt/db)
            docs (vec (legacy-descriptor-docs db))]
        (println "Legacy descriptor docs:" (count docs))
        (doseq [doc (take limit docs)]
          (println "legacy:" (:xt/id doc) (:entity/id doc) (:entity/name doc)))
        (when (and apply? (seq docs))
          (doseq [doc docs]
            (xt/delete-entity! (:xt/id doc)))
          (println "Deleted legacy descriptor docs:" (count docs))))
      (finally
        (xt/stop!)))))
