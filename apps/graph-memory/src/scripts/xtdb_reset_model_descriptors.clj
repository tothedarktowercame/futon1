;; scripts/xtdb_reset_model_descriptors.clj
(ns scripts.xtdb-reset-model-descriptors
  "Delete all model/descriptor docs from XTDB (use before re-bootstrapping)."
  (:require [app.charon-guard :as charon-guard]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:gen-class))

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.xtdb-reset-model-descriptors [options]"
    ""
    "Options:"
    "  --apply           Delete descriptors (default: dry-run)"
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
                         :xt/created-by "scripts.xtdb-reset-model-descriptors"})))

(defn- descriptor-docs [db]
  (->> (xtdb/q db '{:find [?e ?id ?name]
                    :where [[?e :entity/type :model/descriptor]
                            [?e :entity/id ?id]
                            [?e :entity/name ?name]]})
       (map (fn [[xt-id id name]]
              {:xt/id xt-id
               :entity/id id
               :entity/name name}))))

(defn -main [& args]
  (let [{:keys [apply? limit]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (start-xt!)
    (try
      (charon-guard/guard-models! conn [:meta-model] env :model/reset)
      (let [db (xt/db)
            docs (vec (descriptor-docs db))]
        (println "Model descriptor docs:" (count docs))
        (doseq [doc (take limit docs)]
          (println "descriptor:" (:xt/id doc) (:entity/id doc) (:entity/name doc)))
        (when (and apply? (seq docs))
          (doseq [doc docs]
            (xt/delete-entity! (:xt/id doc)))
          (println "Deleted descriptor docs:" (count docs))))
      (finally
        (xt/stop!)))))
