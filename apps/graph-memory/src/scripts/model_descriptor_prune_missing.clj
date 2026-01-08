;; scripts/model_descriptor_prune_missing.clj
(ns scripts.model-descriptor-prune-missing
  "Prune model/descriptor entities whose source metadata is missing required keys."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.model-descriptor-prune-missing [options]"
    ""
    "Options:"
    "  --apply           Delete missing descriptors (default: dry-run)"
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

(defn- parse-source [source]
  (cond
    (map? source) source
    (string? source) (try (edn/read-string source) (catch Exception _ nil))
    :else nil))

(defn- missing-source? [source]
  (let [parsed (parse-source source)]
    (or (not (map? parsed))
        (not (contains? parsed :schema/version))
        (not (contains? parsed :model/scope))
        (not (contains? parsed :schema/certificate)))))

(defn- descriptor-rows [conn]
  (d/q '[:find ?id ?name
         :where
         [?e :entity/type :model/descriptor]
         [?e :entity/id ?id]
         [?e :entity/name ?name]]
       @conn))

(defn -main [& args]
  (let [{:keys [apply? limit]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (try
      (let [rows (descriptor-rows conn)
            missing (->> rows
                         (map (fn [[id name]]
                                (let [source (:entity/source (d/pull @conn [:entity/source] [:entity/id id]))]
                                  {:entity/id id
                                   :entity/name name
                                   :entity/source source})))
                         (filter (fn [doc]
                                   (missing-source? (:entity/source doc))))
                         vec)]
        (println "Missing model descriptors:" (count missing))
        (doseq [doc (take limit missing)]
          (println "missing:" (:entity/id doc) (:entity/name doc)))
        (when (and apply? (seq missing))
          (doseq [doc missing]
            (store/forget-entity! conn env {:id (:entity/id doc)}))
          (println "Deleted missing descriptors:" (count missing))))
      (finally
        (store-manager/shutdown!)))))
