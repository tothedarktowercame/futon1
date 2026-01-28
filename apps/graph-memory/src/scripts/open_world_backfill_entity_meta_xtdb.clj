;; scripts/open_world_backfill_entity_meta_xtdb.clj
(ns scripts.open-world-backfill-entity-meta-xtdb
  "Backfill minimal metadata on open-world entities missing stub attributes."
  (:require [app.charon-guard :as charon-guard]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:gen-class))

(def ^:private stub-attrs
  [:entity/name
   :entity/type
   :entity/last-seen
   :entity/seen-count
   :entity/pinned?
   :entity/external-id
   :entity/source])

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.open-world-backfill-entity-meta-xtdb [options]"
    ""
    "Options:"
    "  --apply           Write updates (default: dry-run)"
    "  --dry-run         Report updates without applying (default)"
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
                         :xt/created-by "scripts.open-world-backfill-entity-meta-xtdb"})))

(defn- missing-stub-attrs? [doc]
  (not (some #(contains? doc %) stub-attrs)))

(defn- candidate-docs [db]
  (->> (xtdb/q db '{:find [(pull ?e [:entity/id
                                     :entity/label
                                     :entity/lower-label
                                     :entity/kind
                                     :entity/first-seen
                                     :entity/updated-at
                                     :entity/name
                                     :entity/type
                                     :entity/last-seen
                                     :entity/seen-count
                                     :entity/pinned?
                                     :entity/external-id
                                     :entity/source])]
                    :where [[?e :entity/id _]]})
       (map first)
       (filter (fn [doc]
                 (and (:entity/id doc)
                      (missing-stub-attrs? doc))))))

(defn- backfill-doc [doc now-ms]
  (cond-> doc
    (nil? (:entity/name doc))
    (assoc :entity/name (:entity/label doc))

    (nil? (:entity/seen-count doc))
    (assoc :entity/seen-count 1)

    (nil? (:entity/last-seen doc))
    (assoc :entity/last-seen (or (:entity/updated-at doc) now-ms))))

(defn -main [& args]
  (let [{:keys [apply? limit]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (start-xt!)
    (try
      (charon-guard/guard-models! conn [:open-world-ingest] env :open-world/backfill)
      (let [db (xt/db)
            now-ms (System/currentTimeMillis)
            docs (vec (candidate-docs db))]
        (println "Open-world entities missing stub metadata:" (count docs))
        (doseq [doc (take limit docs)]
          (println "needs-meta:" (:entity/id doc) (:entity/label doc)))
        (when (and apply? (seq docs))
          (doseq [doc docs]
            (xt/put-entity! (backfill-doc doc now-ms)))
          (println "Updated entities:" (count docs))))
      (finally
        (xt/stop!)))))
