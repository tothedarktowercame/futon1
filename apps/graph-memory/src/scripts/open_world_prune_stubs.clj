(ns scripts.open-world-prune-stubs
  "Prune dangling entity stubs (only :entity/id) from the Datascript cache."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private stub-attrs
  [:entity/name
   :entity/type
   :entity/last-seen
   :entity/seen-count
   :entity/pinned?
   :entity/external-id
   :entity/source])

(defn- usage []
  (str "Usage: clojure -M -m scripts.open-world-prune-stubs [--dry-run] [--no-compact]\n"
       "  --dry-run     Report stubs without deleting\n"
       "  --no-compact  Skip snapshot+log compaction after pruning\n"))

(defn- parse-args [args]
  (loop [args args opts {:dry-run? false :compact? true}]
    (if (seq args)
      (case (first args)
        "--dry-run" (recur (rest args) (assoc opts :dry-run? true))
        "--no-compact" (recur (rest args) (assoc opts :compact? false))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- stub-entities [db]
  (d/q (into
        '[:find ?e ?id
          :where
          [?e :entity/id ?id]]
        (mapcat (fn [attr]
                  [(list 'not ['?e attr '_])]))
        stub-attrs)
       db))

(defn- referenced? [db eid]
  (or (seq (d/q '[:find ?r
                  :in $ ?e
                  :where
                  [?r :relation/src ?e]]
                db eid))
      (seq (d/q '[:find ?r
                  :in $ ?e
                  :where
                  [?r :relation/dst ?e]]
                db eid))
      (seq (d/q '[:find ?m
                  :in $ ?e
                  :where
                  [?m :mention/entity ?e]]
                db eid))))

(defn -main [& args]
  (let [{:keys [dry-run? compact?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)
        db @conn
        stubs (stub-entities db)
        candidates (remove (fn [[eid _id]] (referenced? db eid)) stubs)]
    (try
      (println (format "Found %d stub entity(ies); %d safe to prune."
                       (count stubs)
                       (count candidates)))
      (when (seq candidates)
        (println "Sample IDs:"
                 (str/join ", " (take 10 (map second candidates)))))
      (when-not dry-run?
        (doseq [[eid _] candidates]
          (d/transact! conn [[:db.fn/retractEntity eid]]))
        (println (format "Pruned %d stub entity(ies)." (count candidates)))
        (when compact?
          (store/compact! conn {:data-dir (:data-dir env)})
          (println "Compaction complete.")))
      (finally
        (store-manager/shutdown!)))))
