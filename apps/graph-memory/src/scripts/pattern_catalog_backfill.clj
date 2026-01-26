(ns scripts.pattern-catalog-backfill
  "Backfill catalog relation provenance labels for pattern-language/catalog."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private catalog-name "pattern-language/catalog")
(def ^:private catalog-note ":language/catalog")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.pattern-catalog-backfill [--dry-run]"
    "Environment:"
    "  ALPHA_PROFILE      Profile name (defaults to configured profile)"
    "  BASIC_CHAT_DATA_DIR Data root (optional)"]))

(defn- parse-args [args]
  (loop [opts {:dry-run? false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--dry-run" (recur (assoc opts :dry-run? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- catalog-entity-id [db]
  (ffirst (d/q '[:find ?id
                 :where
                 [?e :entity/name ?name]
                 [?e :entity/id ?id]]
               db catalog-name)))

(defn- catalog-relations [db catalog-id]
  (when catalog-id
    (d/q '[:find ?rid ?dst-id ?prov
           :in $ ?catalog-id
           :where
           [?r :relation/type :arxana/scholium]
           [?r :relation/id ?rid]
           [?r :relation/src ?src]
           [?src :entity/id ?catalog-id]
           [?r :relation/dst ?dst]
           [?dst :entity/id ?dst-id]
           [(get-else $ ?r :relation/provenance nil) ?prov]]
         db catalog-id)))

(defn- ensure-note [prov]
  (let [base (if (map? prov) prov {})
        current (some-> (:note base) str/trim)]
    (if (and current (not (str/blank? current)))
      base
      (assoc base :note catalog-note))))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)
          db @conn
          catalog-id (catalog-entity-id db)]
      (try
        (if (nil? catalog-id)
          (println (format "Catalog entity %s not found in profile %s"
                           catalog-name profile))
          (let [rows (catalog-relations db catalog-id)
                updates (->> rows
                             (map (fn [[rid dst-id prov]]
                                    {:id rid
                                     :dst-id dst-id
                                     :provenance prov}))
                             (filter (fn [{:keys [provenance]}]
                                       (let [note (when (map? provenance)
                                                    (:note provenance))]
                                         (or (nil? note) (str/blank? (str note)))))))
                total (count rows)
                pending (count updates)]
            (println (format "Catalog relations: total=%d missing-note=%d dry-run=%s"
                             total pending (:dry-run? opts)))
            (doseq [{:keys [id dst-id provenance]} updates]
              (let [prov (ensure-note provenance)]
                (if (:dry-run? opts)
                  (println (format "Would tag %s -> %s" catalog-name dst-id))
                  (store/upsert-relation! conn env
                                          {:type :arxana/scholium
                                           :id id
                                           :src {:id catalog-id}
                                           :dst {:id dst-id}
                                           :provenance prov}))))))
        (finally
          (store-manager/shutdown!))))))
