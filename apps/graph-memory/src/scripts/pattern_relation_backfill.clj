(ns scripts.pattern-relation-backfill
  "Backfill provenance labels for pattern language relations."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private relation-type :arxana/scholium)
(def ^:private catalog-name "pattern-language/catalog")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.pattern-relation-backfill [--dry-run]"
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

(defn- label-for [src-name dst-name]
  (cond
    (= catalog-name src-name) ":language/catalog"
    (str/starts-with? (or dst-name "") "pattern-language/source/") ":language/source"
    (str/starts-with? (or dst-name "") "pattern-language/status/") ":language/status"
    :else nil))

(defn- relations [db]
  (let [pattern '[:relation/id :relation/type :relation/provenance
                  {:relation/src [:entity/id :entity/name]}
                  {:relation/dst [:entity/id :entity/name]}]]
    (->> (d/q '[:find (pull ?r pattern)
                :in $ pattern
                :where
                [?r :relation/type ?rtype]]
              db pattern)
         (map first)
         (filter (fn [doc]
                   (= relation-type (:relation/type doc))))
         vec)))

(defn- missing-labels [rels]
  (->> rels
       (map (fn [doc]
              (let [src-name (get-in doc [:relation/src :entity/name])
                    dst-name (get-in doc [:relation/dst :entity/name])
                    label (label-for src-name dst-name)
                    prov (:relation/provenance doc)
                    note (when (map? prov) (:note prov))]
                (when (and label (or (nil? note) (str/blank? (str note))))
                  {:id (:relation/id doc)
                   :src-id (get-in doc [:relation/src :entity/id])
                   :src-name (get-in doc [:relation/src :entity/name])
                   :dst-id (get-in doc [:relation/dst :entity/id])
                   :dst-name (get-in doc [:relation/dst :entity/name])
                   :label label
                   :provenance (if (map? prov) prov {})}))))
       (remove nil?)
       vec))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)
          db @conn]
      (try
        (let [rels (relations db)
              updates (missing-labels rels)]
          (println (format "Pattern relation backfill: total=%d missing-label=%d dry-run=%s"
                           (count rels) (count updates) (:dry-run? opts)))
          (doseq [{:keys [id src-id src-name dst-id dst-name label provenance]} updates]
            (let [prov (assoc provenance :note label)]
              (if (:dry-run? opts)
                (println (format "Would tag %s -> %s (%s)" src-id dst-id label))
                (store/upsert-relation! conn env
                                        {:type relation-type
                                         :id id
                                         :src {:id src-id
                                               :name src-name}
                                         :dst {:id dst-id
                                               :name dst-name}
                                         :provenance prov})))))
        (finally
          (store-manager/shutdown!))))))
