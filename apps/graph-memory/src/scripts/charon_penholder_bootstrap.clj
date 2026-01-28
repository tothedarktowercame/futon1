;; apps/graph-memory/src/scripts/charon_penholder_bootstrap.clj
(ns scripts.charon-penholder-bootstrap
  "Ensure Charon is a strict penholder for ingest descriptors."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private default-descriptors
  ["model/descriptor/patterns"
   "model/descriptor/media"
   "model/descriptor/meta-model"
   "model/descriptor/open-world-ingest"
   "model/descriptor/docbook"
   "model/descriptor/penholder"])

(defn- normalize-penholder [value]
  (when-let [raw (cond
                   (keyword? value) (name value)
                   (string? value) value
                   :else nil)]
    (let [clean (str/trim raw)]
      (when (seq clean)
        (str/lower-case clean)))))

(defn- existing-entries [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :model/penholder]]
                      db)
                 (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/source] %) ids)))

(defn- entry-by-descriptor [entries]
  (reduce (fn [acc entry]
            (let [source (:entity/source entry)
                  descriptor (:descriptor source)]
              (if descriptor
                (assoc acc descriptor entry)
                acc)))
          {}
          entries))

(defn- penholder-entry [descriptor penholders strict? certificate]
  {:name (str "penholder/" descriptor)
   :type :model/penholder
   :source {:descriptor descriptor
            :penholders penholders
            :certificate certificate
            :strict? strict?}})

(defn -main [& _args]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (assoc (store-manager/env profile) :penholder "charon")
        now (System/currentTimeMillis)
        certificate {:penholder "charon"
                     :issued-at now}
        entries (existing-entries @conn)
        by-descriptor (entry-by-descriptor entries)]
    (try
      (doseq [descriptor default-descriptors]
        (let [existing (get by-descriptor descriptor)
              source (:entity/source existing)
              prior (->> (:penholders source)
                         (map normalize-penholder)
                         (remove nil?)
                         vec)
              penholders (vec (distinct (concat prior ["charon"])))]
          (store/ensure-entity! conn env
                                (penholder-entry descriptor penholders true certificate))))
      (println "Charon penholder registry ready.")
      (finally
        (store-manager/shutdown!)))))
