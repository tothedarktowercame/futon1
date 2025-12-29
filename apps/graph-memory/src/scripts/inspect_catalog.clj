(ns scripts.inspect-catalog
  "Inspect catalog relations for pattern-language/catalog."
  (:require [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private catalog-name "pattern-language/catalog")

(defn- catalog-entity [db]
  (ffirst
   (d/q '[:find (pull ?e [:db/id :entity/id :entity/name])
          :in $ ?name
          :where
          [?e :entity/name ?name]]
        db catalog-name)))

(defn- catalog-relations [db catalog-db-id]
  (d/q '[:find (pull ?r [:relation/id :relation/type :relation/provenance
                         {:relation/src [:entity/id :entity/name]}
                         {:relation/dst [:entity/id :entity/name]}])
         :in $ ?catalog
         :where
         [?r :relation/src ?catalog]]
       db catalog-db-id))

(defn- format-relation [doc]
  (let [prov (:relation/provenance doc)
        note (when (map? prov) (:note prov))
        dst (:relation/dst doc)]
    (format "  - %s (%s) note=%s"
            (:entity/name dst)
            (:entity/id dst)
            (or note "nil"))))

(defn -main [& _]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        db @conn]
    (try
      (if-let [catalog (catalog-entity db)]
        (let [rels (map first (catalog-relations db (:db/id catalog)))]
          (println (format "[%s] %s (%s)" profile (:entity/name catalog) (:entity/id catalog)))
          (println (format "  relations: %d" (count rels)))
          (doseq [rel (sort-by #(get-in % [:relation/dst :entity/name]) rels)]
            (println (format-relation rel))))
        (println (format "Catalog entity %s not found in profile %s" catalog-name profile)))
      (finally
        (store-manager/shutdown!)))))
