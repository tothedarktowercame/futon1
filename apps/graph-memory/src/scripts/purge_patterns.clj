(ns scripts.purge-patterns
  "Remove all pattern/library + pattern/component entities from the current profile."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [datascript.core :as d]))

(defn- entity-ids-by-type [db etype]
  (map first
       (d/q '[:find ?e
              :in $ ?type
              :where
              [?e :entity/type ?type]]
            db etype)))

(defn- entity-by-eid [db eid]
  (d/pull db [:entity/id :entity/name :entity/type] eid))

(defn -main [& _args]
  (let [sm-cfg (store-manager/configure! {})
        profile (or (:default-profile sm-cfg)
                    (store-manager/default-profile))
        conn (store-manager/conn profile)
        env (store-manager/env profile)
        db @conn
        pattern-entities (concat (entity-ids-by-type db :pattern/library)
                                 (entity-ids-by-type db :pattern/component))]
    (try
      (println (format "Purging %d pattern entities in profile %s"
                       (count pattern-entities) profile))
      (doseq [eid pattern-entities
              :let [entity (entity-by-eid db eid)]]
        (when-let [id (:entity/id entity)]
          (store/forget-entity! conn env {:id id})))
      (finally
        (store-manager/shutdown!)))))
