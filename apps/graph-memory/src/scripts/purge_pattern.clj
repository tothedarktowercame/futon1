(ns scripts.purge-pattern
  "Remove existing pattern includes + pattern-owned components for a slug."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(defn- pattern-entity [db slug]
  (ffirst
   (d/q '[:find (pull ?e [:db/id :entity/id :entity/name])
          :in $ ?name
          :where
          [?e :entity/name ?name]
          [?e :entity/type :pattern/library]]
        db slug)))

(defn- include-relations [db pattern-db-id]
  (map first
       (d/q '[:find (pull ?rel [:relation/id
                                {:relation/dst [:entity/id :entity/name]}])
              :in $ ?pattern
              :where
              [?rel :relation/src ?pattern]
              [?rel :relation/type :pattern/includes]]
            db pattern-db-id)))

(defn -main [& [slug]]
  (if (nil? slug)
    (do
      (println "Usage: clojure -M -m scripts.purge-pattern <pattern-slug>")
      (System/exit 1))
    (let [sm-cfg (store-manager/configure! {})
          profile (or (:default-profile sm-cfg)
                      (store-manager/default-profile))
          conn (store-manager/conn profile)
          env (store-manager/env profile)
          db @conn]
      (try
        (if-let [pattern (pattern-entity db slug)]
          (let [pattern-name (:entity/name pattern)
                prefix (str pattern-name "/")
                rels (include-relations db (:db/id pattern))
                components (map :relation/dst rels)]
            (doseq [rel rels
                    :let [rel-id (:relation/id rel)]]
              (when rel-id
                (store/delete-relation! conn env rel-id)))
            (doseq [component components
                    :let [name (:entity/name component)]]
              (when (and name (str/starts-with? name prefix))
                (store/forget-entity! conn env {:id (:entity/id component)})))
            (println (format "Purged %d include relations for %s"
                             (count rels) pattern-name))
            (println (format "Deleted %d pattern components with prefix %s"
                             (count (filter (fn [c]
                                              (let [name (:entity/name c)]
                                                (and name (str/starts-with? name prefix))))
                                            components))
                             prefix)))
          (println (format "Pattern %s not found in profile %s" slug profile)))
        (finally
          (store-manager/shutdown!))))))
