(ns scripts.purge-pattern-includes
  "Remove pattern/includes relations whose dst name doesn't match the pattern prefix."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(defn- pattern-entities [db]
  (map first
       (d/q '[:find (pull ?e [:db/id :entity/id :entity/name])
              :where
              [?e :entity/type :pattern/library]]
            db)))

(defn- include-relations [db pattern-db-id]
  (map first
       (d/q '[:find (pull ?rel [:relation/id
                                {:relation/dst [:entity/name]}])
              :in $ ?pattern
              :where
              [?rel :relation/src ?pattern]
              [?rel :relation/type :pattern/includes]
              [?rel :relation/dst ?dst]]
            db pattern-db-id)))

(defn -main [& _args]
  (let [sm-cfg (store-manager/configure! {})
        profile (or (:default-profile sm-cfg)
                    (store-manager/default-profile))
        conn (store-manager/conn profile)
        env (store-manager/env profile)
        db @conn]
    (try
      (let [patterns (pattern-entities db)]
        (println (format "Scanning %d patterns in profile %s" (count patterns) profile))
        (doseq [pattern patterns
                :let [pattern-name (:entity/name pattern)
                      prefix (str pattern-name "/")
                      rels (include-relations db (:db/id pattern))
                      bad (filter (fn [rel]
                                    (let [name (get-in rel [:relation/dst :entity/name])]
                                      (and name (not (str/starts-with? name prefix)))))
                                  rels)]]
          (when (seq bad)
            (doseq [rel bad
                    :let [rel-id (:relation/id rel)]]
              (when rel-id
                (store/delete-relation! conn env rel-id)))
            (println (format "  %s: removed %d includes"
                             pattern-name
                             (count bad))))))
      (finally
        (store-manager/shutdown!)))))
