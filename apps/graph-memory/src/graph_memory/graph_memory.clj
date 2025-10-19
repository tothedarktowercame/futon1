(ns graph-memory.graph-memory
  (:require [datascript.core :as d]))

(def schema
  {:node/id    {:db/unique :db.unique/identity}
   :node/type  {}
   :node/label {}
   :edge/to    {:db/valueType :db.type/ref}
   :edge/type  {}})

(defn init-db [] (d/create-conn schema))

(defonce conn (init-db))

(defn seed! []
  (d/transact! conn
               [{:node/id :joe :node/type :person :node/label "Joe"}
                {:node/id :project :node/type :artifact :node/label "Graph Memory Seed"}
                {:node/id :joe :edge/type :created :edge/to [:node/id :project]}]))

(defn conn! "Return the current Datascript connection."
  []
  conn)

(defn reset-conn! "Blow away the current DB and return a fresh connection (good for tests/experiments)."
  []
  (alter-var-root #'conn (constantly (d/create-conn schema))))

(defn upsert! [e] (d/transact! (conn!) [e]))

(defn node [id]
  (d/entity @(conn!) [:node/id id]))

(defn neighbors [id]
  (d/q '[:find ?to ?label
         :in $ ?id
         :where
         [?e :node/id ?id]
         [?e :edge/to ?to-e]
         [?to-e :node/id ?to]
         [?to-e :node/label ?label]]
       @(conn!) id))

(defn -main [& _]
  (seed!)
  (println "Nodes:"
           (count (d/q '[:find ?e :where [?e :node/id]] @conn))))

(comment
  (reset-conn!)
  (seed!)
  (require '[datascript.core :as d])
  (d/q '[:find ?e ?id :where [?e :node/id ?id]] @conn))
