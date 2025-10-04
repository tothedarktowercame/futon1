(ns graph-memory.main
  (:require [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.util UUID)))

(def schema
  {:utterance/id    {:db/unique :db.unique/identity}
   :utterance/text  {}
   :utterance/ts    {}
   :intent/id       {:db/unique :db.unique/identity}
   :intent/data     {}
   :link/id         {:db/unique :db.unique/identity}
   :link/type       {}
   :link/from       {:db/valueType :db.type/ref}
   :link/to         {:db/valueType :db.type/ref}
   :entity/id       {:db/unique :db.unique/identity}
   :entity/name     {:db/unique :db.unique/identity}
   :entity/type     {}
   :mention/id      {:db/unique :db.unique/identity}
   :mention/utterance {:db/valueType :db.type/ref}
   :mention/entity  {:db/valueType :db.type/ref}
   :mention/span    {}
   :relation/id     {:db/unique :db.unique/identity}
   :relation/type   {}
   :relation/src    {:db/valueType :db.type/ref}
   :relation/dst    {:db/valueType :db.type/ref}
   :relation/provenance {}})

(defn init-db []
  (d/create-conn schema))

(defn- transact! [conn tx]
  (let [{:keys [db-after tempids]} (d/transact! conn tx)]
    {:db-after db-after
     :resolve (fn [tmp]
                (d/resolve-tempid db-after tempids tmp))}))

(defn add-utterance! [conn text ts]
  (let [id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        {:keys [resolve]} (transact! conn [{:db/id tmp
                                            :utterance/id id
                                            :utterance/text text
                                            :utterance/ts ts}])
        eid (resolve tmp)]
    {:id id
     :db/eid eid
     :text text
     :ts ts}))

(defn add-intent! [conn intent]
  (let [id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        {:keys [resolve]} (transact! conn [{:db/id tmp
                                            :intent/id id
                                            :intent/data intent}])
        eid (resolve tmp)]
    {:id id
     :db/eid eid
     :data intent}))

(defn- conn->db [conn-or-db]
  (if (and (map? conn-or-db) (:schema conn-or-db))
    conn-or-db
    (d/db conn-or-db)))

(defn ensure-entity!
  "Return (and create if needed) an entity with stable identifiers."
  [conn {:keys [name type]}]
  (let [db @conn
        eid (ffirst (d/q '[:find ?e
                            :in $ ?name
                            :where
                            [?e :entity/name ?name]]
                          db name))]
    (if eid
      (let [entity (d/pull db '[:entity/id :entity/name :entity/type] eid)
            existing-type (:entity/type entity)]
        (when (and type (not= existing-type type))
          (d/transact! conn [[:db/add eid :entity/type type]]))
        {:id (:entity/id entity)
         :name (:entity/name entity)
         :type (or type existing-type)
         :db/eid eid})
      (let [entity-id (UUID/randomUUID)
            tmp (d/tempid :db.part/user)
            {:keys [resolve]} (transact! conn [{:db/id tmp
                                                :entity/id entity-id
                                                :entity/name name
                                                :entity/type type}])
            eid (resolve tmp)]
        {:id entity-id
         :name name
         :type type
         :db/eid eid}))))

(defn add-mention!
  "Associate an utterance with an entity. span is an inclusive-exclusive vector."
  [conn utterance-id entity-id span]
  (let [mention-id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        {:keys [resolve]} (transact! conn [{:db/id tmp
                                            :mention/id mention-id
                                            :mention/utterance [:utterance/id utterance-id]
                                            :mention/entity   [:entity/id entity-id]
                                            :mention/span     span}])
        eid (resolve tmp)]
    {:id mention-id
     :span span
     :db/eid eid}))

(defn add-relation!
  "Create a relation between two entities with optional provenance."
  [conn {:keys [type src-id dst-id prov]}]
  (let [rel-id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        {:keys [resolve]} (transact! conn [{:db/id tmp
                                            :relation/id rel-id
                                            :relation/type type
                                            :relation/src [:entity/id src-id]
                                            :relation/dst [:entity/id dst-id]
                                            :relation/provenance prov}])
        eid (resolve tmp)]
    {:id rel-id
     :type type
     :db/eid eid}))

(defn link! [conn from-id to-id edge-type]
  (let [id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        {:keys [resolve]} (transact! conn [{:db/id tmp
                                            :link/id id
                                            :link/type edge-type
                                            :link/from from-id
                                            :link/to to-id}])
        eid (resolve tmp)]
    {:id id
     :db/eid eid
     :type edge-type}))

(defn find-utterances [conn]
  (d/q '[:find ?id ?text ?ts
         :where
         [?e :utterance/id ?id]
         [?e :utterance/text ?text]
         [?e :utterance/ts ?ts]]
       @conn))

(defn entities-by-name
  "Return entities whose name starts with the provided string (case-insensitive).
   When s is nil, return all entities."
  [conn-or-db s]
  (let [db (conn->db conn-or-db)
        q (if s
            '[:find (pull ?e [:entity/id :entity/name :entity/type])
              :in $ ?prefix
              :where
              [?e :entity/name ?name]
              [(clojure.string/lower-case ?name) ?ln]
              [(clojure.string/lower-case ?prefix) ?lp]
              [(clojure.string/starts-with? ?ln ?lp)]]
            '[:find (pull ?e [:entity/id :entity/name :entity/type])
              :where
              [?e :entity/name ?]] )
        raw (if s
              (d/q q db s)
              (d/q q db))]
    (->> raw
         (map first)
         (sort-by :entity/name)
         vec)))

(defn neighbors
  "Return entities directly connected to entity-id, along with relation metadata."
  [conn-or-db entity-id]
  (let [db (conn->db conn-or-db)
        outgoing (d/q '[:find ?rel (pull ?dst [:entity/id :entity/name :entity/type])
                        :in $ ?id
                        :where
                        [?src :entity/id ?id]
                        [?r :relation/src ?src]
                        [?r :relation/dst ?dst]
                        [?r :relation/type ?rel]]
                      db entity-id)
        incoming (d/q '[:find ?rel (pull ?src [:entity/id :entity/name :entity/type])
                        :in $ ?id
                        :where
                        [?dst :entity/id ?id]
                        [?r :relation/dst ?dst]
                        [?r :relation/src ?src]
                        [?r :relation/type ?rel]]
                      db entity-id)]
    (vec (concat (map (fn [[rel other]]
                        {:relation rel
                         :direction :out
                         :entity other})
                      outgoing)
                 (map (fn [[rel other]]
                        {:relation rel
                         :direction :in
                         :entity other})
                      incoming)))))

(defn paths
  "Placeholder for path search between entities."
  [& _]
  [])
