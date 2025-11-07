(ns graph-memory.main
  (:require [clojure.string :as str]
            [datascript.core :as d]
            [graph-memory.types-registry :as types])
  (:import (java.util UUID)))

(def schema
  {:utterance/id    {:db/unique :db.unique/identity}
   :utterance/text  {}
   :utterance/ts    {}
   :utterance/intent {}
   :utterance/intent-conf {}
   :utterance/intent-source {}
   :intent/id       {:db/unique :db.unique/identity}
   :intent/data     {}
   :intent/type     {}
   :intent/confidence {}
   :intent/source   {}
   :intent/candidates {}
   :link/id         {:db/unique :db.unique/identity}
   :link/type       {}
   :link/from       {:db/valueType :db.type/ref}
   :link/to         {:db/valueType :db.type/ref}
   :entity/id       {:db/unique :db.unique/identity}
   :entity/name     {:db/unique :db.unique/identity}
   :entity/type     {}
   :entity/last-seen {:db/index true}
   :entity/seen-count {}
   :entity/pinned? {}
   :mention/id      {:db/unique :db.unique/identity}
   :mention/utterance {:db/valueType :db.type/ref}
   :mention/entity  {:db/valueType :db.type/ref}
   :mention/span    {}
   :relation/id     {:db/unique :db.unique/identity}
   :relation/type   {}
   :relation/src    {:db/valueType :db.type/ref}
   :relation/dst    {:db/valueType :db.type/ref}
   :relation/provenance {}
   :relation/confidence {}
   :relation/last-seen {:db/index true}})

(defn init-db []
  (d/create-conn schema))

(defn- transact! [conn tx]
  (let [{:keys [db-after tempids]} (d/transact! conn tx)]
    {:db-after db-after
     :resolve (fn [tmp]
                (d/resolve-tempid db-after tempids tmp))}))

(defn- normalize-source [source]
  (cond
    (keyword? source) source
    (string? source) (let [trimmed (str/trim source)]
                       (when (seq trimmed)
                         (keyword trimmed)))
    :else nil))

(defn add-utterance!
  ([conn text ts]
   (add-utterance! conn text ts nil))
  ([conn text ts {:keys [intent]}]
   (let [intent-type (some-> intent :type)
         intent-type (when intent-type (if (keyword? intent-type)
                                         intent-type
                                         (keyword (name intent-type))))
         intent-conf (some-> intent :conf)
         intent-conf (when (number? intent-conf) (double intent-conf))
         intent-source (some-> intent :source normalize-source)
         _ (when intent-type (types/ensure! :intent intent-type))
         id (UUID/randomUUID)
         tmp (d/tempid :db.part/user)
         tx-result (transact! conn [(cond-> {:db/id tmp
                                             :utterance/id id
                                             :utterance/text text
                                             :utterance/ts ts}
                                      intent-type (assoc :utterance/intent intent-type)
                                      intent-conf (assoc :utterance/intent-conf intent-conf)
                                      intent-source (assoc :utterance/intent-source intent-source))])
         eid ((:resolve tx-result) tmp)
         base {:id id
               :db/eid eid
               :text text
               :ts ts}]
     (cond-> base
       intent-type (assoc :intent intent-type)
       intent-conf (assoc :intent-conf intent-conf)
       intent-source (assoc :intent-source intent-source)
       intent (assoc :intent-data intent)))))

(defn- clean-candidate [cand]
  (select-keys cand [:type :conf :score]))

(defn add-intent! [conn intent]
  (let [intent-type (some-> intent :type)
        intent-type (when intent-type (if (keyword? intent-type)
                                        intent-type
                                        (keyword (name intent-type))))
        intent-conf (some-> intent :conf)
        intent-conf (when (number? intent-conf) (double intent-conf))
        intent-source (some-> intent :source normalize-source)
        candidates (some-> intent :intent-candidates)
        pruned-candidates (when (seq candidates)
                            (->> candidates
                                 (map clean-candidate)
                                 (remove empty?)
                                 vec))
        _ (when intent-type (types/ensure! :intent intent-type))
        id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        tx-result (transact! conn [(cond-> {:db/id tmp
                                            :intent/id id
                                            :intent/data intent}
                                     intent-type (assoc :intent/type intent-type)
                                     intent-conf (assoc :intent/confidence intent-conf)
                                     intent-source (assoc :intent/source intent-source)
                                     (seq pruned-candidates) (assoc :intent/candidates pruned-candidates))])
        eid ((:resolve tx-result) tmp)]
    {:id id
     :db/eid eid
     :data intent
     :type intent-type
     :conf intent-conf}))

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
            tx-result (transact! conn [{:db/id tmp
                                        :entity/id entity-id
                                        :entity/name name
                                        :entity/type type}])
            eid ((:resolve tx-result) tmp)]
        {:id entity-id
         :name name
         :type type
         :db/eid eid}))))

(defn add-mention!
  "Associate an utterance with an entity. span is an inclusive-exclusive vector."
  [conn utterance-id entity-id span]
  (let [mention-id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        tx-result (transact! conn [{:db/id tmp
                                    :mention/id mention-id
                                    :mention/utterance [:utterance/id utterance-id]
                                    :mention/entity   [:entity/id entity-id]
                                    :mention/span     span}])
        eid ((:resolve tx-result) tmp)]
    {:id mention-id
     :span span
     :db/eid eid}))

(defn add-relation!
  "Create a relation between two entities with optional provenance."
  [conn {:keys [type src-id dst-id prov]}]
  (let [rel-id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        tx-result (transact! conn [{:db/id tmp
                                    :relation/id rel-id
                                    :relation/type type
                                    :relation/src [:entity/id src-id]
                                    :relation/dst [:entity/id dst-id]
                                    :relation/provenance prov}])
        eid ((:resolve tx-result) tmp)]
    {:id rel-id
     :type type
     :db/eid eid}))

(defn link! [conn from-id to-id edge-type]
  (let [id (UUID/randomUUID)
        tmp (d/tempid :db.part/user)
        tx-result (transact! conn [{:db/id tmp
                                    :link/id id
                                    :link/type edge-type
                                    :link/from from-id
                                    :link/to to-id}])
        eid ((:resolve tx-result) tmp)]
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
              [?e :entity/name ?]])
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
