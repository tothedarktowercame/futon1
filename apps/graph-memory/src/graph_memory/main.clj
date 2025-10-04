(ns graph-memory.main
  (:require [datascript.core :as d])
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
   :link/to         {:db/valueType :db.type/ref}})

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
