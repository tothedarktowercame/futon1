(ns app.xt.recipes
  "REPL-friendly helpers for querying XTDB mirrors."
  (:require [app.xt :as xt]
            [clojure.string :as str])
  (:import (java.util UUID)))

(def ^:private entity-pull
  [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen :entity/pinned?])

(def ^:private relation-pull
  [:relation/id :relation/type :relation/provenance :relation/last-seen
   {:relation/src [:entity/id :entity/name :entity/type]}
   {:relation/dst [:entity/id :entity/name :entity/type]}])

(defn- normalize-id [v]
  (cond
    (nil? v) nil
    (uuid? v) v
    (instance? UUID v) v
    (keyword? v) v
    (string? v) (let [trimmed (str/trim v)]
                  (when (seq trimmed)
                    (try
                      (UUID/fromString trimmed)
                      (catch Exception _
                        (keyword trimmed)))))
    :else v))

(defn- normalize-type [v]
  (cond
    (keyword? v) v
    (string? v) (-> v str/trim str/lower-case keyword)
    :else v))

(defn- entity-summary [doc]
  (when-let [id (:entity/id doc)]
    (let [name (:entity/name doc)
          type (:entity/type doc)
          seen (:entity/seen-count doc)
          last (:entity/last-seen doc)
          pinned? (when (contains? doc :entity/pinned?)
                    (boolean (:entity/pinned? doc)))]
      (cond-> {:id id}
        name (assoc :name name)
        type (assoc :type (if (keyword? type) type (keyword (name type))))
        (some? seen) (assoc :seen-count (long seen))
        (some? last) (assoc :last-seen (long last))
        (some? pinned?) (assoc :pinned? pinned?)))))

(defn- relation-summary [doc direction]
  (let [src (:relation/src doc)
        dst (:relation/dst doc)]
    (cond-> {:id (:relation/id doc)
             :type (:relation/type doc)
             :src {:id (:entity/id src)
                   :name (:entity/name src)
                   :type (:entity/type src)}
             :dst {:id (:entity/id dst)
                   :name (:entity/name dst)
                   :type (:entity/type dst)}
             :direction direction}
      (:relation/provenance doc) (assoc :provenance (:relation/provenance doc))
      (:relation/last-seen doc) (assoc :last-seen (long (:relation/last-seen doc))))))

(defn- pull-entities []
  (->> (xt/q {:find '[(pull ?e ?pattern)]
              :in ['?pattern]
              :where '[[?e :entity/id _]]}
             entity-pull)
       (map first)
       (map entity-summary)
       (remove nil?)))

(defn- pull-relations [clauses target]
  (->> (xt/q {:find '[(pull ?r ?pattern)]
              :in ['?target '?pattern]
              :where (into '[[?r :relation/id _]] clauses)}
             target
             relation-pull)
       (map first)))

(defn- dedupe-by-id [rels]
  (loop [seen #{}
         acc []
         remaining rels]
    (if (empty? remaining)
      acc
      (let [rel (first remaining)
            rel-id (:id rel)]
        (if (or (nil? rel-id) (seen rel-id))
          (recur seen acc (rest remaining))
          (recur (conj seen rel-id) (conj acc rel) (rest remaining)))))))

(defn list-entities
  "Return entity summaries sorted for REPL use.

  Options:
  - `:limit` (default 25)
  - `:type` keyword/string filter
  - `:sort` one of `:recent` (default), `:seen`, or `:name`."
  ([] (list-entities {}))
  ([{:keys [limit type sort]}]
   (let [limit (or limit 25)
         sort (or sort :recent)
         wanted-type (some-> type normalize-type)
         sorter (case sort
                  :seen (fn [{:keys [seen-count name]}]
                          [(- (or seen-count 0)) (or name "")])
                  :name (fn [{:keys [name]}]
                          [(some-> name str/lower-case) (or name "")])
                  (fn [{:keys [pinned? last-seen seen-count name]}]
                    [(if pinned? 0 1)
                     (- (or last-seen 0))
                     (- (or seen-count 0))
                     (or name "")]))]
     (->> (pull-entities)
          (filter (fn [ent]
                    (if wanted-type
                      (= wanted-type (:type ent))
                      true)))
          (sort-by sorter)
          (take limit)
          vec))))

(defn entity-relations
  "Return relations touching `entity-id`.

  Options:
  - `:direction` `:outgoing`, `:incoming`, or `:any` (default)
  - `:limit` maximum relations to return (default 10 per direction when `:any`)."
  ([entity-id]
   (entity-relations entity-id {}))
  ([entity-id {:keys [direction limit]
               :or {direction :any limit 10}}]
   (when-let [target (normalize-id entity-id)]
     (let [limit (or limit 10)
           outgoing (when (#{:any :outgoing} direction)
                      (->> (pull-relations '[[?r :relation/src ?src]
                                             [?src :entity/id ?target]]
                                            target)
                           (map #(relation-summary % :outgoing))
                           (take limit)
                           (into [])))
           incoming (when (#{:any :incoming} direction)
                      (->> (pull-relations '[[?r :relation/dst ?dst]
                                             [?dst :entity/id ?target]]
                                            target)
                           (map #(relation-summary % :incoming))
                           (take limit)
                           (into [])))
           combined (cond
                      (= direction :outgoing) outgoing
                      (= direction :incoming) incoming
                      :else (dedupe-by-id (concat outgoing incoming)))]
       (vec combined)))))

(defn counts
  "Return the entity and relation counts stored in XT."
  []
  {:entities (or (ffirst (xt/q '{:find [(count ?e)]
                                  :where
                                  [?e :entity/id _]}))
                 0)
   :relations (or (ffirst (xt/q '{:find [(count ?r)]
                                   :where
                                   [?r :relation/id _]}))
                  0)})
