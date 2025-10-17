(ns app.focus
  (:require [app.xt :as xt]
            [clojure.math :as math]
            [clojure.set :as set]
            [datascript.core :as d]
            [graph-memory.types-registry :as types]
            [xtdb.api :as xtdb]))

(def default-allowed-types
  "Default set of entity types that the focus heuristics consider eligible."
  #{:person :org :project :place})
(def allowed-types
  "Deprecated alias maintained for consumers expecting `allowed-types`."
  default-allowed-types)

(def ^:private type-weights
  {:person 1.0
   :org 0.8
   :project 0.7
   :place 0.6})

(defn safe-deref
  "Return @f if f is a Future, else nil. Never throws on nil."
  [f]
  (when (instance? java.util.concurrent.Future f)
    (deref f)))

;; If you want a completed 'no-op' future for uniformity:
(defn done-future
  "A completed future with value v (default nil)."
  ([] (done-future nil))
  ([v] (let [p (promise)] (deliver p v) p)))

(defn ds-activated-ids
  "Return a set of entity-ids considered 'activated' in the in-memory DS db."
  [dsdb]
  (into #{}
        (map first)
        (d/q '[:find ?id
               :where
               [?e :entity/id ?id]
               [?e :entity/pinned? true]]
             dsdb)))

(defn xt-neighbor-rows-for-ids
  "Query XT for neighbors touching any eid in `seed-ids`.
   Returns rows shaped for scoring + printing:
   {:relation kw
    :direction :out|:in
    :neighbor {:id uuid :name str :type kw}
    :confidence double?
    :last-seen long?
    :subject str? :object str? :provenance map?}"
  [xt-db seed-ids]
  (let [out-q
        '[:find (pull ?r [:relation/type :relation/confidence :relation/last-seen
                          :relation/subject :relation/object :relation/provenance])
          (pull ?n [:entity/id :entity/name :entity/type])
          :in $ [?eid ...]
          :where
          [?r :relation/src ?eid]
          [?r :relation/dst ?n]]

        in-q
        '[:find (pull ?r [:relation/type :relation/confidence :relation/last-seen
                          :relation/subject :relation/object :relation/provenance])
          (pull ?n [:entity/id :entity/name :entity/type])
          :in $ [?eid ...]
          :where
          [?r :relation/dst ?eid]
          [?r :relation/src ?n]]

        outs (if (seq seed-ids) (xt/q out-q seed-ids) [])
        ins  (if (seq seed-ids) (xt/q in-q  seed-ids) [])]
    (concat
     (map (fn [[r n]]
            {:relation   (:relation/type r)
             :direction  :out
             :neighbor   {:id (:entity/id n)
                          :name (:entity/name n)
                          :type (:entity/type n)}
             :confidence (:relation/confidence r)
             :last-seen  (:relation/last-seen r)
             :subject    (:relation/subject r)
             :object     (:relation/object r)
             :provenance (:relation/provenance r)})
          outs)
     (map (fn [[r n]]
            {:relation   (:relation/type r)
             :direction  :in
             :neighbor   {:id (:entity/id n)
                          :name (:entity/name n)
                          :type (:entity/type n)}
             :confidence (:relation/confidence r)
             :last-seen  (:relation/last-seen r)
             :subject    (:relation/subject r)
             :object     (:relation/object r)
             :provenance (:relation/provenance r)})
          ins))))

(defn- now-ms [] (System/currentTimeMillis))

(defn- safe-long [v]
  (cond
    (instance? Number v) (long v)
    (some? v) (try (Long/parseLong (str v)) (catch NumberFormatException _ nil))
    :else nil))

(defn- coalesce-entity
  [entity]
  (-> entity
      (update :entity/type (fn [t] (when t (keyword t))))
      (update :entity/seen-count #(some-> % long))
      (update :entity/last-seen #(some-> % long))
      (update :entity/pinned? #(true? %))))

(defn- allowed-type-pred [allowed]
  (cond
    (nil? allowed) nil
    (ifn? allowed) allowed
    :else (types/effective-pred :entity allowed)))

(defn- fetch-entity
  [db eid]
  (some-> (xtdb/entity db eid)
          (select-keys [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?])
          coalesce-entity))

(defn- candidate-score
  [now focus-window-ms anchor-set entity]
  (let [id (:entity/id entity)
        type (:entity/type entity)
        last-seen (:entity/last-seen entity)
        seen-count (:entity/seen-count entity)
        pinned? (boolean (:entity/pinned? entity))
        anchor? (contains? anchor-set id)
        age-ms (when last-seen (- now last-seen))
        window (double (max focus-window-ms 1))
        recency (if (and age-ms (<= age-ms focus-window-ms))
                  (- 1.0 (/ age-ms window))
                  0.0)
        seen (double (or seen-count 0))
        count-score (math/log1p seen)
        type-score (get type-weights type 0.5)]
    (+ (* 1.5 recency)
       (* 0.8 count-score)
       (* 1.2 type-score)
       (if anchor? 3.0 0.0)
       (if pinned? 4.0 0.0))))

(defn focus-candidates
  "Return top K_e focus entities ordered by salience. Anchors always included."
  ([_node anchors cutoff-ms k-e]
   (focus-candidates _node anchors cutoff-ms k-e {}))
  ([_node anchors cutoff-ms k-e {:keys [allowed-types] :as _opts}]
   (let [db (xt/db)
         now (now-ms)
         anchor-set (set anchors)
         cutoff (or cutoff-ms (- now (* 30 24 60 60 1000)))
         focus-window-ms (max 1 (- now cutoff))
         type-pred (allowed-type-pred allowed-types)
         raw (xt/q '{:find [(pull ?e [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?])]
                     :where [[?e :entity/id _]
                             [?e :entity/type ?t]]})
         base (map (comp coalesce-entity first) raw)
         base (if type-pred
                (filter (fn [entity]
                          (let [etype (:entity/type entity)]
                            (or (nil? etype)
                                (type-pred etype))))
                        base)
                base)
         anchors-from-db (->> anchors
                              (keep #(fetch-entity db %)))
         combined (->> (concat base anchors-from-db)
                       (reduce (fn [acc e]
                                 (assoc acc (:entity/id e) e))
                               {}))
         qualifies? (fn [{:entity/keys [id last-seen pinned? type]}]
                      (let [anchor? (anchor-set id)]
                        (and (or type anchor? pinned?)
                             (or anchor?
                                 pinned?
                                 (and last-seen (>= last-seen cutoff))))))
         candidates (->> combined
                         vals
                         (filter qualifies?)
                         (map (fn [entity]
                                (let [score (candidate-score now focus-window-ms anchor-set entity)]
                                  {:id (:entity/id entity)
                                   :entity entity
                                   :anchor? (anchor-set (:entity/id entity))
                                   :pinned? (boolean (:entity/pinned? entity))
                                   :score score})))
                         (sort-by (comp - :score)))
         result (if k-e (take k-e candidates) candidates)]
     (vec result))))

(defn- neighbor-score
  [now focus-window-ms {:keys [confidence last-seen]}]
  (let [conf (double (or confidence 1.0))
        age (when last-seen (- now last-seen))
        recency (if (and age (<= age focus-window-ms))
                  (- 1.0 (/ age (double focus-window-ms)))
                  0.0)]
    (+ conf (* 1.2 recency))))

(defn top-neighbors
  "Pick the top-k neighbors for an entity-id using DS activation -> XT neighbors.
   - ds-conn-or-db: Datascript conn or db
   - xt-db: XTDB db snapshot (from xta/db or your wrapper)
   - entity-id: UUID of the focal entity
   - opts: {:k <int> | :k-per-anchor <int> :time-hint <ms>}"
  [ds-conn-or-db xt-db entity-id {:keys [k k-per-anchor time-hint] :as _opts}]
  (let [k (or k k-per-anchor 3)]
    (assert (some? xt-db) "top-neighbors: xt-db is nil")
    (assert (some? entity-id) "top-neighbors: entity-id is nil")

    ;; Local helper: realize x if it's a Future, otherwise pass-through.
    ;; If a future yields a seq (e.g., a batch of rows), we'll flatten later.
    (letfn [(realize-maybe [x]
              (cond
                (instance? java.util.concurrent.Future x)
                (try @x (catch Throwable _ nil))
                :else x))]

      (let [;; Accept either a DS db or a conn
            ds-db         (try
                            (if (datascript.core/db? ds-conn-or-db)
                              ds-conn-or-db
                              @ds-conn-or-db)
                            (catch Throwable _
                              ;; Fallback: if it's neither, just use it as-is and
                              ;; let downstream fail loudly in tests.
                              ds-conn-or-db))

            ;; We keep DS activation for possible future filtering; not strictly used below.
            _activated-ids (set (or (ds-activated-ids ds-db) []))

            raw-rows      (or (xt-neighbor-rows-for-ids xt-db #{entity-id}) [])
            realized-rows (->> raw-rows
                               (map realize-maybe)
                               (remove nil?)
                               (mapcat (fn [v] (if (sequential? v) v [v]))))

            ;; hydrate neighbor map from XT if needed, and compute score
            now           (now-ms)
            cutoff        (or time-hint (- now (* 30 24 60 60 1000))) ;; default 30d
            window-ms     (max 1 (- now cutoff))
            enriched      (map
                           (fn [row]
                             (let [conf  (double (or (:confidence row) 1.0))
                                   ls    (safe-long (:last-seen row))
                                   score (neighbor-score now window-ms {:confidence conf
                                                                        :last-seen  ls})
                                   ;; ensure neighbor is a map {:id :name :type}
                                   row'  (if (and (string? (:neighbor row))
                                                  (:neighbor-id row))
                                           (let [nid (:neighbor-id row)
                                                 n   (or (fetch-entity xt-db nid)
                                                         {:entity/id nid
                                                          :entity/name (:neighbor row)
                                                          :entity/type nil})]
                                             (-> row
                                                 (assoc :neighbor {:id   (:entity/id n)
                                                                   :name (:entity/name n)
                                                                   :type (:entity/type n)})))
                                           row)]
                               (-> row'
                                   (update :relation  #(or % :unknown))
                                   (update :direction #(or % :out))
                                   (assoc  :score (double (or (:score row') score))))))
                           realized-rows)]

        (->> enriched
             distinct
             (sort-by (comp - :score))
             (take k)
             vec)))))
