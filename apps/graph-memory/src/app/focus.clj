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
  "Query XT for neighbors touching any eid in `seed-ids`."
  [xt-db seed-ids]
  (let [out-q
        '[:find ?rel ?nid ?nname
          :in $ [?eid ...]
          :where
          [?r :relation/src ?eid]
          [?r :relation/type ?rel]
          [?r :relation/dst ?n]
          [?n :entity/id ?nid]
          [?n :entity/name ?nname]]

        in-q
        '[:find ?rel ?nid ?nname
          :in $ [?eid ...]
          :where
          [?r :relation/dst ?eid]
          [?r :relation/type ?rel]
          [?r :relation/src ?n]
          [?n :entity/id ?nid]
          [?n :entity/name ?nname]]

        outs (if (seq seed-ids) (xt/q out-q seed-ids) [])
        ins  (if (seq seed-ids) (xt/q in-q  seed-ids) [])]
    (concat
     (map (fn [[rel nid nname]]
            {:relation rel :direction :out
             :neighbor nname :neighbor-id nid})
          outs)
     (map (fn [[rel nid nname]]
            {:relation rel :direction :in
             :neighbor nname :neighbor-id nid})
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
   - opts: {:k <int>}"
  [ds-conn-or-db xt-db entity-id {:keys [k] :or {k 3}}]
  (assert (some? xt-db) "top-neighbors: xt-db is nil")
  (assert (some? entity-id) "top-neighbors: entity-id is nil")

  (let [ds-db         (if (d/db? ds-conn-or-db) ds-conn-or-db @ds-conn-or-db)
        activated-ids (set (or (ds-activated-ids ds-db) []))
        seed-ids      (cond-> activated-ids (some? entity-id) (conj entity-id))
        rows          (if (seq seed-ids)
                        (or (xt-neighbor-rows-for-ids xt-db seed-ids) [])
                        [])
        ;; Safely realize rows that might be futures; drop nils/errors.
        realized-rows (->> rows
                           (keep (fn [x]
                                   (cond
                                     (instance? java.util.concurrent.Future x)
                                     (try @x (catch Throwable _ nil))
                                     :else x)))
                           (remove nil?))
        ;; Normalize for stable sorting even if some keys are missing.
        normalized    (map (fn [row]
                             (-> row
                                 (update :relation  #(or % :unknown))
                                 (update :neighbor  #(or % ""))
                                 (update :direction #(or % :out))))
                           realized-rows)]
    (->> normalized
         distinct
         (sort-by (juxt :relation :neighbor :direction))
         (take k))))
