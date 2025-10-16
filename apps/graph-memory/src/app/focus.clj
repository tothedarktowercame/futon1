(ns app.focus
  (:require [app.xt :as xt]
            [clojure.math :as math]
            [clojure.set :as set]
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
  "Return ranked neighbors for focus entity with per-type caps."
  [_node focus-ids {:keys [k-per-anchor per-edge-caps per-type-caps allow-works? allowed-types time-hint] :as _opts}]
  (let [db (xt/db)
        now (now-ms)
        focus-id-set (set (if (coll? focus-ids) focus-ids [focus-ids]))
        cutoff (or time-hint (- now (* 30 24 60 60 1000)))
        focus-window-ms (max 1 (- now cutoff))
        type-pred (allowed-type-pred allowed-types)
        allow-all? (or allow-works? (nil? type-pred))
        out-docs (map first (xt/q '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst :relation/confidence :relation/last-seen :relation/provenance :relation/subject :relation/object])]
                                    :in [$ [?focus ...]]
                                    :where [[?r :relation/src ?focus]]}
                                   focus-id-set))
        in-docs  (map first (xt/q '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst :relation/confidence :relation/last-seen :relation/provenance :relation/subject :relation/object])]
                                    :in [$ [?focus ...]]
                                    :where [[?r :relation/dst ?focus]]}
                                   focus-id-set))
        raw (concat (map #(assoc % :direction :out :target-id (:relation/dst %)) out-docs)
                    (map #(assoc % :direction :in :target-id (:relation/src %)) in-docs))
        grouped (group-by :relation/type raw)
        summarize (fn [[rel-type rels]]
                    (let [cap (get (or per-edge-caps per-type-caps {}) rel-type k-per-anchor)]
                      (->> rels
                           (keep (fn [{:keys [target-id] :as rel}]
                                   (let [confidence (:relation/confidence rel)
                                         last-seen (:relation/last-seen rel)]
                                     (when-let [neighbor (fetch-entity db target-id)]
                                       (when (or allow-all?
                                                 (type-pred (:entity/type neighbor)))
                                         (let [score (neighbor-score now focus-window-ms {:confidence confidence
                                                                                           :last-seen (safe-long last-seen)})]
                                           {:relation/id (:relation/id rel)
                                            :relation/type rel-type
                                            :focus-id (:relation/src rel)
                                            :direction (:direction rel)
                                            :neighbor neighbor
                                            :confidence (double (or confidence 1.0))
                                            :last-seen (safe-long last-seen)
                                            :provenance (:relation/provenance rel)
                                            :subject (:relation/subject rel)
                                            :object (:relation/object rel)
                                            :score score}))))))
                           (sort-by (comp - :score))
                           (take (or cap k-per-anchor)))))
        limited (->> grouped
                     (mapcat summarize)
                     (sort-by (comp - :score))
                     (take (or k-per-anchor 5)))]
    (vec limited)))
