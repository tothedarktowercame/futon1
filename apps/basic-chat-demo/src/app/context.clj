(ns app.context
  "Context utilities for printing nearby graph neighbors."
  (:require [app.focus :as focus]
            [clojure.string :as str]
            [graph-memory.main :as gm]
            [xtdb.api :as xt]))

(defn- distinct-by [f coll]
  (let [seen (volatile! #{})]
    (filter (fn [item]
              (let [k (f item)]
                (when (and k (not (@seen k)))
                  (vswap! seen conj k)
                  true)))
            coll)))

(defn neighbors
  "Return neighbor maps for the given entity-id."
  [conn entity-id]
  (if (nil? entity-id)
    []
    (->> (gm/neighbors conn entity-id)
         (map (fn [{:keys [relation direction entity]}]
                (when-let [neighbor-name (:entity/name entity)]
                  {:relation relation
                   :direction direction
                   :neighbor entity
                   :neighbor-name neighbor-name})))
         (remove nil?)
         (distinct-by (juxt :relation :direction :neighbor-name)))))

(defn top-neighbors
  "Pick the top-k neighbors for an entity map that includes :entity-id and :name."
  [conn {:keys [entity-id name]} {:keys [k] :or {k 3}}]
  (when (and entity-id name)
    (->> (neighbors conn entity-id)
         (sort-by (juxt :relation :neighbor-name :direction))
         (take k)
         (map (fn [{:keys [relation direction neighbor neighbor-name]}]
                {:entity name
                 :entity-id entity-id
                 :neighbor neighbor-name
                 :neighbor-id (:entity/id neighbor)
                 :neighbor-type (:entity/type neighbor)
                 :relation relation
                 :direction direction})))))

(def ^:private default-per-type-caps
  {:member-of 2
   :advisor-of 2
   :affiliated-with 2
   :performed-in 1
   :created 0})

(defn enrich-with-neighbors
  "Return a flattened vector of top neighbors for the provided entities.

   NOTE: The first argument is an XTDB DB **snapshot** (xt-db), not a node.
   If you still have call sites that pass a node, update them to pass (xtdb.api/db node)."
  [xt-db conn entities
   {:keys [neighbors context-cap context? anchors timestamp focus-days per-type-caps allow-works? focus-limit]
    :or   {neighbors 3, context-cap 10, context? true, focus-days 30}}]
  (when context?
    (let [day-ms      (* 24 60 60 1000)
          now         (or timestamp (System/currentTimeMillis))
          cutoff      (- now (* focus-days day-ms))
          focus-count (or focus-limit context-cap)
          ;; Build candidate list directly from the provided anchors (no need for focus-candidates)
          anchor-cands (->> (or anchors [])
                            (map (fn [a]
                                   (when-let [id (:id a)]
                                     {:id id :entity a :anchor? true :pinned? (:pinned? a)})))
                            (remove nil?)
                            vec)
          per-type     (or per-type-caps default-per-type-caps)
          ;; Helper: run top-neighbors for one candidate id with safe opts
          neighbors-for (fn [{:keys [id]}]
                          (when id
                            (focus/top-neighbors
                             conn xt-db id
                             {:k-per-anchor  (max 1 neighbors)
                              :per-type-caps per-type
                              :allow-works?  allow-works?
                              :time-hint     cutoff})))]

      (if (seq anchor-cands)
        ;; Use anchors as the foci
        (let [focus-map        (into {} (map (fn [{:keys [id entity]}] [id entity]) anchor-cands))
              neighbor-results (mapcat neighbors-for anchor-cands)
              ;; robust sort: nil-safe, descending by score
              trimmed          (->> neighbor-results
                                    (map #(update % :score (fnil double 0.0)))
                                    (sort-by :score >)
                                    (take focus-count))]
          (vec
           (keep (fn [{:keys [focus-id neighbor direction relation/type] :as entry}]
                   (when-let [focus-entity (get focus-map focus-id)]
                     {:entity       (:entity/name focus-entity)
                      :entity-id    (:entity/id focus-entity)
                      :neighbor     (:entity/name neighbor)
                      :neighbor-id  (:entity/id neighbor)
                      :neighbor-type (:entity/type neighbor)
                      :relation     (:relation/type entry)  ; keep the original key if present
                      :direction    direction}))
                 trimmed)))

        ;; No anchors: fall back to distinct seeds from `entities`
        (let [seed-ents (->> (or entities [])
                             (distinct-by :entity-id)   ;; assumes you have distinct-by; if not, replace with a set-based impl
                             (take focus-count))
              ;; Seed items are maps like {:entity-id ..., :neighbor ...}? Normalize to ids:
              seed-ids  (->> seed-ents
                             (map :entity-id)
                             (remove nil?)
                             distinct
                             vec)
              ;; Build a minimal focus-map for formatting
              focus-map (into {}
                              (map (fn [e]
                                     (let [eid (:entity-id e)
                                           nm  (or (:entity/name e) (:name e) (:entity e))]
                                       [eid {:entity/id eid :entity/name nm}])))
                              seed-ents)
              neighbor-results (mapcat (fn [eid]
                                         (focus/top-neighbors
                                          conn xt-db eid
                                          {:k-per-anchor  (max 1 neighbors)
                                           :per-type-caps per-type
                                           :allow-works?  allow-works?
                                           :time-hint     cutoff}))
                                       seed-ids)
              trimmed (->> neighbor-results
                           (map #(update % :score (fnil double 0.0)))
                           (sort-by :score >)
                           (take context-cap))]
          (vec
           (keep (fn [{:keys [focus-id neighbor direction] :as entry}]
                   (when-let [focus-entity (get focus-map focus-id)]
                     {:entity        (:entity/name focus-entity)
                      :entity-id     (:entity/id focus-entity)
                      :neighbor      (:entity/name neighbor)
                      :neighbor-id   (:entity/id neighbor)
                      :neighbor-type (:entity/type neighbor)
                      :relation      (:relation/type entry)
                      :direction     direction}))
                 trimmed)))))))

(defn- display-name [x]
  (cond
    (string? x) x
    (map? x)    (or (:entity/name x) (:name x) (str x))
    :else       (str x)))

(defn render-context
  "Render context neighbor entries into a multi-line string.
   Accepts {:entity … :neighbor … :relation kw :direction :in|:out}.
   :entity / :neighbor can be strings or entity maps with :entity/name."
  [entries]
  (when (seq entries)
    (let [line (fn [{:keys [entity neighbor relation direction]}]
                 (let [arrow   (if (= direction :out) "->" "<-")
                       bracket (name relation)]
                   (format "%s %s[%s]%s %s"
                           (display-name entity)
                           (if (= arrow "->") "-" "<-")
                           bracket
                           arrow
                           (display-name neighbor))))]
      (str "context:\n" (str/join "\n" (map line entries))))))
