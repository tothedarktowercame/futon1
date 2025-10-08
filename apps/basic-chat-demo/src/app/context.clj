(ns app.context
  "Context utilities for printing nearby graph neighbors."
  (:require [app.focus :as focus]
            [clojure.string :as str]
            [graph-memory.main :as gm]))

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
  "Return a flattened vector of top neighbors for the provided entities."
  [conn entities {:keys [neighbors context-cap context? anchors timestamp focus-days per-type-caps allow-works? focus-limit]
                  :or {neighbors 3 context-cap 10 context? true focus-days 30}}]
  (when context?
    (let [anchor-ids (->> anchors (map :id) (remove nil?) set)]
      (if (seq anchor-ids)
        (let [day-ms (* 24 60 60 1000)
              now (or timestamp (System/currentTimeMillis))
              cutoff (- now (* focus-days day-ms))
              focus-count (or focus-limit context-cap)
              candidates (->> (focus/focus-candidates nil anchor-ids cutoff focus-count)
                               (filter #(or (:anchor? %) (:pinned? %))))
              focus-map (into {} (map (fn [{:keys [id entity]}] [id entity]) candidates))
              per-type (or per-type-caps default-per-type-caps)
              neighbor-results (mapcat (fn [{:keys [id]}]
                                         (focus/top-neighbors nil id {:k-per-anchor neighbors
                                                                      :per-type-caps per-type
                                                                      :allow-works? allow-works?
                                                                      :time-hint cutoff}))
                                       candidates)
              trimmed (->> neighbor-results
                           (sort-by (comp - :score))
                           (take context-cap))]
          (vec (keep (fn [{:keys [focus-id neighbor direction] :as entry}]
                       (when-let [focus-entity (focus-map focus-id)]
                         {:entity (:entity/name focus-entity)
                          :entity-id (:entity/id focus-entity)
                          :neighbor (:entity/name neighbor)
                          :neighbor-id (:entity/id neighbor)
                          :neighbor-type (:entity/type neighbor)
                          :relation (:relation/type entry)
                          :direction direction}))
                     trimmed)))
        (->> (distinct-by :entity-id entities)
             (mapcat #(top-neighbors conn % {:k neighbors}))
             (remove nil?)
             (take context-cap)
             vec)))))

(defn render-context
  "Render context neighbor entries into a multi-line string."
  [entries]
  (when (seq entries)
    (let [line (fn [{:keys [entity neighbor relation direction]}]
                 (let [arrow (if (= direction :out) "->" "<-")
                       bracket (name relation)]
                   (format "%s %s[%s]%s %s"
                           entity
                           (if (= arrow "->") "-" "<-")
                           bracket
                           arrow
                           neighbor)))]
      (str "context:\n" (str/join "\n" (map line entries))))))
