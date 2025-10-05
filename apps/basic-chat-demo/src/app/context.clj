(ns app.context
  "Context utilities for printing nearby graph neighbors."
  (:require [clojure.string :as str]
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

(defn enrich-with-neighbors
  "Return a flattened vector of top neighbors for the provided entities."
  [conn entities {:keys [neighbors context-cap context?]
                  :or {neighbors 3 context-cap 10 context? true}}]
  (when context?
    (->> (distinct-by :entity-id entities)
         (mapcat #(top-neighbors conn % {:k neighbors}))
         (remove nil?)
         (take context-cap)
         vec)))

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
