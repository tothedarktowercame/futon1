(ns v2.graph-memory
  "v2 — ultra-light in-memory graph store for chat/NLP demos.
   Nodes have :id, :label, :types (a set), and :evidence (type -> count).
   Edges are {:src id :rel keyword :dst id :w number}."
  (:require [clojure.edn :as edn])
  (:import (java.util UUID)))

;; -----------------------------------------------------------------------------
;; Store

(defonce !g
  (atom {:nodes {}   ;; id -> {:id .. :label .. :types #{..} :evidence {type n}}
         :edges []})) ;; vector of {:src :rel :dst :w}

(defn- uuid [] (UUID/randomUUID))

(defn reset-db!
  "Reset the in-memory graph (useful for tests)."
  []
  (clojure.core/reset! !g {:nodes {} :edges []}))

;; -----------------------------------------------------------------------------
;; Nodes & edges

(defn add-node!
  "Add a new node with a fresh id. Returns id."
  [{:keys [label types]}]
  (let [id (uuid)]
    (swap! !g assoc-in [:nodes id]
           {:id id :label label :types (set types) :evidence {}})
    id))

(defn upsert-node-by-label!
  "Return id of node with `label`, creating if needed."
  [label]
  (or (some (fn [[i n]] (when (= label (:label n)) i))
            (:nodes @!g))
      (add-node! {:label label :types #{}})))

(defn add-edge!
  "Add an edge; default weight 1."
  ([src rel dst] (add-edge! src rel dst 1))
  ([src rel dst w]
   (swap! !g update :edges conj {:src src :rel rel :dst dst :w w})
   {:src src :rel rel :dst dst :w w}))

(declare nodes edges)

(defn labels
  "Return the set of node labels currently in the graph."
  []
  (set (map :label (nodes))))

(defn summary
  "Return a minimal summary of nodes and edges for inspection."
  []
  {:nodes (->> (nodes)
               (map (fn [{:keys [id label types evidence]}]
                      {:id id
                       :label label
                       :types types
                       :evidence evidence}))
               vec)
   :edges (vec (edges))})

;; -----------------------------------------------------------------------------
;; Types & evidence

(def default-threshold 2)

(defn bump-type!
  "Increment evidence for type `t` on node `id`. Promotes into :types at threshold."
  ([id t] (bump-type! id t default-threshold))
  ([id t threshold]
   (swap! !g update-in [:nodes id :evidence t] (fnil inc 0))
   (when (>= (get-in @!g [:nodes id :evidence t]) threshold)
     (swap! !g update-in [:nodes id :types] conj t))
   id))

;; -----------------------------------------------------------------------------
;; Query helpers

(defn node
  "Fetch node by id."
  [id] (get-in @!g [:nodes id]))

(defn nodes
  "Seq of all nodes."
  []
  (vals (:nodes @!g)))

(defn edges
  "Seq of all edges."
  []
  (:edges @!g))

(defn edges-from
  "Edges originating at id (optionally filtered by rel)."
  ([id] (filter #(= id (:src %)) (edges)))
  ([id rel] (filter #(and (= id (:src %)) (= rel (:rel %))) (edges))))

(defn edges-to
  "Edges targeting id (optionally filtered by rel)."
  ([id] (filter #(= id (:dst %)) (edges)))
  ([id rel] (filter #(and (= id (:dst %)) (= rel (:rel %))) (edges))))

(defn find-by-type
  "All nodes that include type `t` in :types."
  [t]
  (filter #(contains? (:types %) t) (nodes)))

(defn last-utterances
  "Return the most recent N utterance node ids (by edge order heuristic)."
  [n]
  (->> (edges)
       (filter #(= :links-to (:rel %)))
       (map :src)
       (distinct)
       (take-last n)))

;; -----------------------------------------------------------------------------
;; Persistence (EDN)

(defn save!
  [path]
  (spit path (pr-str @!g))
  path)

(defn load!
  [path]
  (reset! !g (edn/read-string (slurp path)))
  @!g)
