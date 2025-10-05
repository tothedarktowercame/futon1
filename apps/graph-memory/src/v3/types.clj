(ns v3.types
  "Tiny type lattice for v3: register types, record subtyping, and check <:."
  (:require [clojure.set :as set]))

(defonce !types
  ;; Registry example:
  ;; {:subtype-of {:ProperNoun? #{:Noun?}
  ;;               :ListOfWord? #{:Vector?}
  ;;               :Person?     #{:ProperNoun?}}
  ;;  :meta       {:Noun? {:desc \"wordnet noun\"}}}
  (atom {:subtype-of {} :meta {}}))

(defn register-type!
  "Ensure `t` exists in the lattice (optionally with metadata)."
  ([t] (register-type! t nil))
  ([t meta]
   (swap! !types
          (fn [{:keys [subtype-of meta] :as reg}]
            {:subtype-of (if (contains? subtype-of t) subtype-of (assoc subtype-of t #{}))
             :meta       (if meta (assoc meta t meta) meta)}))
   t))

(defn add-subtype!
  "Declare a <: b (i.e., a is a subtype of b)."
  [a b]
  (doseq [t [a b]] (register-type! t))
  (swap! !types update-in [:subtype-of a] (fnil conj #{}) b)
  [a b])

(defn parents-of [t] (get-in @!types [:subtype-of t] #{}))

(defn type<=?
  "Return true if a <: b in the transitive closure (or a == b)."
  [a b]
  (or (= a b)
      (loop [front (parents-of a) seen #{}]
        (when (seq front)
          (if (contains? front b)
            true
            (let [next (set/difference (apply set/union (map parents-of front)) seen)]
              (recur next (set/union seen front))))))))

;; --- optional: learn subtyping from evidence -------------------------

(defn learn-subtype!
  "Heuristic: if every node tagged with `a` is also often tagged with `b`, propose a <: b.
   Provide a predicate `nodes-with-type` that returns seq of nodes for a given type."
  [{:keys [nodes-with-type threshold]} a b]
  (let [as (set (map :id (nodes-with-type a)))
        bs (set (map :id (nodes-with-type b)))
        overlap (count (set/intersection as bs))
        cov     (double (/ (max 1 (count as))))]
    (when (and (pos? (count as))
               (>= overlap (or threshold 3)))
      (add-subtype! a b))))
