(ns v3.matcher
  (:require [v3.types :as T]))

(defn satisfies-type?
  "Does a node’s concrete type set satisfy a required abstract type?"
  [have-types required]
  (some #(T/type<=? % required) have-types))

(defn witness-path
  "Return a justification chain a <: ... <: b, or nil."
  [a b]
  (when (T/type<=? a b)
    (loop [paths [[a]] seen #{}]
      (when-let [p (first paths)]
        (if (= (peek p) b)
          p
          (let [exp (for [q (T/parents-of (peek p)) :when (not (seen q))] (conj p q))]
            (recur (into (subvec paths 1) exp) (conj seen (peek p)))))))))
