(ns nlp-interface.ner-er
  "Classical gazetteer-backed NER with simple relation heuristics."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn load-gazetteer
  "Load gazetteer sets for people and places from resources."
  []
  {:people (or (some-> "gazetteer/people.edn" io/resource slurp edn/read-string)
               #{})
   :places (or (some-> "gazetteer/places.edn" io/resource slurp edn/read-string)
               #{})})

(defn- all-spans [tokens]
  (let [n (count tokens)]
    (for [start (range n)
          end (range (inc start) (inc n))]
      [start end])))

(defn- match-type [phrase {:keys [people places]}]
  (cond
    (contains? people phrase) :person
    (contains? places phrase) :place
    :else nil))

(defn- gazetteer-matches [tokens gaz]
  (->> (all-spans tokens)
       (map (fn [[start end :as span]]
              (let [phrase (str/join " " (subvec tokens start end))
                    type (match-type phrase gaz)]
                (when type
                  {:name phrase
                   :type type
                   :span span}))))
       (remove nil?)
       (sort-by (fn [{:keys [span]}]
                  (- (apply - span))))))

(defn- pick-non-overlapping [matches]
  (loop [remaining matches
         used #{}
         acc []]
    (if-let [{:keys [span] :as match} (first remaining)]
      (let [[start end] span
            indices (set (range start end))]
        (if (empty? (set/intersection used indices))
          (recur (rest remaining)
                 (set/union used indices)
                 (conj acc match))
          (recur (rest remaining) used acc)))
      acc)))

(defn ner
  "Return a vector of {:name .. :type .. :span [i j)} entities based on gazetteer matches."
  [tokens _pos gaz]
  (->> (gazetteer-matches tokens gaz)
       pick-non-overlapping
       vec))

(defn relations
  "Simple SVO/preposition heuristic returning {:type kw :src name :dst name :prov {...}} maps."
  [{:keys [entities tokens]}]
  (let [entity-pairs (partition 2 1 entities)
        base (or (first entity-pairs)
                  (when (seq entities) [(first entities)]))]
    (cond
      (>= (count entities) 2)
      (let [[src dst] (first entity-pairs)
            verb (some (fn [[idx token]]
                         (when (re-find #"(?i)^(met|lives|visited|lives|works|studies)$" token)
                           idx))
                       (map-indexed vector tokens))]
        [{:type :mentions
          :src (:name src)
          :dst (:name dst)
          :prov {:verb-index verb}}])

      (seq entities)
      [{:type :mentions
        :src (:name (first entities))
        :prov {}}]

      :else
      [])))
