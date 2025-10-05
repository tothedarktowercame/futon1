(defn load-gazetteer []
  {:person (set (edn/read-string (slurp "resources/gazetteer/people.edn")))
   :place  (set (edn/read-string (slurp "resources/gazetteer/places.edn")))})

(defn ner [{:keys [tokens pos]} gaz]
  (letfn [(is-proper? [[tok p]] (#{:NNP :NNPS} p))
          (merge-nnps [toks] ;; group contiguous proper nouns
            (->> toks
                 (partition-by (comp is-proper? vector))
                 (mapcat (fn [grp]
                           (if (every? (comp is-proper? vector) grp)
                             [[(str/join " " (map first grp)) :PROPN]]
                             grp)))))]
    (let [cands (merge-nnps (map vector tokens pos))]
      (->> cands
           (keep (fn [[s tag]]
                   (cond
                     (contains? (:person gaz) s) {:name s :type :person}
                     (contains? (:place  gaz) s) {:name s :type :place}
                     (and (= tag :PROPN) (re-matches #"[A-Z][a-z].*" s))
                     {:name s :type :unknown-proper})))
           vec))))
