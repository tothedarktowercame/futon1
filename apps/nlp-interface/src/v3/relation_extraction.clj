(defn relations [{:keys [tokens deps entities]}]
  ;; deps: list of {:rel :nsubj|:obj|:nmod:with, :head i, :dep j}
  ;; entities: vector of {:name :type :span [i j)}
  (let [ent-at (fn [i] (some #(when (<= (first (:span %)) i (dec (second (:span %)))) %) entities))]
    (->> deps
         (filter #(#{:nsubj :obj} (:rel %)))
         (group-by :head)
         (mapcat (fn [[verb idxs]]
                   (let [subj (some-> (some #(when (= (:rel %) :nsubj) %) idxs) :dep ent-at)
                         obj  (some-> (some #(when (= (:rel %) :obj)  %) idxs) :dep ent-at)]
                     (when (and subj obj)
                       [{:type :links-to :src (:id subj) :dst (:id obj)}]))))
         vec)))
