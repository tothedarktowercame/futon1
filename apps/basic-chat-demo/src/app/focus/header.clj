(ns app.focus.header
  "Human-readable focus header construction."
  (:require [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private default-max-length 2000)
(def ^:private default-recent-limit 5)

(defn- conn->db [conn]
  (cond
    (nil? conn) nil
    (d/conn? conn) (d/db conn)
    :else conn))

(defn- format-arrow [direction]
  (if (= direction :in)
    ["<-" "<-"]
    ["-" "->"]))

(defn- render-edge [{:keys [src dst type direction]}]
  (let [[left right] (format-arrow direction)
        type-label (some-> type name)]
    (when (and src dst type-label)
      (format "%s %s[%s]%s %s" src left type-label right dst))))

(defn- dedupe-by [f coll]
  (loop [items coll
         seen #{}
         acc []]
    (if-let [item (first items)]
      (let [k (f item)]
        (if (contains? seen k)
          (recur (rest items) seen acc)
          (recur (rest items) (conj seen k) (conj acc item))))
      acc)))

(defn- take-unique-recent-eids [db limit]
  (when db
    (loop [datoms (sort-by :tx > (seq (d/datoms db :avet :relation/type)))
           seen #{}
           acc []]
      (if (or (empty? datoms) (>= (count acc) limit))
        acc
        (let [datom (first datoms)
              eid (:e datom)]
          (if (contains? seen eid)
            (recur (rest datoms) seen acc)
            (recur (rest datoms) (conj seen eid) (conj acc eid))))))))

(defn recent-relations
  "Return the most recent relation edges recorded in Datascript."
  ([conn]
   (recent-relations conn 5))
  ([conn limit]
   (let [db    @conn
         limit (max 1 (long (or limit 5)))
         eids (take-unique-recent-eids db (or limit default-recent-limit))]
     (->> eids
          (keep (fn [eid]
                  (when-let [rel (d/pull db
                                         '[:relation/id :relation/type :relation/last-seen :relation/confidence
                                           {:relation/src [:entity/id :entity/name :entity/type]}
                                           {:relation/dst [:entity/id :entity/name :entity/type]}]
                                         eid)]
                    {:id         (:relation/id rel)
                     :type       (:relation/type rel)
                     :last-seen  (:relation/last-seen rel)
                     :confidence (:relation/confidence rel)
                     :src {:id   (get-in rel [:relation/src :entity/id])
                           :name (get-in rel [:relation/src :entity/name])
                           :type (get-in rel [:relation/src :entity/type])}
                     :dst {:id   (get-in rel [:relation/dst :entity/id])
                           :name (get-in rel [:relation/dst :entity/name])
                           :type (get-in rel [:relation/dst :entity/type])}})))
          (into []))))) ; materialise a vector only if you really need one

(defn- entities-from-relations [rels]
  (->> rels
       (mapcat (fn [{:keys [src dst]}]
                 [(select-keys src [:name :type])
                  (select-keys dst [:name :type])]))
       (remove #(str/blank? (:name %)))
       (dedupe-by :name)))

(defn- current-section [{:keys [entities relations]}]
  (let [entity-data (->> entities
                         (keep (fn [{:keys [name type]}]
                                 (let [trimmed (some-> name str/trim)]
                                   (when (seq trimmed)
                                     {:name trimmed
                                      :type type}))))
                         (dedupe-by :name))
        relation-data (->> relations
                           (keep (fn [{:keys [src dst type]}]
                                   (when (and src dst type)
                                     {:type type
                                      :src {:name src}
                                      :dst {:name dst}}))))]
    (when (or (seq entity-data) (seq relation-data))
      {:entities entity-data
       :relations relation-data})))

(defn- enriched-section [context-lines]
  (let [relation-data (->> context-lines
                           (keep (fn [{:keys [entity neighbor relation direction]}]
                                   (when (and entity neighbor relation)
                                     {:type relation
                                      :src {:name entity}
                                      :dst {:name neighbor}
                                      :direction direction}))))
        entity-data (->> context-lines
                         (mapcat (fn [{:keys [entity neighbor]}]
                                   [{:name entity}
                                    {:name neighbor}]))
                         (remove #(str/blank? (:name %)))
                         (dedupe-by :name))]
    (when (or (seq entity-data) (seq relation-data))
      {:entities entity-data
       :relations relation-data})))

(defn- recent-section [conn current-relations {:keys [limit]}]
  (let [exclude (set (map (fn [{:keys [type src dst]}]
                            [type (get-in src [:name]) (get-in dst [:name])])
                          current-relations))
        rels (recent-relations conn {:limit limit :exclude exclude})]
    (when (seq rels)
      {:entities (entities-from-relations rels)
       :relations rels})))

(defn- render-section [title relations]
  (when (seq relations)
    (str title "\n"
         (str/join "\n"
                   (keep render-edge relations)))))

(defn- truncate [s max-length]
  (when s
    (if (<= (count s) max-length)
      s
      (str (subs s 0 (max 0 (- max-length 1))) "…"))))

(defn- presence [section]
  (when (and section
             (or (seq (:entities section))
                 (seq (:relations section))))
    section))

(defn build
  "Construct focus header from recent graph state, current turn, and enrichment."
  ([conn result context]
   (build conn result context {}))
  ([conn {:keys [entities relations]} context {:keys [max-length recent-limit]
                                               :or {max-length default-max-length
                                                    recent-limit default-recent-limit}}]
   (let [current (current-section {:entities entities
                                   :relations relations})
         recent (recent-section conn (:relations current) {:limit recent-limit})
         enriched (enriched-section context)
         sections (->> [(when recent (render-section "Recent:" (map (fn [{:keys [src dst type]}]
                                                                      {:src (:name src)
                                                                       :dst (:name dst)
                                                                       :type type})
                                                                    (:relations recent))))
                        (when current (render-section "Current:" (map (fn [{:keys [src dst type]}]
                                                                        {:src (get-in src [:name])
                                                                         :dst (get-in dst [:name])
                                                                         :type type})
                                                                      (:relations current))))
                        (when enriched (render-section "Enriched:" (map (fn [{:keys [src dst type direction]}]
                                                                          {:src (get-in src [:name])
                                                                           :dst (get-in dst [:name])
                                                                           :type type
                                                                           :direction direction})
                                                                        (:relations enriched))))]
                       (remove nil?))
         text (when (seq sections)
                (truncate (str "context:\n" (str/join "\n" sections)) max-length))
         json (not-empty
               (into {}
                     (keep (fn [[k v]]
                             (when-let [section (presence v)]
                               [k section])))
                     [[:recent recent]
                      [:current current]
                      [:enriched enriched]]))]
     {:text text
      :json json})))
