(ns client.focus-header
  "Construct and render focus header debug output for the client app."
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

(defn- safe-datoms [db idx attr]
  (try
    (vec (d/datoms db idx attr))
    (catch Throwable _ [])))

(defn- recent-relation-eids [db limit]
  (when db
    (let [primary  (let [avet (safe-datoms db :avet :relation/last-seen)]
                     (if (seq avet)
                       avet
                       (safe-datoms db :aevt :relation/last-seen)))
          fallback (if (seq primary)
                     []
                     (let [avet (safe-datoms db :avet :relation/type)]
                       (if (seq avet)
                         avet
                         (safe-datoms db :aevt :relation/type))))
          source   (if (seq primary) primary fallback)]
      (loop [idx  (dec (count source))
             seen #{}
             acc  []]
        (if (or (< idx 0) (>= (count acc) limit))
          acc
          (let [datom (nth source idx)
                eid   (:e datom)]
            (if (contains? seen eid)
              (recur (dec idx) seen acc)
              (recur (dec idx) (conj seen eid) (conj acc eid)))))))))

(defn recent-relations
  "Return the most recent relation edges recorded in Datascript.

   Accepts an optional opts map with :limit (default 5) and :exclude, a collection of
   [type src-name dst-name] tuples that should be filtered from the result."
  ([conn]
   (recent-relations conn {}))
  ([conn opts]
   (let [{:keys [limit exclude]} (if (map? opts) opts {:limit opts})
         db           (conn->db conn)
         limit        (max 1 (long (or limit default-recent-limit)))
         exclude-set  (set exclude)
         fetch-limit  (+ limit (count exclude-set))
         exclude-key  (fn [rel]
                        [(:type rel)
                         (get-in rel [:src :name])
                         (get-in rel [:dst :name])])]
     (->> (recent-relation-eids db fetch-limit)
          (keep (fn [eid]
                  (when-let [rel (and db
                                      (d/pull db
                                              '[:relation/id :relation/type :relation/last-seen :relation/confidence
                                                {:relation/src [:entity/id :entity/name :entity/type]}
                                                {:relation/dst [:entity/id :entity/name :entity/type]}]
                                              eid))]
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
          (remove #(contains? exclude-set (exclude-key %)))
          (take limit)
          vec))))

(defn- entities-from-relations [rels]
  (->> rels
       (mapcat (fn [{:keys [src dst]}]
                 (let [src' {:name (or (:name src) (:entity/name src) src)
                             :type (or (:type src) (:entity/type src))}
                       dst' {:name (or (:name dst) (:entity/name dst) dst)
                             :type (or (:type dst) (:entity/type dst))}]
                   [src' dst'])))
       (remove #(str/blank? (some-> (:name %) str)))
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
                                   (let [src-name (or (get src :name) (get src :entity/name) src)
                                         dst-name (or (get dst :name) (get dst :entity/name) dst)
                                         src-type (or (get src :type) (get src :entity/type))
                                         dst-type (or (get dst :type) (get dst :entity/type))]
                                     (when (and src-name dst-name type)
                                       {:type type
                                        :src {:name src-name :type src-type}
                                        :dst {:name dst-name :type dst-type}})))))]
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
  (let [current-relations (or current-relations [])
        exclude (set (map (fn [{:keys [type src dst]}]
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

(defn- select-sections [pairs]
  (not-empty
   (reduce (fn [acc [k v]]
             (if-let [section (presence v)]
               (assoc acc k section)
               acc))
           {}
           pairs)))

(defn build
  "Construct a focus header summary of the Datascript graph state.

   conn    — Datascript conn or db snapshot (may be nil)
   result  — {:entities [...], :relations [...]}
   context — vector of enrichment rows from context/enrich-with-neighbors
   opts    — {:max-length N, :recent-limit M}

   Returns {:text string? :json map? :sections map?}."
  ([conn result context]
   (build conn result context {}))
  ([conn {:keys [entities relations]} context {:keys [max-length recent-limit]
                                               :or {max-length default-max-length
                                                    recent-limit default-recent-limit}}]
   (let [current   (current-section {:entities entities
                                     :relations relations})
         recent    (when conn
                     (recent-section conn (:relations current) {:limit recent-limit}))
         explicit  (enriched-section context)
         fallback  (when (and recent current)
                     (let [current-rels  (:relations current)
                           current-names (->> current-rels
                                              (mapcat (fn [{:keys [src dst]}]
                                                        [(some-> src :name)
                                                         (some-> dst :name)]))
                                              (map #(some-> % str/trim))
                                              (remove str/blank?)
                                              set)
                           related       (->> (:relations recent)
                                              (filter (fn [{:keys [src dst]}]
                                                        (let [src-name (some-> (:name src) str/trim)
                                                              dst-name (some-> (:name dst) str/trim)]
                                                          (or (contains? current-names src-name)
                                                              (contains? current-names dst-name)))))
                                              (map (fn [{:keys [src dst type]}]
                                                     {:type type
                                                      :src {:name (:name src)}
                                                      :dst {:name (:name dst)}}))
                                              (remove nil?)
                                              vec)]
                       (when (seq related)
                         {:entities (entities-from-relations related)
                          :relations related})))
         enriched  (or explicit fallback)
         sections  (->> [(when recent (render-section "Recent:" (map (fn [{:keys [src dst type]}]
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
         text      (when (seq sections)
                     (truncate (str "context:\n" (str/join "\n" sections)) max-length))
         section-map (select-sections [[:recent recent]
                                       [:current current]
                                       [:enriched enriched]])]
     (cond-> {:text text}
       section-map (assoc :json section-map :sections section-map)))))

(defn text-lines
  "Return a printable block of lines prefixed with \"Focus header\" from build output."
  [{:keys [text]}]
  (when-let [body (some-> text str/trim not-empty)]
    (let [lines (str/split-lines body)]
      (into ["Focus header"]
            (map #(str "  " %) lines)))))

(defn current-entities
  "Extract the current-section entities from a build result."
  [{:keys [json]}]
  (get-in json [:current :entities]))
