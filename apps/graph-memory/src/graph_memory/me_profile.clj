(ns graph-memory.me-profile
  "Derive a salience-aware profile for the :me entity based on XTDB relations."
  (:require [app.focus :as focus]
            [app.xt :as xt]
            [clojure.math :as math]
            [clojure.string :as str])
  (:import (java.time Instant ZoneId)
           (java.time.format DateTimeFormatter)
           (java.util UUID)))

(def ^:private entity-pull
  '(pull ?e [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen :entity/pinned?]))

(def ^:private base-entity-query
  {:find [entity-pull]
   :where '[[?e :entity/id _]]})

(def ^:private exact-name-query
  {:find [entity-pull]
   :in ['?name]
   :where '[[?e :entity/name ?name]]})

(def ^:private ci-name-query
  {:find [entity-pull]
   :in ['?target]
   :where '[[?e :entity/name ?name]
            [(clojure.string/lower-case ?name) ?ln]
            [(clojure.string/lower-case ?target) ?tn]
            [(= ?ln ?tn)]]})

(def ^:private default-window-days 45)
(def ^:private default-neighbor-limit 8)
(def ^:private default-topic-limit 5)

(def ^:private iso-formatter
  (.withZone DateTimeFormatter/ISO_OFFSET_DATE_TIME (ZoneId/of "UTC")))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- coerce-long [v]
  (cond
    (nil? v) nil
    (instance? Number v) (long v)
    :else (try
            (Long/parseLong (str v))
            (catch Exception _ nil))))

(defn- normalize-entity [doc]
  (when-let [id (:entity/id doc)]
    (let [name (:entity/name doc)
          type (:entity/type doc)
          seen (coerce-long (:entity/seen-count doc))
          last (coerce-long (:entity/last-seen doc))
          pinned? (when (contains? doc :entity/pinned?)
                    (boolean (:entity/pinned? doc)))]
      (cond-> {:id id}
        name (assoc :name name)
        type (assoc :type (if (keyword? type) type (keyword (str type))))
        seen (assoc :seen-count seen)
        last (assoc :last-seen last)
        (some? pinned?) (assoc :pinned? pinned?)))))

(defn- run-entity-query
  ([] (run-entity-query []))
  ([clauses]
   (let [query (update base-entity-query :where into (or clauses []))]
     (->> (xt/q query)
          (map first)
          (map normalize-entity)
          (remove nil?)
          vec))))

(defn- normalize-id [v]
  (cond
    (nil? v) nil
    (instance? UUID v) v
    (uuid? v) v
    (keyword? v) v
    (string? v) (let [trimmed (str/trim v)]
                  (when (seq trimmed)
                    (try
                      (UUID/fromString trimmed)
                      (catch Exception _
                        (keyword trimmed)))))
    :else nil))

(defn- collect-ids [v]
  (cond
    (nil? v) []
    (sequential? v) (mapcat collect-ids v)
    (map? v) (concat (collect-ids (:id v))
                     (collect-ids (:entity-id v))
                     (collect-ids (:entity v)))
    :else (let [id (normalize-id v)]
            (if id [id] []))))

(defn- clean-name [v]
  (let [trimmed (some-> v str str/trim)]
    (when (seq trimmed) trimmed)))

(defn- collect-names [v]
  (cond
    (nil? v) []
    (sequential? v) (mapcat collect-names v)
    (map? v) (concat (collect-names (:name v))
                     (collect-names (:display-name v))
                     (collect-names (:label v))
                     (collect-names (:alias v)))
    (keyword? v) [(name v)]
    :else (let [name (clean-name v)]
            (if name [name] []))))

(defn- ordered-distinct [coll]
  (loop [seen #{}
         acc []
         xs coll]
    (if (empty? xs)
      acc
      (let [v (first xs)]
        (if (or (nil? v) (seen v))
          (recur seen acc (rest xs))
          (recur (conj seen v) (conj acc v) (rest xs)))))))

(defn preferences-from-manual
  "Extract preferred identifiers and names from a manual profile document."
  [doc]
  (when (map? doc)
    (let [name-keys [:name :preferred-name :display-name :handle :nickname :aka :aliases :handles :primary-entity :entity :preferred-entity]
          id-keys [:entity-id :entity/id :primary-entity :entity :preferred-entity :entity-ids :entities :ids]
          names (->> name-keys
                     (mapcat #(collect-names (get doc %)))
                     ordered-distinct)
          ids (->> id-keys
                   (mapcat #(collect-ids (get doc %)))
                   ordered-distinct)]
      (cond-> {}
        (seq ids) (assoc :preferred-ids ids)
        (seq names) (assoc :preferred-names names)))))

(defn- resolve-by-ids [ids]
  (->> ids
       (map (fn [id]
              (some-> (normalize-id id) xt/entity normalize-entity)))
       (remove nil?)
       vec))

(defn- resolve-by-name [name]
  (let [trimmed (clean-name name)]
    (when (seq trimmed)
      (or (some-> (xt/q exact-name-query trimmed) ffirst normalize-entity)
          (some-> (xt/q ci-name-query trimmed) ffirst normalize-entity)))))

(defn- resolve-by-names [names]
  (->> names
       (map resolve-by-name)
       (remove nil?)
       vec))

(defn- salience-key [{:keys [pinned? seen-count last-seen name]}]
  [(if pinned? 0 1)
   (- (or seen-count 0))
   (- (or last-seen 0))
   (or name "")])

(defn- pick-best [entities]
  (when (seq entities)
    (first (sort-by salience-key entities))))

(defn select-me [{:keys [preferred-ids preferred-names]}]
  (let [candidate-fns [#(resolve-by-ids preferred-ids)
                       #(resolve-by-names preferred-names)
                       #(run-entity-query '[[?e :entity/type :person]
                                            [?e :entity/pinned? true]])
                       #(run-entity-query '[[?e :entity/pinned? true]])
                       #(run-entity-query '[[?e :entity/type :person]])
                       #(run-entity-query [])]]
    (some (fn [f]
            (when-let [candidate (pick-best (f))]
              candidate))
          candidate-fns)))

(defn- recency-score [last-seen now cutoff]
  (if (and last-seen (>= last-seen cutoff))
    (let [age (- now last-seen)
          window (max 1 (- now cutoff))]
      (- 1.0 (/ age (double window))))
    0.0))

(defn- entity-salience [entity now cutoff]
  (let [seen (double (or (:seen-count entity) 0))
        recency (recency-score (:last-seen entity) now cutoff)
        pinned (boolean (:pinned? entity))]
    (+ (* 0.8 (math/log1p seen))
       (* 1.5 recency)
       (if pinned 1.0 0.0))))

(defn- humanize-keyword [k]
  (when k
    (-> k name (str/replace #"[_-]" " "))))

(defn- format-ts [ms]
  (when ms
    (.format iso-formatter (Instant/ofEpochMilli (long ms)))))

(defn- to-double [v]
  (when (some? v)
    (double v)))

(defn- convert-relation [{relation-id :relation/id
                          relation-type :relation/type
                          :keys [direction neighbor score confidence last-seen provenance subject object]}]
  (let [neighbor' (normalize-entity neighbor)]
    (cond-> {:id relation-id
             :type relation-type
             :direction direction
             :score (to-double score)
             :subject subject
             :object object}
      neighbor' (assoc :neighbor neighbor')
      confidence (assoc :confidence (to-double confidence))
      last-seen (assoc :last-seen (coerce-long last-seen))
      (seq provenance) (assoc :provenance provenance))))

;; before:
;; (defn- hot-relations [db entity {:keys [...] }]

(defn- hot-relations
  [ds-conn-or-db xt-db entity
   {:keys [neighbor-limit per-type-caps window-days now allow-works? allowed-types]}]
  (let [now         (or now (now-ms))
        days        (long (or window-days default-window-days))
        day-ms      (* 24 60 60 1000)
        cutoff      (- now (* days day-ms))
        limit       (max 1 (or neighbor-limit default-neighbor-limit))
        neighbors   (or (focus/top-neighbors
                         ds-conn-or-db xt-db (:id entity)
                         {:k-per-anchor  limit
                          :per-edge-caps per-type-caps
                          :allowed-types allowed-types
                          :allow-works?  allow-works?
                          :time-hint     cutoff})
                        [])]
    (->> neighbors
         (keep convert-relation)                    ; drop nil rows outright
         (map #(update % :score (fnil double 0.0))) ; nil score -> 0.0
         (sort-by :score >)                         ; descending by score
         (take limit)
         vec)))

(defn- build-topics [relations topic-limit]
  (->> relations
       (group-by :type)
       (map (fn [[type rels]]
              (let [score (reduce + 0.0 (map #(or (:score %) 0.0) rels))
                    neighbor-names (->> rels
                                        (map #(get-in % [:neighbor :name]))
                                        (remove str/blank?)
                                        ordered-distinct
                                        (take 4)
                                        vec)]
                {:type type
                 :score (double score)
                 :neighbors neighbor-names})))
       (sort-by (comp - :score))
       (take (or topic-limit default-topic-limit))
       vec))

(defn profile
  "Build a structured profile for the :me entity using salience-weighted relations."
  ([]
   (profile {}))
  ([{:keys [db preferred-ids preferred-names neighbor-limit per-type-caps window-days allow-works? allowed-types now entity] :as opts}]
   (let [now    (or now (now-ms))
         days   (long (or window-days default-window-days))
         day-ms (* 24 60 60 1000)
         cutoff (- now (* days day-ms))
         me     (or entity
                    (select-me {:preferred-ids preferred-ids
                                :preferred-names preferred-names})
                    (throw (ex-info "No candidate entity found for :me" {:status 404})))
         hot-opts {:neighbor-limit neighbor-limit
                   :per-type-caps  per-type-caps
                   :window-days    days
                   :now            now
                   :allow-works?   allow-works?
                   :allowed-types  allowed-types}
         ;; 1) Never let relations be nil
         ;; ds-conn-or-db might be provided in opts as :db; if it's a conn, @-it here or in top-neighbors
         ds-conn-or-db db
         ;; xt-db must come from your node/fixture (thread it into opts the same way as :db)
         xt-db         (:xt-db opts)
         relations (vec (or (hot-relations ds-conn-or-db xt-db me hot-opts) []))
         salience-score (entity-salience me now cutoff)]
     (let [me-relations  (when (not= :me (:id me))
                           (vec (or (hot-relations ds-conn-or-db xt-db {:id :me} hot-opts) [])))
           all-relations (->> (concat relations me-relations)
                              (sort-by :score >)
                              (take (or neighbor-limit 8))
                              vec)
           ;; 2) Only build topics if there’s something to build from
           topics        (if (seq all-relations)
                           (vec (or (build-topics all-relations default-topic-limit) []))
                           [])]
       {:entity    me
        :salience  {:score      salience-score
                    :seen-count (:seen-count me)
                    :last-seen  (:last-seen me)
                    :pinned?    (:pinned? me)
                    :window     {:days days
                                 :cutoff cutoff
                                 :now now}}
        :relations all-relations
        :topics    topics
        :generated-at now}))))

(defn- relation-line
  ([entity-name rel]
   (relation-line entity-name rel 0 true))
  ([entity-name {:keys [type direction score confidence last-seen provenance subject object] :as rel} indent display-subject?]
   (let [neighbor-name (get-in rel [:neighbor :name])
         focus (or entity-name "?")
         subject-text (or subject (if (= direction :in)
                                    (or neighbor-name "?")
                                    focus))
         object-text (or object (if (= direction :in)
                                  focus
                                  (or neighbor-name "?")))
         relation-label (or (humanize-keyword type) "relation")
         arrow (if (= direction :in) "←" "→")
         indent-prefix (if (zero? indent)
                         "- "
                         (apply str (repeat indent "    ")))
         base (if display-subject?
                (str indent-prefix subject-text " —" relation-label " " arrow " " object-text)
                (str indent-prefix "—" relation-label " " arrow " " object-text))
         extras (->> [(when (number? score) (format "score %.2f" (double score)))
                      (when (number? confidence) (format "conf %.2f" (double confidence)))
                      (when last-seen (str "last " (format-ts last-seen)))
                      (when (map? provenance)
                        (let [pairs (->> provenance
                                         (take 4)
                                         (map (fn [[k v]]
                                                (str (name k) "=" (if (string? v)
                                                                    v
                                                                    (pr-str v)))))
                                         (str/join ", "))]
                          (when (seq pairs)
                            (str "prov " pairs))))]
                     (remove str/blank?)
                     (str/join ", "))]
     (if (seq extras)
       (str base " (" extras ")")
       base))))

(defn- chain-key
  [text]
  (when-let [clean (some-> text
                           str
                           (str/replace #"[’]" "'")
                           (str/replace #"\s+-\s+" "-")
                           (str/replace #"\s+" " ")
                           str/trim)]
    (when (seq clean)
      (str/lower-case clean))))

(defn- prepare-relations-for-chain
  [entity-name relations]
  (let [focus (or entity-name "?")]
    (map (fn [rel]
           (let [subject (or (:subject rel) (if (= :in (:direction rel))
                                              (get-in rel [:neighbor :name])
                                              focus))
                 object (or (:object rel) (if (= :in (:direction rel))
                                            focus
                                            (get-in rel [:neighbor :name])))
                 rel-id (or (:id rel) (hash rel))]
             (assoc rel
                    :subject subject
                    :object object
                    ::rel-id rel-id
                    ::subject-key (chain-key subject)
                    ::object-key (chain-key object))))
         relations)))

(defn- emit-chain-lines
  [entity-name rel indent parent-key visited by-subject]
  (let [rel-id (::rel-id rel)]
    (if (visited rel-id)
      [visited []]
      (let [visited' (conj visited rel-id)
            subject-key (::subject-key rel)
            display-subject? (or (zero? indent)
                                 (nil? parent-key)
                                 (not= parent-key subject-key)
                                 (str/blank? (:subject rel)))
            line (relation-line entity-name rel indent display-subject?)
            object-key (::object-key rel)
            children (->> (get by-subject object-key)
                          (remove #(contains? visited' (::rel-id %))))
            [visited'' child-lines]
            (reduce (fn [[v acc] child]
                      (let [[v' lines'] (emit-chain-lines entity-name child (inc indent) object-key v by-subject)]
                        [v' (into acc lines')]))
                    [visited' []]
                    children)]
        [visited'' (cons line child-lines)]))))

(defn- relation-lines-with-chains
  [entity-name relations]
  (let [prepared (vec (prepare-relations-for-chain entity-name relations))
        by-subject (group-by ::subject-key prepared)]
    (loop [remaining prepared
           visited #{}
           acc []]
      (if (empty? remaining)
        acc
        (let [rel (first remaining)
              rel-id (::rel-id rel)]
          (if (visited rel-id)
            (recur (rest remaining) visited acc)
            (let [[visited' lines] (emit-chain-lines entity-name rel 0 nil visited by-subject)]
              (recur (rest remaining) visited' (into acc lines)))))))))

(defn- topic-line [{:keys [type neighbors score]}]
  (let [label (or (humanize-keyword type) "focus")
        joined (if (seq neighbors)
                 (str/join ", " neighbors)
                 "(none)")]
    (format "- %s: %s (score %.2f)" label joined (double (or score 0.0)))))

(defn- render-manual-value [v]
  (cond
    (nil? v) nil
    (string? v) (let [trimmed (str/trim v)] (when (seq trimmed) trimmed))
    (keyword? v) (name v)
    (boolean? v) (str v)
    (number? v) (str v)
    (sequential? v) (->> v (map render-manual-value) (remove str/blank?) (str/join ", "))
    (map? v) (or (render-manual-value (:summary v))
                 (render-manual-value (:description v)))
    :else (str v)))

(defn- manual-lines [manual]
  (when (and (map? manual) (seq manual))
    (let [entries (->> manual
                       (map (fn [[k v]]
                              (let [val (render-manual-value v)]
                                (when (seq val)
                                  [k val]))))
                       (remove nil?)
                       (take 6))]
      (when (seq entries)
        (into ["Manual notes:"]
              (map (fn [[k v]]
                     (str "- " (name k) ": " v))
                   entries))))))

(defn summary
  "Render a text summary for the provided or inferred :me profile."
  ([opts]
   (let [profile-data (profile opts)]
     (summary profile-data opts)))
  ([profile-data {:keys [limit manual] :or {limit 2000}}]
   (let [{:keys [entity salience relations topics generated-at]} profile-data
         entity-name (:name entity)
         type        (:type entity)
         header      (cond-> (or entity-name "Unknown entity")
                       type (str " (" (humanize-keyword type) ")"))
         {:keys [score seen-count last-seen pinned? window]} salience
         status-components (->> [(when (number? score) (format "score %.2f" (double score)))
                                 (when seen-count (str "seen " seen-count))
                                 (when last-seen (str "last seen " (format-ts last-seen)))
                                 (when pinned? "pinned")]
                                (remove nil?)
                                (str/join " · "))
         status-line (when (seq status-components)
                       (str "Status: " status-components
                            (when-let [days (get window :days)]
                              (str " (window " days "d)"))))
         topic-block (if (seq topics)
                       (into ["Focus priorities:"] (map topic-line topics))
                       ["Focus priorities:" "- (no dominant themes in window)"])
         ;; 3) Only compute relation lines when we have relations
         relation-lines (if (seq relations)
                          (vec (or (relation-lines-with-chains entity-name relations) []))
                          [])
         relation-block (if (seq relation-lines)
                          (into ["Hot relations:"] relation-lines)
                          ["Hot relations:" "- (none in window)"])
         manual-block   (manual-lines manual)
         generated-line (when generated-at
                          (str "Generated: " (format-ts generated-at)))
         lines  (->> (concat [header]
                             (when status-line [status-line])
                             topic-block
                             relation-block
                             (or manual-block [])
                             (when generated-line [generated-line]))
                     (remove str/blank?))
         text   (str/join "\n" lines)
         trimmed (if (> (count text) limit)
                   (-> text (subs 0 limit) str/trim)
                   text)
         final  (str/trim trimmed)]
     (if (str/blank? final)
       "No salient profile available."
       final))))
