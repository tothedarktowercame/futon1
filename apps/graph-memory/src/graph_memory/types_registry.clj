(ns graph-memory.types-registry
  "XT-backed registry for entity and relation types with parent inference and alias support."
  (:require [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(def ^:private kind-set #{:entity :relation :intent})

(def ^:private !cache (atom nil))

(declare load-cache! merge! ensure!)

(def ^:private namespace-map
  (delay
    (if-let [res (io/resource "type_namespace_map.edn")]
      (-> res io/reader (java.io.PushbackReader.) edn/read)
      {})))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- ensure-cache! []
  (or @!cache (load-cache!)))

(defn- ->keyword [value]
  (cond
    (keyword? value) value
    (string? value)
    (let [trimmed (str/trim value)]
      (when (seq trimmed)
        (if (str/starts-with? trimmed ":")
          (when (> (count trimmed) 1)
            (keyword (subs trimmed 1)))
          (keyword trimmed))))
    :else nil))

(defn- normalize-kind [kind]
  (when kind
    (let [k (-> kind name str/lower-case keyword)]
      (when (kind-set k) k))))

(defn- wildcard? [kw]
  (= "*" (name kw)))

(defn- infer-parent [kind type]
  (let [t (->keyword type)]
    (cond
      (nil? t) nil
      (wildcard? t) nil
      (namespace t) (keyword (namespace t) "*")
      :else (get @namespace-map t (keyword (name t) "*")))))

(defn- kind-name [kind]
  (some-> kind name))

(defn- type-fragment [type]
  (when-let [kw (->keyword type)]
    (if-let [ns (namespace kw)]
      (str ns "/" (name kw))
      (name kw))))

(defn- doc-id [kind type]
  (when-let [fragment (type-fragment type)]
    (str "type|" (kind-name kind) "|" fragment)))

(defn- normalize-doc [doc]
  (when doc
    (let [id (:type/id doc)
          kind (:type/kind doc)
          parent (:type/parent doc)
          alias-of (:type/alias-of doc)
          aliases (set (map ->keyword (or (:type/aliases doc) [])))
          inferred? (boolean (:type/inferred-parent? doc))]
      {:xt/id (:xt/id doc)
       :id (->keyword id)
       :kind (normalize-kind kind)
       :parent (some-> parent ->keyword)
       :alias-of (some-> alias-of ->keyword)
       :aliases aliases
       :inferred? inferred?})))

(defn- sort-types [coll]
  (->> coll
       (map ->keyword)
       (remove nil?)
       (sort-by str)))

(defn- denormalize-doc [doc]
  (let [{:keys [id kind parent alias-of aliases inferred?]} doc
        xt-id (:xt/id doc)]
    (cond-> {:xt/id (or xt-id (doc-id kind id))
             :type/id id
             :type/kind kind}
      parent (assoc :type/parent parent)
      (boolean? inferred?) (assoc :type/inferred-parent? inferred?)
      alias-of (assoc :type/alias-of alias-of)
      true (assoc :type/aliases (vec (sort-types (or aliases #{})))))))

(defn- xt-available? []
  (xt/started?))

(defn- fetch-doc [kind type]
  (let [kind (normalize-kind kind)
        type (->keyword type)]
    (if (xt-available?)
      (when (and kind type)
        (normalize-doc (xt/entity (doc-id kind type))))
      (get-in (ensure-cache!) [:types [kind type]]))))

(defn- assoc-aliases [doc aliases]
  (update doc :aliases #(into (set (or % #{})) aliases)))

(defn- store-doc! [doc]
  (when (xt-available?)
    (xt/submit! [[::xtdb/put (denormalize-doc doc)]]))
  ;; optimistically update the cache for both test and prod environments
  (swap! !cache (fn [cache]
                  (let [docs (vec (distinct (conj (or (:docs cache) []) doc)))]
                    (assoc cache :docs docs))))
  (load-cache!)
  doc)

(defn- canonical-of [cache kind type]
  (loop [current type]
    (let [alias-target (get-in cache [:alias->canonical [kind current]])]
      (if alias-target
        (recur alias-target)
        current))))

(defn- alias-set [cache kind type]
  (get-in cache [:aliases [kind type]] #{}))

(defn load-cache!
  "Refresh and return the in-memory view of the type registry."
  []
  (let [docs (if (xt-available?)
               (->> (xt/q '{:find [(pull ?t [*])]
                            :where [[?t :type/id _]]})
                    (map (comp normalize-doc first))
                    (remove nil?)
                    (sort-by (juxt :kind :id)))
               (or (some-> @!cache :docs) []))
        types (into {} (map (fn [doc] [[(:kind doc) (:id doc)] doc]) docs))
        alias->canonical (into {}
                               (for [doc docs
                                     :let [alias-of (:alias-of doc)]
                                     :when alias-of]
                                 [[(:kind doc) (:id doc)] alias-of]))
        aliases (reduce (fn [acc doc]
                          (let [kind (:kind doc)
                                id (:id doc)
                                alias-of (:alias-of doc)
                                aliases (:aliases doc)]
                            (cond-> acc
                              (seq aliases) (update [kind id] (fnil into #{}) aliases)
                              alias-of (update [kind alias-of] (fnil conj #{}) id))))
                        {}
                        docs)
        children (reduce (fn [acc doc]
                           (let [kind (:kind doc)
                                 parent (:parent doc)
                                 alias-of (:alias-of doc)]
                             (if (and parent (nil? alias-of))
                               (update acc [kind parent] (fnil conj #{}) (:id doc))
                               acc)))
                         {}
                         docs)
        cache {:docs docs
               :types types
               :alias->canonical alias->canonical
               :aliases aliases
               :children children
               :updated-at (now-ms)}]
    (reset! !cache cache)
    cache))

(defn merge!
  "Mark aliases for the canonical type and refresh the cache.
   aliases may be a single type or a collection."
  [kind into aliases]
  (let [kind (normalize-kind kind)
        canonical (->keyword into)
        values (cond
                 (nil? aliases) []
                 (sequential? aliases) aliases
                 (set? aliases) aliases
                 :else [aliases])
        alias-set (->> values
                       (map ->keyword)
                       (remove nil?)
                       (remove #(= % canonical))
                       set)]
    (when (and kind canonical (seq alias-set))
      (ensure! kind canonical)
      (doseq [alias alias-set]
        (ensure! kind alias)
        (let [doc (fetch-doc kind alias)]
          (store-doc! (-> doc
                          (assoc :alias-of canonical)
                          (assoc :aliases #{})
                          (assoc :inferred? false)
                          (dissoc :parent)))))
      (let [doc (fetch-doc kind canonical)
            updated (assoc-aliases doc alias-set)]
        (store-doc! updated)))))

(defn ensure!
  "Ensure type docs exist for the given kind/type or sequence of types.
   Optional opts map may include :parent to override inferred parent."
  ([kind types]
   (ensure! kind types {}))
  ([kind types {:keys [parent] :as opts}]
   (let [kind (normalize-kind kind)]
     (cond
       (nil? kind) nil
       (sequential? types)
       (do (doseq [t types]
             (ensure! kind t opts))
           (ensure-cache!))
       :else
       (let [type (->keyword types)]
         (when type
           (let [parent (some-> parent ->keyword)
                 existing (fetch-doc kind type)]
             (if existing
               existing
               (do
                 (when (and (= :entity kind) (= :person type))
                   (merge! :entity :me ["i"]))
                 (let [parent (or parent (infer-parent kind type))
                       inferred? (boolean (and parent (not (contains? opts :parent))))
                       doc {:id type
                            :kind kind
                            :parent (when parent (do (ensure! kind parent {:parent nil}) parent))
                            :aliases #{}
                            :inferred? inferred?}]
                   (store-doc! doc)))))))))))

(defn set-parent!
  "Override the parent for the provided kind/type pair."
  [kind type parent]
  (let [kind (normalize-kind kind)
        type (->keyword type)
        parent (some-> parent ->keyword)]
    (when (and kind type)
      (when parent
        (ensure! kind parent))
      (let [existing (or (fetch-doc kind type)
                         (ensure! kind type {:parent parent}))
            updated (-> existing
                        (assoc :parent parent)
                        (assoc :inferred? false))]
        (store-doc! updated)))))

(defn descendants-of
  "Return the set of descendant types (including aliases) for the provided selector."
  ([kind selector]
   (descendants-of kind selector (ensure-cache!)))
  ([kind selector cache]
   (let [kind (normalize-kind kind)
         type (->keyword selector)]
     (if (and kind type)
       (let [canonical (canonical-of cache kind type)
             walk (fn walk [queue visited]
                    (if-let [t (first queue)]
                      (if (visited t)
                        (recur (rest queue) visited)
                        (let [children (get-in cache [:children [kind t]] #{})]
                          (recur (into (rest queue) children)
                                 (conj visited t))))
                      visited))
             canonicals (walk [canonical] #{})]
         (into canonicals
               (mapcat #(alias-set cache kind %) canonicals)))
       #{}))))

(defn effective-pred
  "Return a predicate that matches concrete types respecting descendants and aliases."
  [kind selectors]
  (let [kind (normalize-kind kind)
        cache (ensure-cache!)
        values (cond
                 (nil? selectors) nil
                 (and (seqable? selectors) (not (string? selectors))) selectors
                 :else [selectors])
        ks (->> values (map ->keyword) (remove nil?))]
    (if (empty? ks)
      (constantly true)
      (let [allowed (set (mapcat #(descendants-of kind % cache) ks))]
        (fn [type]
          (let [kw (->keyword type)]
            (if (and kw kind)
              (let [canon (canonical-of cache kind kw)]
                (boolean
                 (or (contains? allowed kw)
                     (contains? allowed canon)
                     (some #(contains? allowed %)
                           (alias-set cache kind canon)))))
              false)))))))

(defn docs
  "Return the cached registry documents."
  []
  (:docs (ensure-cache!)))
