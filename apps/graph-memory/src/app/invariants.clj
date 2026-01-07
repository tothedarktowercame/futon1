;; apps/graph-memory/src/app/invariants.clj
(ns app.invariants
  "Shared model invariant helpers for enforcement and API reporting."
  (:require [app.config :as config]
            [app.model :as model]
            [app.model-docbook :as model-docbook]
            [app.model-media :as model-media]
            [app.model-meta :as model-meta]
            [app.model-open-world :as model-open-world]
            [app.model-penholder :as model-penholder]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private core-models
  [:patterns :media :meta-model :open-world-ingest :docbook :penholder])

(def ^:private model-registry
  {:patterns {:verify model/verify
              :ensure model/ensure-descriptor!}
   :media {:verify model-media/verify
           :ensure model-media/ensure-descriptor!}
   :meta-model {:verify model-meta/verify
                :ensure model-meta/ensure-descriptor!}
   :open-world-ingest {:verify model-open-world/verify
                       :ensure model-open-world/ensure-descriptor!}
   :docbook {:verify model-docbook/verify
             :ensure model-docbook/ensure-descriptor!}
   :penholder {:verify model-penholder/verify
               :ensure model-penholder/ensure-descriptor!}})

(def ^:private model-descriptors
  {:patterns "model/descriptor/patterns"
   :media "model/descriptor/media"
   :meta-model "model/descriptor/meta-model"
   :open-world-ingest "model/descriptor/open-world-ingest"
   :docbook "model/descriptor/docbook"
   :penholder "model/descriptor/penholder"})

(defn verify-on-write? []
  (true? (:model/verify-on-write? (config/config))))

(defn ensure-descriptors!
  ([conn env]
   (ensure-descriptors! conn env core-models))
  ([conn env models]
   (doseq [model (or (seq models) core-models)]
     (when-let [ensure-fn (get-in model-registry [model :ensure])]
       (ensure-fn conn env)))
   (vec (or (seq models) core-models))))

(defn verify-models
  ([conn]
   (verify-models conn core-models nil))
  ([conn models]
   (verify-models conn models nil))
  ([conn models opts]
   (let [models (or (seq models) core-models)
         results (into {}
                       (map (fn [model]
                              (if-let [verify-fn (get-in model-registry [model :verify])]
                                (let [model-opts (cond
                                                   (and (= model :meta-model)
                                                        (seq (:meta-only opts)))
                                                   {:only (:meta-only opts)}
                                                   :else nil)
                                      result (try
                                               (if model-opts
                                                 (verify-fn conn model-opts)
                                                 (verify-fn conn))
                                               (catch clojure.lang.ArityException _
                                                 (verify-fn conn)))]
                                  [model result])
                                [model {:ok? false :error :model/unknown}]))
                            models))
         ok? (every? (fn [[_ result]] (:ok? result)) results)]
     {:ok? ok?
      :models (vec models)
      :results results})))

(defn verify-core
  [conn]
  (verify-models conn core-models))

(def ^:private relation-type-models
  {:arxana/scholium :patterns
   :media/lyrics :media})

(def ^:private namespace-models
  {"pattern" :patterns
   "docbook" :docbook
   "open-world" :open-world-ingest
   "model" :meta-model
   "media" :media})

(defn- normalize-type [value]
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)
                          cleaned (if (str/starts-with? trimmed ":")
                                    (subs trimmed 1)
                                    trimmed)]
                      (when (seq cleaned)
                        (keyword cleaned)))
    :else nil))

(defn- parse-source [source]
  (cond
    (map? source) source
    (string? source) (try (edn/read-string source) (catch Exception _ nil))
    :else nil))

(defn- normalize-descriptor [value]
  (cond
    (keyword? value) (let [ns (namespace value)
                           nm (name value)]
                       (if ns
                         (str ns "/" nm)
                         nm))
    (string? value) (some-> value str/trim not-empty)
    :else nil))

(defn- normalize-penholder [value]
  (when-let [raw (cond
                   (keyword? value) (name value)
                   (string? value) value
                   :else nil)]
    (let [clean (str/trim raw)]
      (when (seq clean)
        (str/lower-case clean)))))

(defn- normalize-certificate [value]
  (cond
    (map? value) (when (seq value) value)
    (string? value) (let [clean (str/trim value)]
                      (when (seq clean) clean))
    :else nil))

(defn- parse-descriptor-source [value]
  (cond
    (map? value) value
    (string? value) (try (edn/read-string value) (catch Exception _ nil))
    :else nil))

(defn- descriptor-source-ok? [value]
  (let [parsed (parse-descriptor-source value)]
    (and (map? parsed)
         (contains? parsed :schema/version)
         (contains? parsed :model/scope)
         (contains? parsed :schema/certificate))))

(def ^:private pattern-language-keys
  #{:patterns/language-has-source
    :patterns/language-has-status
    :patterns/language-in-catalog
    :patterns/language-has-includes})

(def ^:private pattern-core-keys
  #{:patterns/pattern-core-components})

(defn- pattern-entity-type? [value]
  (when-let [kw (normalize-type value)]
    (let [ns (namespace kw)]
      (and ns (str/starts-with? ns "pattern")))))

(defn- pattern-allowed-keys [event]
  (case (:type event)
    :entity/upsert
    (let [etype (or (get-in event [:entity :type])
                    (get-in event [:entity :entity/type]))]
      (cond
        (= (normalize-type etype) :pattern/language)
        (into #{} (concat pattern-language-keys pattern-core-keys))
        (= (normalize-type etype) :pattern/library) pattern-core-keys
        (= (normalize-type etype) :pattern/component) pattern-core-keys
        (pattern-entity-type? etype) pattern-language-keys
        :else #{}))

    :relation/upsert
    (let [rel-type (or (get-in event [:relation :type])
                       (get-in event [:relation :relation/type]))]
      (if (= (normalize-type rel-type) :arxana/scholium)
        (into #{} (concat pattern-language-keys pattern-core-keys))
        #{}))

    #{}))

(defn- failure-counts-by-key [result model]
  (let [model-entry (get-in result [:results model])]
    (cond
      (and (map? model-entry) (contains? model-entry :results))
      (->> (:results model-entry)
           (map (fn [entry]
                  [(:key entry) (count (:failures entry))]))
           (into {}))

      (and (map? model-entry) (:error model-entry))
      {:model/error 1}

      :else {})))

(defn- non-worsening-by-key?
  [baseline preview allowed-keys model]
  (let [baseline-counts (failure-counts-by-key baseline model)
        preview-counts (failure-counts-by-key preview model)
        keys (into #{} (concat (keys baseline-counts) (keys preview-counts)))]
    (every? (fn [key]
              (let [baseline-count (get baseline-counts key 0)
                    preview-count (get preview-counts key 0)]
                (if (> preview-count baseline-count)
                  (contains? allowed-keys key)
                  true)))
            keys)))

(defn- bad-string? [value]
  (when (string? value)
    (let [trimmed (str/trim value)
          lowered (str/lower-case trimmed)]
      (or (str/blank? trimmed)
          (= lowered "external")))))

(defn- bad-strings
  "Collect any banned string values with their paths."
  ([value] (bad-strings [] value))
  ([path value]
   (cond
     (map? value)
     (mapcat (fn [[k v]]
               (bad-strings (conj path k) v))
             value)

     (sequential? value)
     (mapcat (fn [[idx v]]
               (bad-strings (conj path idx) v))
             (map-indexed vector value))

     (bad-string? value)
     [{:path path :value value}]

     :else [])))

(defn- penholder-entries [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :model/penholder]]
                      db)
                 (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/source] %) ids)))

(defn- penholder-registry [conn]
  (let [db @conn
        entries (penholder-entries db)]
    (reduce
     (fn [{:keys [registry errors]} entry]
       (let [source (parse-source (:entity/source entry))
             descriptor (normalize-descriptor (:descriptor source))
             penholders (->> (:penholders source)
                             (map normalize-penholder)
                             (remove nil?)
                             vec)
             certificate (normalize-certificate (:certificate source))
             strict? (if (contains? source :strict?)
                       (boolean (:strict? source))
                       true)
             valid? (and descriptor (seq penholders) certificate)]
         (if valid?
           {:registry (assoc registry descriptor {:penholders (set penholders)
                                                  :strict? strict?
                                                  :name (:entity/name entry)
                                                  :certificate certificate})
            :errors errors}
           {:registry registry
            :errors (conj errors
                          {:id (:entity/id entry)
                           :name (:entity/name entry)
                           :issue :invalid-penholder-entry
                           :descriptor descriptor
                           :penholders penholders
                           :certificate (boolean certificate)})})))
     {:registry {}
      :errors []}
     entries)))

(defn- model-from-entity-type [raw-type]
  (when-let [kw (normalize-type raw-type)]
    (let [ns (namespace kw)
          nm (name kw)]
      (cond
        (= kw :model/penholder) :penholder
        (= kw :model/descriptor) :meta-model
        (get namespace-models ns) (get namespace-models ns)
        (and (= "arxana" ns)
             (str/starts-with? nm "media-")) :media
        :else nil))))

(defn- model-from-relation-type [raw-type]
  (when-let [kw (normalize-type raw-type)]
    (or (get relation-type-models kw)
        (get namespace-models (namespace kw)))))

(defn- models-for-event [event]
  (let [event-type (:type event)]
    (case event-type
      :entity/upsert
      (let [entity (:entity event)
            model (model-from-entity-type (or (:type entity)
                                              (:entity/type entity)))]
        (if model [model] core-models))

      :relation/upsert
      (let [rel (:relation event)
            models (->> [(model-from-relation-type (or (:type rel)
                                                       (:relation/type rel)))
                         (model-from-entity-type (get-in rel [:src :type]))
                         (model-from-entity-type (get-in rel [:dst :type]))]
                        (remove nil?)
                        distinct
                        vec)]
        (if (seq models) models core-models))

      (:entity/retract :relation/retract :entity/expire)
      core-models

      core-models)))

(defn- verify-penholder
  [conn event opts models]
  (let [{:keys [registry errors]} (penholder-registry conn)
        penholder (or (:penholder opts)
                      (:model/penholder opts))
        holder (normalize-penholder penholder)
        bad-values (bad-strings event)
        models (or models [])
        failures (vec
                  (concat
                   (when (seq bad-values)
                     (map (fn [bad]
                            (assoc bad :issue :penholder/bad-string))
                          bad-values))
                   (for [err errors]
                     (assoc err :issue :invalid-penholder-entry))
                   (keep (fn [model]
                           (when-let [descriptor (get model-descriptors model)]
                             (when-let [{:keys [penholders strict? name certificate]}
                                        (get registry descriptor)]
                               (when strict?
                                 (cond
                                   (nil? holder)
                                   {:model model
                                    :descriptor descriptor
                                    :entry name
                                    :issue :penholder/missing}

                                   (not (contains? penholders holder))
                                   {:model model
                                    :descriptor descriptor
                                    :entry name
                                    :penholder holder
                                    :issue :penholder/unauthorized}

                                   (nil? certificate)
                                   {:model model
                                    :descriptor descriptor
                                    :entry name
                                    :issue :penholder/missing-certificate}

                                   :else nil)))))
                         models)))]
    {:ok? (empty? failures)
     :event-type (:type event)
     :failures failures}))

(defn verify-event
  "Verify model invariants for the event. Returns {:ok? ... :results ...}."
  ([conn event]
   (verify-event conn event {}))
  ([conn event opts]
   (let [models (models-for-event event)
         entity (:entity event)
         entity-type (model-from-entity-type (or (:type entity)
                                                 (:entity/type entity)))
         meta-only (when (= entity-type :meta-model)
                     (->> [(:id entity) (:name entity)]
                          (remove nil?)
                          set))
         model-result (if (= entity-type :meta-model)
                        (let [ok? (descriptor-source-ok? (:source entity))]
                          {:ok? ok?
                           :models [:meta-model]
                           :results {:meta-model {:ok? ok?
                                                  :results (if ok?
                                                             []
                                                             [{:key :meta-model/descriptor-source
                                                               :ok? false
                                                               :failures [[:descriptor-source-invalid]]}])}}})
                        (verify-models conn models {:meta-only meta-only}))
         penholder-result (verify-penholder conn event opts models)
         baseline-conn (:baseline-conn opts)
         baseline-result (when (and baseline-conn (not (:ok? model-result)))
                           (verify-models baseline-conn models {:meta-only meta-only}))
         non-worsening? (when (and baseline-result (not (:ok? model-result)))
                          (let [allowed-pattern (pattern-allowed-keys event)]
                            (every? (fn [model]
                                      (if (get-in model-result [:results model :ok?])
                                        true
                                        (non-worsening-by-key?
                                         baseline-result
                                         model-result
                                         (if (= model :patterns) allowed-pattern #{})
                                         model)))
                                    models)))
         ok? (and (or (:ok? model-result) non-worsening?)
                  (:ok? penholder-result))]
     (-> model-result
         (assoc :ok? ok?
                :event-type (:type event)
                :penholder penholder-result
                :non-worsening? (boolean non-worsening?))))))
