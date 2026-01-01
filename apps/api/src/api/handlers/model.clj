(ns api.handlers.model
  (:require [api.util.http :as http]
            [app.model :as model]
            [app.model-docbook :as model-docbook]
            [app.model-media :as model-media]
            [app.model-meta :as model-meta]
            [app.model-open-world :as model-open-world]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn- ensure-patterns-descriptor! [profile conn]
  (let [env (store-manager/env profile)]
    (model/ensure-descriptor! conn env)))

(defn- ensure-docbook-descriptor! [profile conn]
  (let [env (store-manager/env profile)]
    (model-docbook/ensure-descriptor! conn env)))

(defn- ensure-open-world-descriptor! [profile conn]
  (let [env (store-manager/env profile)]
    (model-open-world/ensure-descriptor! conn env)))

(defn- ensure-media-descriptor! [profile conn]
  (let [env (store-manager/env profile)]
    (model-media/ensure-descriptor! conn env)))

(defn- ensure-meta-model-descriptor! [profile conn]
  (let [env (store-manager/env profile)]
    (model-meta/ensure-descriptor! conn env)))

(declare registry-handler)

(defn describe-handler [request]
  (registry-handler request))

(defn verify-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        _ (ensure-patterns-descriptor! profile conn)
        result (model/verify conn)]
    (http/ok-json (assoc result :profile profile))))

(defn describe-docbook-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        desc (or (model-docbook/describe conn)
                 (do
                   (ensure-docbook-descriptor! profile conn)
                   (model-docbook/describe conn)))]
    (if desc
      (http/ok-json (assoc desc :profile profile))
      (http/ok-json {:error "Docbook model descriptor not found"
                     :profile profile}
                    404))))

(defn verify-docbook-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        result (model-docbook/verify conn)]
    (http/ok-json (assoc result :profile profile))))

(defn describe-open-world-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        desc (or (model-open-world/describe conn)
                 (do
                   (ensure-open-world-descriptor! profile conn)
                   (model-open-world/describe conn)))]
    (if desc
      (http/ok-json (assoc desc :profile profile))
      (http/ok-json {:error "Open-world ingest model descriptor not found"
                     :profile profile}
                    404))))

(defn verify-open-world-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        result (model-open-world/verify conn)]
    (http/ok-json (assoc result :profile profile))))

(defn describe-media-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        desc (or (model-media/describe conn)
                 (do
                   (ensure-media-descriptor! profile conn)
                   (model-media/describe conn)))]
    (if desc
      (http/ok-json (assoc desc :profile profile))
      (http/ok-json {:error "Media model descriptor not found"
                     :profile profile}
                    404))))

(defn verify-media-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        result (model-media/verify conn)]
    (http/ok-json (assoc result :profile profile))))

(defn describe-meta-model-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        desc (or (model-meta/describe conn)
                 (do
                   (ensure-meta-model-descriptor! profile conn)
                   (model-meta/describe conn)))]
    (if desc
      (http/ok-json (assoc desc :profile profile))
      (http/ok-json {:error "Meta-model descriptor not found"
                     :profile profile}
                    404))))

(defn verify-meta-model-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        result (model-meta/verify conn)]
    (http/ok-json (assoc result :profile profile))))

(defn registry-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        patterns (or (model/describe conn)
                     (do
                       (ensure-patterns-descriptor! profile conn)
                       (model/describe conn)))
        docbook (or (model-docbook/describe conn)
                    (do
                      (ensure-docbook-descriptor! profile conn)
                      (model-docbook/describe conn)))
        meta-model (or (model-meta/describe conn)
                       (do
                         (ensure-meta-model-descriptor! profile conn)
                         (model-meta/describe conn)))
        open-world (or (model-open-world/describe conn)
                       (do
                         (ensure-open-world-descriptor! profile conn)
                         (model-open-world/describe conn)))
        media (or (model-media/describe conn)
                  (do
                    (ensure-media-descriptor! profile conn)
                    (model-media/describe conn)))]
    (http/ok-json {:profile profile
                   :descriptors
                   {:patterns patterns
                    :docbook docbook
                    :meta-model meta-model
                    :open-world-ingest open-world
                    :media media}})))

(defn- inventory-type-counts []
  (->> (xt/q '{:find [?type (count ?e)]
               :where [[?e :entity/type ?type]]})
       (map (fn [[type count]] {:type type :count count}))
       (remove (fn [{:keys [type]}] (nil? type)))
       (sort-by (juxt (comp str :type) :count))))

(def ^:private top-sizes [10 20 50 100])

(defn- type-name [t]
  (cond
    (keyword? t) (if-let [ns (namespace t)]
                   (str ns "/" (name t))
                   (name t))
    (string? t) t
    :else (str t)))

(defn- open-world-type? [t]
  (cond
    (keyword? t) (nil? (namespace t))
    (string? t) true
    :else false))

(defn- top-lists [counts]
  (let [sorted (->> counts
                    (sort-by (juxt (comp - :count)
                                   (comp type-name :type))))]
    {:total (count sorted)
     :top (into {}
                (map (fn [n] [(keyword (str "top-" n)) (vec (take n sorted))]))
                top-sizes)}))

(defn- entity-type-counts []
  (->> (xt/q '{:find [?type (count ?e)]
               :where [[?e :entity/type ?type]]})
       (map (fn [[type count]] {:type type :count count}))
       (remove (fn [{:keys [type]}] (nil? type)))))

(defn- relation-type-counts []
  (->> (xt/q '{:find [?type (count ?r)]
               :where [[?r :relation/type ?type]]})
       (map (fn [[type count]] {:type type :count count}))
       (remove (fn [{:keys [type]}] (nil? type)))))

(defn open-world-type-counts-handler [_request]
  (let [entity-counts (entity-type-counts)
        relation-counts (relation-type-counts)
        entity-open-world (filter (fn [{:keys [type]}]
                                    (and (open-world-type? type)
                                         (not= type :model/descriptor)))
                                  entity-counts)
        relation-open-world (filter (fn [{:keys [type]}]
                                      (open-world-type? type))
                                    relation-counts)]
    (http/ok-json {:generated-at (System/currentTimeMillis)
                   :entity-types {:all (top-lists entity-counts)
                                  :open-world (top-lists entity-open-world)}
                   :relation-types {:all (top-lists relation-counts)
                                    :open-world (top-lists relation-open-world)}})))

(defn- covered-pattern-types [patterns]
  (if-let [entities (get-in patterns [:descriptor :entities])]
    (set (keys entities))
    #{}))

(defn- covered-media-types [media]
  (if-let [entities (get-in media [:descriptor :entities])]
    (set (keys entities))
    #{}))

(defn- descriptor-entity-types [descriptor]
  (if-let [entities (get-in descriptor [:descriptor :entities])]
    (set (keys entities))
    #{}))

(defn- type-registry-summary []
  (let [counts (->> (xt/q '{:find [?kind (count ?t)]
                            :where [[?t :type/kind ?kind]]})
                    (map (fn [[kind count]] {:kind kind :count count}))
                    (sort-by (juxt :kind :count)))]
    {:counts counts}))

(defn queue-handler [request]
  (try
    (let [profile (request-profile request)
          conn (store-manager/conn profile)
          patterns (or (model/describe conn)
                       (do
                         (ensure-patterns-descriptor! profile conn)
                         (model/describe conn)))
          docbook (or (model-docbook/describe conn)
                      (do
                        (ensure-docbook-descriptor! profile conn)
                        (model-docbook/describe conn)))
          meta-model (or (model-meta/describe conn)
                         (do
                           (ensure-meta-model-descriptor! profile conn)
                           (model-meta/describe conn)))
          open-world (or (model-open-world/describe conn)
                         (do
                           (ensure-open-world-descriptor! profile conn)
                           (model-open-world/describe conn)))
          media (or (model-media/describe conn)
                    (do
                      (ensure-media-descriptor! profile conn)
                      (model-media/describe conn)))
          covered-patterns (covered-pattern-types patterns)
          covered-media (covered-media-types media)
          covered-docbook (descriptor-entity-types docbook)
          covered-open-world (descriptor-entity-types open-world)
          covered-meta-model (descriptor-entity-types meta-model)
          covered (into #{}
                        (concat covered-patterns
                                covered-media
                                covered-docbook
                                covered-open-world
                                covered-meta-model))
          inventory (inventory-type-counts)
          pending (->> inventory
                       (remove (fn [{:keys [type]}]
                                 (or (= type :model/descriptor)
                                      (contains? covered type))))
                       vec)
          verbose? (some-> (get-in request [:query-params "verbose"]) str (= "1"))
          pending-count (count pending)
          pending-sample (->> pending (take 20) vec)]
    (http/ok-json {:profile profile
                   :generated-at (System/currentTimeMillis)
                   :covered {:pattern-types (sort-by str covered-patterns)
                             :media-types (sort-by str covered-media)
                             :docbook-types (sort-by str covered-docbook)
                             :open-world-types (sort-by str covered-open-world)
                             :meta-model-types (sort-by str covered-meta-model)}
                   :type-registry (type-registry-summary)
                   :completed (->> {:patterns patterns
                                    :docbook docbook
                                    :media media
                                    :open-world-ingest open-world
                                    :meta-model meta-model}
                                   (filter (fn [[_ desc]] (some? desc)))
                                   (map first)
                                   vec)
                   :pending (if verbose?
                              pending
                              pending-sample)
                   :pending-count pending-count
                   :descriptors
                   {:patterns patterns
                     :docbook docbook
                     :meta-model meta-model
                     :open-world-ingest open-world
                     :media media}}))
    (catch Exception e
      (http/ok-json {:error "XTDB not available"
                     :message (.getMessage e)}
                    503))))
