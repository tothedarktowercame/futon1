(ns api.handlers.model
  (:require [api.util.http :as http]
            [app.model :as model]
            [app.model-docbook :as model-docbook]
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

(defn describe-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        desc (or (model/describe conn)
                 (do
                   (ensure-patterns-descriptor! profile conn)
                   (model/describe conn)))]
    (if desc
      (http/ok-json (assoc desc :profile profile))
      (http/ok-json {:error "Model descriptor not found"
                     :profile profile}
                    404))))

(defn verify-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
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
        open-world (or (model-open-world/describe conn)
                       (do
                         (ensure-open-world-descriptor! profile conn)
                         (model-open-world/describe conn)))]
    (http/ok-json {:profile profile
                   :descriptors
                   {:patterns patterns
                    :docbook docbook
                    :open-world-ingest open-world}})))

(defn- inventory-type-counts []
  (->> (xt/q '{:find [?type (count ?e)]
               :where [[?e :entity/type ?type]]})
       (map (fn [[type count]] {:type type :count count}))
       (remove (fn [{:keys [type]}] (nil? type)))
       (sort-by (juxt (comp str :type) :count))))

(defn- open-world-types []
  (->> (xt/q '{:find [?id]
               :where [[?t :type/id ?id]]})
       (map first)
       (remove nil?)
       set))

(defn- covered-pattern-types [patterns]
  (if-let [entities (get-in patterns [:descriptor :entities])]
    (set (keys entities))
    #{}))

(defn queue-handler [request]
  (try
    (let [profile (request-profile request)
          conn (store-manager/conn profile)
          patterns (or (model/describe conn)
                       (do
                         (ensure-patterns-descriptor! profile conn)
                         (model/describe conn)))
          open-world (or (model-open-world/describe conn)
                         (do
                           (ensure-open-world-descriptor! profile conn)
                           (model-open-world/describe conn)))
          covered-patterns (covered-pattern-types patterns)
          covered-open-world (open-world-types)
          covered (into covered-patterns covered-open-world)
          inventory (inventory-type-counts)
          pending (->> inventory
                       (remove (fn [{:keys [type]}]
                                 (or (= type :model/descriptor)
                                     (contains? covered type))))
                       vec)]
      (http/ok-json {:profile profile
                     :generated-at (System/currentTimeMillis)
                     :covered {:pattern-types (sort-by str covered-patterns)
                               :open-world-types (sort-by str covered-open-world)}
                     :pending pending
                     :descriptors
                     {:patterns patterns
                      :open-world-ingest open-world}}))
    (catch Exception e
      (http/ok-json {:error "XTDB not available"
                     :message (.getMessage e)}
                    503))))
