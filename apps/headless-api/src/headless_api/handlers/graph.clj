(ns headless_api.handlers.graph
  (:require [app.store :as store]
            [clojure.string :as str]
            [headless_api.store-manager :as store-manager]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn- normalize-entity [value]
  (cond
    (map? value)
    (let [name (some-> (:name value) str/trim not-empty)
          type (:type value)
          id (:id value)]
      (when name
        (cond-> {:name name}
          type (assoc :type type)
          id (assoc :id id))))

    (string? value)
    (let [trimmed (str/trim value)]
      (when (seq trimmed)
        {:name trimmed}))

    :else
    nil))

(defn ensure-entity!
  [request body]
  (when-not (map? body)
    (throw (ex-info "Entity payload must be an object" {:status 400})))
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        env-now (assoc (store-manager/env profile) :now (System/currentTimeMillis))
        spec (normalize-entity body)]
    (when-not spec
      (throw (ex-info "Entity name required" {:status 400})))
    (let [stored (store/ensure-entity! conn env-now spec)]
      (store-manager/record-anchors! profile [stored])
      {:profile profile
       :entity (select-keys stored [:id :name :type :seen-count :last-seen :pinned?])})))

(defn upsert-relation!
  [request body]
  (when-not (map? body)
    (throw (ex-info "Relation payload must be an object" {:status 400})))
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        env (store-manager/env profile)
        now (System/currentTimeMillis)
        type (some-> (:type body) keyword)
        src-spec (normalize-entity (:src body))
        dst-spec (normalize-entity (:dst body))]
    (when-not type
      (throw (ex-info "Relation type required" {:status 400})))
    (when (or (nil? src-spec) (nil? dst-spec))
      (throw (ex-info "Relation requires src and dst" {:status 400})))
    (let [env-now (assoc env :now now)
          src-entity (store/ensure-entity! conn env-now src-spec)
          dst-entity (store/ensure-entity! conn env-now dst-spec)
          relation (store/upsert-relation! conn env-now
                                           {:type type
                                            :src (select-keys src-entity [:id :name :type])
                                            :dst (select-keys dst-entity [:id :name :type])})]
      (store-manager/record-anchors! profile [src-entity dst-entity])
      {:profile profile
       :relation (select-keys relation [:id :type :src :dst :confidence :last-seen])})))
