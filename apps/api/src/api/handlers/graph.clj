(ns api.handlers.graph
  (:require
   [api.util.http :as http]
   [app.command-service :as commands]
   [app.slash.format :as fmt]
   [app.store-manager :as store-manager]
   [clojure.string :as str])
  (:import (java.util UUID)))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn ensure-entity!
  [request body]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)
             :env (store-manager/env profile)
             :record-anchors! (fn [anchors]
                                (store-manager/record-anchors! profile anchors))}
        {:keys [entity]} (commands/ensure-entity! ctx body)]
    {:profile profile
     :entity entity
     :lines (fmt/entity-lines entity)}))

(defn ensure-entity-handler [request]
  (http/ok-json (ensure-entity! request (:body request))))

(defn- parse-long-param [value]
  (when (some? value)
    (try
      (Long/parseLong (str/trim (str value)))
      (catch Exception _ nil))))

(defn- parse-uuid-param [value]
  (when (and value (not (str/blank? value)))
    (try
      (UUID/fromString (str/trim value))
      (catch Exception _ nil))))

(defn fetch-entity!
  [request]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)}
        entity-id (some-> (get-in request [:path-params :id]) str/trim)
        query (:query-params request)
        version-id (some-> (get query "version") parse-uuid-param)
        as-of (or (parse-long-param (get query "as-of"))
                  (parse-long-param (get query "as_of")))
        entity (when entity-id
                 (commands/fetch-entity ctx {:id entity-id}
                                        (cond-> {}
                                          version-id (assoc :version-id version-id)
                                          as-of (assoc :as-of as-of))))]
    (if entity
      (http/ok-json {:profile profile
                     :entity entity})
      (http/ok-json {:error "Entity not found"
                     :entity-id entity-id}
                    404))))

(defn entity-history!
  [request]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)}
        entity-id (some-> (get-in request [:path-params :id]) str/trim)
        limit (some-> (get-in request [:query-params "limit"]) parse-long-param)
        history (when entity-id
                  (commands/entity-history ctx {:id entity-id}
                                           (when limit {:limit limit})))]
    (if history
      (http/ok-json {:profile profile
                     :entity (:entity history)
                     :versions (:versions history)})
      (http/ok-json {:error "Entity history not found"
                     :entity-id entity-id}
                    404))))

(defn upsert-relation!
  [request body]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)
             :env (store-manager/env profile)
             :record-anchors! (fn [anchors]
                                (store-manager/record-anchors! profile anchors))}
        {:keys [relation]} (commands/upsert-relation! ctx body)]
    {:profile profile
     :relation relation
     :lines (fmt/relation-lines relation)}))

(defn upsert-relation-handler [request]
  (http/ok-json (upsert-relation! request (:body request))))
