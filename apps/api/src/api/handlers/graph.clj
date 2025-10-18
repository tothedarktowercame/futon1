(ns api.handlers.graph
  (:require
   [api.util.http :as http]
   [app.command-service :as commands]
   [app.slash.format :as fmt]
   [app.store-manager :as store-manager]
   [clojure.string :as str]))

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
