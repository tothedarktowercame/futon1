(ns api.handlers.graph
  (:require [app.command-service :as commands]
            [clojure.string :as str]
            [app.store-manager :as store-manager]))

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
     :entity entity}))

(defn upsert-relation!
  [request body]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)
             :env (store-manager/env profile)
             :record-anchors! (fn [anchors]
                                (store-manager/record-anchors! profile anchors))}
        {:keys [relation]} (commands/upsert-relation! ctx body)]
    {:profile profile
     :relation relation}))
