(ns api.handlers.model
  (:require [api.util.http :as http]
            [app.model :as model]
            [app.model-docbook :as model-docbook]
            [app.model-open-world :as model-open-world]
            [app.store-manager :as store-manager]
            [clojure.string :as str]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn describe-handler [request]
  (let [profile (request-profile request)
        conn (store-manager/conn profile)
        desc (model/describe conn)]
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
        desc (model-docbook/describe conn)]
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
        desc (model-open-world/describe conn)]
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
        patterns (model/describe conn)
        docbook (model-docbook/describe conn)
        open-world (model-open-world/describe conn)]
    (http/ok-json {:profile profile
                   :descriptors
                   {:patterns patterns
                    :docbook docbook
                    :open-world-ingest open-world}})))
