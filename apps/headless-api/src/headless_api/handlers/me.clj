(ns headless_api.handlers.me
  (:require [clojure.string :as str]
            [headless_api.store-manager :as store-manager]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn fetch! [request]
  (let [profile (request-profile request)
        doc (store-manager/profile-doc profile)]
    {:profile profile
     :data doc}))

(defn upsert! [request patch]
  (when-not (map? patch)
    (throw (ex-info "Profile payload must be an object" {:status 400})))
  (let [profile (request-profile request)
        updated (store-manager/upsert-profile! profile patch)]
    {:profile profile
     :data updated}))

(defn summary [request limit]
  (let [profile (request-profile request)
        length (or limit 2000)
        text (store-manager/profile-summary profile length)]
    {:profile profile
     :text text}))
