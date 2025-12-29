(ns api.handlers.patterns
  (:require [api.util.http :as http]
            [app.patterns :as patterns]
            [app.store-manager :as store-manager]
            [clojure.string :as str]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn registry-handler [request]
  (try
    (let [profile (request-profile request)
          conn (store-manager/conn profile)
          env (store-manager/env profile)
          registry (patterns/registry conn)
          data-root (some-> (:data-dir env) str)
          xtdb (or (:xtdb env) {})
          xtdb-config (or (:config-path xtdb) (:resource xtdb))]
      (http/ok-json {:profile profile
                     :registry registry
                     :data-root data-root
                     :xtdb-enabled (get xtdb :enabled? true)
                     :xtdb-config xtdb-config}))
    (catch Exception ex
      (http/ok-json {:error "pattern-registry-failed"
                     :message (.getMessage ex)}
                    500))))
