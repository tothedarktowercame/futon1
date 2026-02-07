(ns api.handlers.diag
  (:require [api.util.http :as http]
            [app.store-manager :as store-manager]))

(defn ctx-snapshot [_request]
  (http/ok-json (store-manager/diag)))

(defn rehydrate! [_request]
  (let [ctx (store-manager/rehydrate!)]
    (http/ok-json {:ok true
                   :profile (:profile ctx)
                   :data-dir (some-> ctx :env :data-dir str)})))

(defn healthz [request]
  (let [caps (or (get-in request [:ctx :capabilities])
                 (store-manager/default-capabilities))
        watchdog (get (store-manager/current) :xtdb/watchdog)
        stalled? (boolean (:stalled? watchdog))
        status (if stalled? "degraded" "ok")]
    (http/ok-json {:status status
                   :xtdb/watchdog watchdog
                   :capabilities caps})))
