(ns api.handlers.diag
  (:require [api.util.http :as http]
            [app.store-manager :as store-manager]))

(defn ctx-snapshot [_request]
  (http/ok-json (store-manager/diag)))

(defn healthz [request]
  (let [caps (or (get-in request [:ctx :capabilities])
                 (store-manager/default-capabilities))]
    (http/ok-json {:status "ok"
                   :capabilities caps})))
