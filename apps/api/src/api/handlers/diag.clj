(ns api.handlers.diag
  (:require [api.util.http :as http]
            [app.store-manager :as store-manager]))

(defn ctx-snapshot [_request]
  (http/ok-json (store-manager/diag)))

(defn healthz [_request]
  (http/ok-json {:status "ok"}))

