(ns api.handlers.diag
  (:require [api.util.http :as http]
            [app.store-manager :as store-manager]))

(defn ctx-snapshot [_request]
  (http/ok-json (store-manager/diag)))

;; {:ctx-keys        (-> ctx keys sort vec)
     ;;  :have-ds         (boolean (:ds/db ctx))
     ;;  :have-xt         (boolean (:xt/db ctx))

     ;;  :profile         (:profile ctx)
     ;;  :limit           (:limit ctx)
     ;;  :query-params    (:query-params req)
     ;;  :config (select-keys (cfg/config)
     ;;                       [:app/data-dir :app/server-port :xtdb/config-path])
     ;;  :data-dir (.getPath (cfg/data-dir))
     ;;  :now             (:now ctx)}
