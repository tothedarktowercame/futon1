(ns api.server
  (:require [api.handlers.turns :as turns]
            [api.middleware.context :refer [wrap-context]]
            [api.middleware.penholder :refer [wrap-penholder]]
            [api.middleware.qparams :refer [wrap-query-ctx]]
            [api.routes :refer [dispatch]]
            [app.config :as cfg]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [cheshire.core :as json]
            [clojure.stacktrace :as st]
            [phoebe.runtime :as phoebe]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]
            ;;[ring.middleware.reload :refer [wrap-reload]]
            ))

;; remove ring.middleware.reload from ns requires

(defn maybe-wrap-reload [handler]
  (if-let [wrap-reload (try
                         (requiring-resolve 'ring.middleware.reload/wrap-reload)
                         (catch Throwable _ nil))]
    (wrap-reload handler)
    handler))

(def api-version "α")
(def ^:private api-version-header-value "±")

(defonce ^:private !server (atom nil))

(defn- logging-wrapper [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable t
        (println "--- CATASTROPHIC ERROR ---")
        (.printStackTrace t)
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body "{\"error\": \"internal server error - see server logs\"}"}))))

(defn- json-response
  ([data]
   (json-response data 200))
  ([data status]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (json/generate-string data)}))

(defn- add-api-version [resp]
  (update resp :headers #(assoc (or % {}) "X-API-Version" api-version-header-value)))

(defn- safe-int [s]
  (when s
    (try
      (Integer/parseInt (str s))
      (catch Exception _ nil))))

(defn- wrap-api-version [handler]
  (fn [request]
    (some-> (handler request) add-api-version)))

(defn- wrap-exceptions [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        (println "--- ERROR ---")
        (println (ex-message e))
        (st/print-stack-trace e)
        (let [status (or (:status (ex-data e)) 400)]
          (add-api-version (json-response {:error (ex-message e)} status))))
      (catch Exception e
        (println "--- ERROR ---")
        (println (.getMessage e))
        (st/print-stack-trace e)
        (add-api-version (json-response {:error "internal error"} 500))))))

(defn- wrap-force-json [handler]
  (fn [request]
    (let [resp (handler request)]
      (if (and (map? resp)
               (contains? resp :body)
               (let [body (:body resp)]
                 (or (map? body)
                     (sequential? body))))
        (-> resp
            (update :headers #(assoc (or % {}) "Content-Type" "application/json"))
            (update :body json/generate-string))
        resp))))

(defn app [ctx]
  (-> #'dispatch
      wrap-json-response
      (wrap-json-body {:keywords? true})
      wrap-params                 ;; <-- add this
      (wrap-context ctx)          ;; <-- runs after params so handlers see :query-params
      wrap-penholder
      wrap-query-ctx
      wrap-exceptions
      wrap-force-json
      wrap-api-version))

(defn app-dev [ctx]
  (-> (app ctx)
      maybe-wrap-reload
      logging-wrapper))

(defn- build-context [opts]
  (let [sm-ctx (store-manager/start! opts)
        xtdb-node (xt/node)
        profile (:profile sm-ctx)]
    (turns/warm-profile! profile)
    (assoc sm-ctx :xtdb-node xtdb-node)))

(defn init! []
  (when (true? (:warmup/enable? (cfg/config)))
    (try
      (turns/warm-profile! {:k (:warmup/focus-k (cfg/config))})
      (catch Throwable t
        (println "[warmup] non-fatal:" (.getMessage t))))))

(defn start!
  ([opts]
   (let [port (or (:port opts)
                  (some-> (System/getenv "ALPHA_PORT") safe-int)
                  8080)
         ctx (build-context (select-keys opts [:data-root :snapshot-every :xtdb :default-profile]))
         server (jetty/run-jetty (app-dev ctx) {:port port :join? false})
         actual-port (-> server .getURI .getPort)]
     (phoebe/print-banner!
      {:app {:name "futon1-api"}
       :ports {:api actual-port}
       :config {:profile (:profile ctx)
                :data-dir (get-in ctx [:env :data-dir])
                :xtdb (get-in ctx [:env :xtdb])}})
     (init!)
     (reset! !server {:server server
                      :port actual-port})
     {:port actual-port
      :server server})))

(defn stop! []
  (when-let [{:keys [server]} @!server]
    (.stop ^org.eclipse.jetty.server.Server server)
    (reset! !server nil))
  (store-manager/shutdown!)
  :stopped)

(defn -main [& _]
  (println "[server] Starting...")
  (let [port-env (or (some-> (System/getenv "ALPHA_PORT") safe-int) 8080)
        profile (or (System/getenv "ALPHA_PROFILE") "default")
        {:keys [port]} (start! {:port port-env :default-profile profile})]
    (println (format "[server] Listening on port %d" port))
    @(promise)))
