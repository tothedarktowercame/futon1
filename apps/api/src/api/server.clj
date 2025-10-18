(ns api.server
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.stacktrace :as st]
            [api.handlers.graph :as graph]
            [api.handlers.me :as me]
            [api.handlers.turns :as turns]
            [api.handlers.types :as types]
            [app.store-manager :as store-manager]
            [datascript.core :as d]
            [app.xt :as xt]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.params :refer [wrap-params]]
            [api.middleware.context :refer [wrap-context]]
            [api.middleware.qparams :refer [wrap-query-ctx]]
            [app.config :as cfg]
            ;[api.middleware.errors :refer [wrap-exceptions]]
            ;[api.middleware.logging :refer [logging-wrapper]]
            ;[api.middleware.version :refer [wrap-api-version]]
            [api.routes :refer [dispatch]]))

(def api-version "α")

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
    :body data}))

(defn- text-response
  ([text]
   (text-response text 200 {}))
  ([text status headers]
   {:status status
    :headers (merge {"Content-Type" "text/plain; charset=utf-8"}
                    headers)
    :body text}))

(defn- add-api-version [resp]
  (update resp :headers #(assoc (or % {}) "X-API-Version" api-version)))

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

(defn- not-found [_]
  (-> (json-response {:error "not found"} 404)
      add-api-version))

(defn- turn-handler [request]
  (let [body (:body request)
        result (turns/process-turn! request body)]
    (json-response result)))

(defn- focus-header-handler [request]
  (json-response (turns/current-focus-header request)))

(defn- me-get-handler [request]
  (json-response (me/fetch request)))

(defn- me-post-handler [request]
  (let [body (:body request)
        result (me/upsert! request body)]
    (json-response result)))

(defn- me-summary-handler [request]
  (let [{:keys [profile text]} (me/summary request)]
    (text-response text 200 (cond-> {}
                              profile (assoc "X-Profile" profile)))))

(defn- entity-handler [request]
  (let [body (:body request)
        result (graph/ensure-entity! request body)]
    (json-response result)))

(defn- relation-handler [request]
  (let [body (:body request)
        result (graph/upsert-relation! request body)]
    (json-response result)))

(defn- types-handler [request]
  (json-response (types/list-types request)))

(defn- types-parent-handler [request]
  (let [body (:body request)
        result (types/set-parent! request body)]
    (json-response result)))

(defn- types-merge-handler [request]
  (let [body (:body request)
        result (types/merge-aliases! request body)]
    (json-response result)))

(def ^:private help-doc
  {:commands ["/me         - Render the profile summary"
              "/me doc     - Show the raw profile document"
              "/help       - Show this help message"]})

(defn- help-handler [_]
  (json-response help-doc))

(defn- canonical-path [path]
  (if (str/starts-with? path "/api/alpha")
    (str "/api/α" (subs path (count "/api/alpha")))
    path))

(defn app [ctx]
  (-> #'dispatch
      wrap-api-version
      wrap-json-response
      (wrap-json-body {:keywords? true})
      wrap-params                 ;; <-- add this
      (wrap-context ctx)          ;; <-- runs after params so handlers see :query-params
      wrap-query-ctx
      wrap-exceptions))

(defn app-dev [ctx]
  (-> (app ctx)
      wrap-reload
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
     (init!)
     (reset! !server {:server server
                      :port actual-port})
     {:port actual-port
      :server server})))

(defn stop! []
  (when-let [{:keys [server]} @!server]
    (.stop server)
    (reset! !server nil))
  (store-manager/shutdown!)
  :stopped)

(defn -main [& _]
  (let [port-env (or (some-> (System/getenv "ALPHA_PORT") safe-int) 8080)
        profile (or (System/getenv "ALPHA_PROFILE") "default")]
    (let [{:keys [port server]} (start! {:port port-env :default-profile profile})]
      (println (format "headless API listening on %d" port))
      @(promise))))
