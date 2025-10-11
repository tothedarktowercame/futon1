(ns headless_api.server
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [headless_api.handlers.graph :as graph]
            [headless_api.handlers.ingest :as ingest]
            [headless_api.handlers.me :as me]
            [headless_api.handlers.turns :as turns]
            [headless_api.handlers.types :as types]
            [headless_api.store-manager :as store-manager])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent Executors)))

(def api-version "α")

(defonce ^:private !server (atom nil))

(defn- read-body [request]
  (if-let [body (:body request)]
    (slurp body)
    ""))

(defn- parse-json-body [request]
  (let [raw (read-body request)]
    (if (str/blank? raw)
      {}
      (try
        (json/parse-string raw true)
        (catch Exception _
          (throw (ex-info "Invalid JSON" {:status 400})))))))

(defn- json-response
  ([data]
   (json-response data 200))
  ([data status]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (json/generate-string data)}))

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
        (let [status (or (:status (ex-data e)) 400)]
          (add-api-version (json-response {:error (ex-message e)} status))))
      (catch Exception e
        (add-api-version (json-response {:error "internal error"} 500))))))

(defn- not-found [_]
  (-> (json-response {:error "not found"} 404)
      add-api-version))

(defn- parse-query-params [qs]
  (if (str/blank? qs)
    {}
    (into {}
          (for [part (str/split qs #"&") :when (seq part)]
            (let [[k v] (str/split part #"=" 2)]
              [(URLDecoder/decode k "UTF-8")
               (when v (URLDecoder/decode v "UTF-8"))])))))

(defn- exchange->request [^HttpExchange exchange]
  (let [uri (.getRequestURI exchange)
        headers (.getRequestHeaders exchange)]
    {:server-port (.getPort (.getLocalAddress exchange))
     :server-name (.getHostString (.getLocalAddress exchange))
     :remote-addr (.getHostString (.getRemoteAddress exchange))
     :uri (.getPath uri)
     :query-string (.getRawQuery uri)
     :query-params (parse-query-params (.getRawQuery uri))
     :scheme :http
     :request-method (keyword (str/lower-case (.getRequestMethod exchange)))
     :protocol (.getProtocol exchange)
     :headers (into {}
                    (for [entry (.entrySet headers)]
                      [(str/lower-case (str (key entry))) (first (val entry))]))
     :body (.getRequestBody exchange)}))

(defn- write-response! [^HttpExchange exchange {:keys [status headers body]}]
  (let [status-code (or status 200)
        response-headers (.getResponseHeaders exchange)
        body-bytes (cond
                     (nil? body) (.getBytes "" StandardCharsets/UTF_8)
                     (string? body) (.getBytes ^String body StandardCharsets/UTF_8)
                     (instance? (class (byte-array 0)) body) body
                     :else (.getBytes (str body) StandardCharsets/UTF_8))]
    (doseq [[k v] (or headers {})]
      (.set response-headers k v))
    (.sendResponseHeaders exchange status-code (count body-bytes))
    (with-open [out (.getResponseBody exchange)]
      (.write out body-bytes))))

(defn- start-http-server [handler port]
  (let [server (HttpServer/create (InetSocketAddress. port) 0)
        executor (Executors/newCachedThreadPool)]
    (.createContext server "/" (reify HttpHandler
                                  (handle [_ exchange]
                                    (let [request (exchange->request exchange)
                                          response (handler request)]
                                      (write-response! exchange response)))))
    (.setExecutor server executor)
    (.start server)
    {:server server
     :executor executor}))

(defn- turn-handler [request]
  (let [body (parse-json-body request)
        result (turns/process-turn! request body)]
    (json-response result)))

(defn- focus-header-handler [request]
  (json-response (turns/current-focus-header request)))

(defn- me-get-handler [request]
  (json-response (me/fetch! request)))

(defn- me-post-handler [request]
  (let [body (parse-json-body request)
        result (me/upsert! request body)]
    (json-response result)))

(defn- me-summary-handler [request]
  (let [limit (safe-int (get-in request [:query-params "limit_chars"]))
        {:keys [profile text]} (me/summary request (or limit 2000))]
    (text-response text 200 (cond-> {}
                              profile (assoc "X-Profile" profile)))))

(defn- ingest-handler [request]
  (let [raw (read-body request)
        result (ingest/ingest! request raw)]
    (json-response result)))

(defn- entity-handler [request]
  (let [body (parse-json-body request)
        result (graph/ensure-entity! request body)]
    (json-response result)))

(defn- relation-handler [request]
  (let [body (parse-json-body request)
        result (graph/upsert-relation! request body)]
    (json-response result)))

(defn- types-handler [request]
  (json-response (types/list-types request)))

(defn- canonical-path [path]
  (if (str/starts-with? path "/api/alpha")
    (str "/api/α" (subs path (count "/api/alpha")))
    path))

(defn- dispatch [request]
  (let [path (canonical-path (:uri request))
        method (:request-method request)]
    (case [method path]
      [:post "/api/α/turns"] (turn-handler request)
      [:get "/api/α/focus-header"] (focus-header-handler request)
      [:get "/api/α/me"] (me-get-handler request)
      [:post "/api/α/me"] (me-post-handler request)
      [:get "/api/α/me/summary"] (me-summary-handler request)
      [:post "/api/α/ingest"] (ingest-handler request)
      [:post "/api/α/entity"] (entity-handler request)
      [:post "/api/α/relation"] (relation-handler request)
      [:get "/api/α/types"] (types-handler request)
      (not-found request))))

(defn- handler []
  (-> dispatch
      wrap-exceptions
      wrap-api-version))

(defn start!
  ([] (start! {}))
  ([opts]
   (let [port (or (:port opts)
                  (some-> (System/getenv "ALPHA_PORT") safe-int)
                  8080)
         config (select-keys opts [:data-root :snapshot-every :xtdb :default-profile])]
     (if (seq config)
       (store-manager/configure! config)
       (store-manager/config))
     (let [{:keys [server executor]} (start-http-server (handler) port)
           actual-port (.getPort (.getAddress ^HttpServer server))]
       (reset! !server {:server server
                        :executor executor
                        :port actual-port})
       {:port actual-port
        :server server}))))

(defn stop! []
  (when-let [{:keys [server executor]} @!server]
    (.stop ^HttpServer server 0)
    (when executor
      (.shutdownNow ^java.util.concurrent.ExecutorService executor))
    (reset! !server nil))
  (store-manager/shutdown!)
  :stopped)

(defn -main [& _]
  (let [port-env (or (some-> (System/getenv "ALPHA_PORT") safe-int) 8080)
        profile (or (System/getenv "ALPHA_PROFILE") "default")]
    (store-manager/configure! {:default-profile profile})
    (let [{:keys [port server]} (start! {:port port-env})]
      (println (format "headless API listening on %d" port))
      @(promise))))
