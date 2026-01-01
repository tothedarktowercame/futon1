(ns api.util.http
  (:require [cheshire.core :as json]))

(defn- json-body [body]
  (if (string? body)
    body
    (json/generate-string body)))

(defn ok-json
  ([body]
   {:status 200
    :headers {"Content-Type" "application/json"}
    :body (json-body body)})
  ([body status]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (json-body body)}))

(defn ok-text
  ([text]
   (ok-text text 200 {}))
  ([text status headers]
   {:status status
    :headers (merge {"Content-Type" "text/plain; charset=utf-8"} headers)
    :body text}))

(defn ok-edn
  ([value]
   (ok-edn value 200 {}))
  ([value status headers]
   {:status status
    :headers (merge {"Content-Type" "application/edn; charset=utf-8"} headers)
    :body (pr-str value)}))
