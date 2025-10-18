
(ns api.handlers.types
  (:require [api.util.http :as http]
            [app.command-service :as commands]
            [app.slash.format :as fmt]))

(defn- ds-conn [request]
  (or (get-in request [:ctx :conn])
      (throw (ex-info "Datascript connection unavailable" {:status 500}))))

(defn list-types [request]
  (let [conn (ds-conn request)
        result (commands/list-types conn)
        all-types (->> (:types result)
                       vals
                       (apply concat)
                       (sort-by :id))]
    {:types (:types result)
     :lines (fmt/types-lines {:types all-types})}))

(defn list-types-handler [request]
  (http/ok-json (list-types request)))

(defn set-parent! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (let [doc (commands/set-type-parent! body)]
    (assoc doc :lines (fmt/types-lines {:types [doc]}))))

(defn set-parent-handler [request]
  (http/ok-json (set-parent! request (:body request))))

(defn merge-aliases! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (let [doc (commands/merge-aliases! body)]
    (assoc doc :lines (fmt/types-lines {:types [doc]}))))

(defn merge-aliases-handler [request]
  (http/ok-json (merge-aliases! request (:body request))))
