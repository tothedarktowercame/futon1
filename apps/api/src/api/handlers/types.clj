
(ns api.handlers.types
  (:require [api.handlers.invariants :as invariants]
            [api.util.http :as http]
            [app.command-service :as commands]
            [app.slash.format :as fmt]
            [app.store-manager :as store-manager]
            [clojure.string :as str]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

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
    {:type doc
     :lines (fmt/types-lines {:types [doc]})}))

(defn set-parent-handler [request]
  (let [payload (set-parent! request (:body request))
        profile (request-profile request)
        verification (invariants/maybe-verify-core profile)]
    (if (and verification (not (:ok? verification)))
      (http/ok-json {:error "Model invariants failed"
                     :profile profile
                     :invariants verification
                     :result payload}
                    409)
      (http/ok-json (cond-> payload
                      verification (assoc :invariants verification))))))

(defn merge-aliases! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (let [doc (commands/merge-aliases! body)]
    {:type doc
     :lines (fmt/types-lines {:types [doc]})}))

(defn merge-aliases-handler [request]
  (let [payload (merge-aliases! request (:body request))
        profile (request-profile request)
        verification (invariants/maybe-verify-core profile)]
    (if (and verification (not (:ok? verification)))
      (http/ok-json {:error "Model invariants failed"
                     :profile profile
                     :invariants verification
                     :result payload}
                    409)
      (http/ok-json (cond-> payload
                      verification (assoc :invariants verification))))))
