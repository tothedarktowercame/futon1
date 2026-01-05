(ns api.handlers.lab
  (:require [api.util.http :as http]
            [app.xt :as xt]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- normalize-session-id [payload]
  (or (:lab/session-id payload)
      (get payload "lab/session-id")
      (:session-id payload)
      (get payload "session-id")))

(defn- ensure-xtdb! []
  (when-not (xt/started?)
    (throw (ex-info "XTDB not started" {:status 503}))))

(defn- parse-long [value]
  (when (some? value)
    (try
      (Long/parseLong (str/trim (str value)))
      (catch Exception _ nil))))

(defn ingest-handler [request]
  (try
    (ensure-xtdb!)
    (let [payload (:body request)
          session-id (some-> (normalize-session-id payload) str/trim)]
      (if (str/blank? session-id)
        (http/ok-json {:error "lab/session-id required"} 400)
        (let [doc (-> payload
                      (assoc :xt/id session-id)
                      (assoc :entity/type :lab/session)
                      (assoc :lab/session-id session-id))]
          (xt/put-entity! doc)
          (http/ok-json {:ok true
                         :session-id session-id}))))
    (catch clojure.lang.ExceptionInfo ex
      (http/ok-json {:error (ex-message ex)} (or (:status (ex-data ex)) 500)))
    (catch Exception ex
      (http/ok-json {:error "lab-ingest-failed"
                     :message (.getMessage ex)}
                    500))))

(defn fetch-handler [request]
  (try
    (ensure-xtdb!)
    (let [session-id (some-> (get-in request [:path-params :id]) str/trim)]
      (if (str/blank? session-id)
        (http/ok-json {:error "lab/session-id required"} 400)
        (let [doc (xtdb/entity (xtdb/db (xt/node)) session-id)]
          (if doc
            (http/ok-json {:ok true
                           :session-id session-id
                           :doc doc})
            (http/ok-json {:error "lab session not found"
                           :session-id session-id}
                          404)))))
    (catch clojure.lang.ExceptionInfo ex
      (http/ok-json {:error (ex-message ex)} (or (:status (ex-data ex)) 500)))
    (catch Exception ex
      (http/ok-json {:error "lab-fetch-failed"
                     :message (.getMessage ex)}
                    500))))

(defn list-handler [request]
  (try
    (ensure-xtdb!)
    (let [limit (or (parse-long (get-in request [:query-params "limit"])) 20)
          db (xtdb/db (xt/node))
          rows (xtdb/q db '{:find [?e ?end ?start]
                            :where [[?e :lab/session-id ?sid]
                                    [(get-else ?e :lab/timestamp-end nil) ?end]
                                    [(get-else ?e :lab/timestamp-start nil) ?start]]})
          entries (->> rows
                       (map (fn [[eid end start]]
                              {:id eid
                               :timestamp-end end
                               :timestamp-start start}))
                       (sort-by (fn [{:keys [timestamp-end timestamp-start]}]
                                  (or timestamp-end timestamp-start 0))
                                >)
                       (take limit)
                       vec)]
      (http/ok-json {:ok true
                     :count (count entries)
                     :entries entries}))
    (catch clojure.lang.ExceptionInfo ex
      (http/ok-json {:error (ex-message ex)} (or (:status (ex-data ex)) 500)))
    (catch Exception ex
      (http/ok-json {:error "lab-list-failed"
                     :message (.getMessage ex)}
                    500))))
