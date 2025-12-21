(ns api.handlers.trails
  (:require [api.util.http :as http]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str])
  (:import (java.time Instant)
           (java.util UUID)))

(defn- normalize-text [value]
  (when-let [raw (cond
                   (string? value) value
                   (keyword? value) (name value)
                   :else (str value))]
    (let [trimmed (str/trim raw)]
      (when (seq trimmed)
        trimmed))))

(defn- parse-ts [value]
  (cond
    (nil? value) nil
    (instance? Number value) (long value)
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (or (try
                              (Long/parseLong trimmed)
                              (catch Exception _ nil))
                            (try
                              (.toEpochMilli (Instant/parse trimmed))
                              (catch Exception _ nil)))))
    :else nil))

(defn- sanitize-trail [profile body]
  (let [body (or body {})
        timestamp (or (parse-ts (:timestamp body))
                      (System/currentTimeMillis))
        intent (or (:intent body)
                   (get-in body [:tatami :intent]))
        payload (cond-> {:session-id (or (normalize-text (:session-id body))
                                         (normalize-text (get-in body [:tatami :session-id])))
                         :turn-id (or (normalize-text (:turn-id body))
                                      (str (UUID/randomUUID)))
                         :timestamp timestamp
                         :profile profile
                         :intent (normalize-text intent)
                         :patterns (or (:patterns body)
                                       (get-in body [:tatami :patterns]))
                         :events (or (:events body)
                                     (get-in body [:tatami :events]))
                         :fruits (or (:fruits body)
                                     (get-in body [:cue :fruits]))
                         :paramitas (or (:paramitas body)
                                        (get-in body [:cue :paramitas]))
                         :rule (or (:rule body)
                                   (get-in body [:cue :rule]))
                         :salience (:salience body)
                         :source (or (:source body) "futon3")}
                  (:matches (:cue body)) (assoc :matches (get-in body [:cue :matches])))]
    payload))

(defn record-trail-handler [request]
  (let [{:keys [ctx]} request
        body (:body request)
        profile (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
                    (:default-profile ctx) "default")
        conn (store-manager/conn profile)
        payload (sanitize-trail profile body)
        env (assoc (:env ctx) :now (:timestamp payload))
        stored (store/record-trail! conn env payload)
        public {:id (:trail/id stored)
                :session-id (:trail/session-id stored)
                :turn-id (:trail/turn-id stored)
                :timestamp (:trail/timestamp stored)}]
    (http/ok-json {:ok true
                   :trail public})))

(defn- parse-limit [request default-value]
  (let [raw (get-in request [:query-params "limit"])]
    (if raw
      (try
        (max 1 (Integer/parseInt raw))
        (catch Exception _ default-value))
      default-value)))

(defn recent-trails-handler [request]
  (let [{:keys [ctx]} request
        profile (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
                    (:default-profile ctx) "default")
        limit (parse-limit request 10)
        conn (store-manager/conn profile)
        trails (store/recent-trails conn limit)]
    (http/ok-json {:profile profile
                   :limit limit
                   :trails trails})))
