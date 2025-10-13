(ns api.handlers.me
  (:require [app.command-service :as commands]
            [clojure.string :as str]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      nil))

(defn fetch!
  [request]
  (commands/fetch-profile {:profile (request-profile request)
                           :query-params (:query-params request)
                           :now (System/currentTimeMillis)}))

(defn upsert!
  [request patch]
  (commands/upsert-profile! {:profile (request-profile request)} patch))

(defn summary
  [request limit]
  (commands/profile-summary {:profile (request-profile request)
                             :query-params (:query-params request)
                             :now (System/currentTimeMillis)}
                            limit))
