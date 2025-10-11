(ns headless_api.handlers.me
  (:require [clojure.string :as str]
            [graph-memory.me-profile :as me-profile]
            [headless_api.store-manager :as store-manager]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn- safe-int [value]
  (when value
    (try
      (Integer/parseInt (str value))
      (catch Exception _ nil))))

(defn- truthy-param? [value]
  (contains? #{"1" "true" "yes" "on"}
             (some-> value str/lower-case)))

(defn- falsy-param? [value]
  (contains? #{"0" "false" "no" "off"}
             (some-> value str/lower-case)))

(defn- keyword-list [csv]
  (when (and csv (not (str/blank? csv)))
    (->> (str/split csv #",")
         (map #(str/trim %))
         (remove str/blank?)
         (map keyword)
         vec)))

(defn- manual-option-overrides [manual]
  (when (map? manual)
    (let [window (or (safe-int (:window-days manual))
                      (safe-int (:focus-window manual)))
          limit (or (safe-int (:neighbor-limit manual))
                    (safe-int (:hot-limit manual)))
          allow (get manual :allow-works?)
          allowed (:allowed-types manual)]
      (cond-> {}
        window (assoc :window-days window)
        limit (assoc :neighbor-limit limit)
        (contains? manual :allow-works?) (assoc :allow-works? (boolean allow))
        (seq allowed) (assoc :allowed-types (vec (map keyword allowed)))))))

(defn- request-options [request manual]
  (let [params (:query-params request)
        manual-overrides (manual-option-overrides manual)
        preferences (me-profile/preferences-from-manual manual)
        query-window (safe-int (get params "window_days"))
        query-limit (safe-int (get params "hot_limit"))
        allow-param (get params "allow_works")
        allowed-param (get params "allowed_types")
        now (System/currentTimeMillis)]
    (merge {:now now}
           manual-overrides
           preferences
           (cond-> {}
             query-window (assoc :window-days query-window)
             query-limit (assoc :neighbor-limit query-limit)
             (truthy-param? allow-param) (assoc :allow-works? true)
             (falsy-param? allow-param) (assoc :allow-works? false)
             allowed-param (assoc :allowed-types (keyword-list allowed-param))))))

(defn fetch! [request]
  (let [profile (request-profile request)
        _ (store-manager/conn profile)
        manual (store-manager/profile-doc profile)
        options (request-options request manual)
        graph (me-profile/profile options)
        data (cond-> graph
               (seq manual) (assoc :manual manual))]
    {:profile profile
     :data data}))

(defn upsert! [request patch]
  (when-not (map? patch)
    (throw (ex-info "Profile payload must be an object" {:status 400})))
  (let [profile (request-profile request)
        updated (store-manager/upsert-profile! profile patch)]
    {:profile profile
     :data updated}))

(defn summary [request limit]
  (let [profile (request-profile request)
        _ (store-manager/conn profile)
        manual (store-manager/profile-doc profile)
        options (request-options request manual)
        profile-map (me-profile/profile options)
        length (or limit 2000)
        text (me-profile/summary profile-map {:manual manual
                                              :limit length})]
    {:profile profile
     :text text
     :generated-at (:generated-at profile-map)}))
