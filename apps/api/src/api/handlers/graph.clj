(ns api.handlers.graph
  (:require
   [api.util.http :as http]
   [api.handlers.invariants :as invariants]
   [api.middleware.penholder :as penholder]
   [app.command-service :as commands]
   [app.slash.format :as fmt]
   [app.store-manager :as store-manager]
   [clojure.string :as str])
  (:import (java.util UUID)))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn- request-env [request profile]
  (let [merged (merge (store-manager/env profile)
                      (get-in request [:ctx :env]))
        penholder (some-> (:penholder merged) str/trim not-empty)
        derived (or penholder (penholder/penholder-from-request request))]
    (if (seq (or penholder derived))
      (cond-> merged
        derived (assoc :penholder derived))
      merged)))

(defn- normalize-relation-type [value]
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)
                          clean (if (str/starts-with? trimmed ":")
                                  (subs trimmed 1)
                                  trimmed)]
                      (when (seq clean) (keyword clean)))
    :else nil))

(defn- maybe-infer-relation-type [payload]
  (let [props (:props payload)
        raw (or (:relation/type props)
                (:link/type props)
                (:relation/type payload)
                (:pattern/type props)
                (:type payload)
                (:label props))]
    (if-let [inferred (normalize-relation-type raw)]
      {:type inferred}
      {})))

(defn ensure-entity!
  [request body]
  (let [profile (request-profile request)
        env (request-env request profile)
        ctx {:conn (store-manager/conn profile)
             :env env
             :record-anchors! (fn [anchors]
                                (store-manager/record-anchors! profile anchors))}
        {:keys [entity]} (commands/ensure-entity! ctx body)]
    {:profile profile
     :entity entity
     :lines (fmt/entity-lines entity)}))

(defn ensure-entity-handler [request]
  (let [body (:body request)
        debug? (some-> (get-in request [:query-params "debug"]) str (= "1"))
        result (try
                 {:ok? true
                  :payload (ensure-entity! request body)}
                 (catch clojure.lang.ExceptionInfo ex
                   (let [data (ex-data ex)
                         msg (.getMessage ex)]
                     (cond
                       ;; Store-level invariant failure - extract full details
                     (= msg "Model invariants failed")
                     {:ok? false
                      :invariant-failure? true
                      :error {:error msg
                              :invariants (:result data)
                              :event (:event data)}}

                      (= msg "XTDB durable proof failed")
                      {:ok? false
                       :error {:error msg
                               :status 503
                               :details (select-keys data [:missing-ids :event])}}

                       ;; Any explicit status error
                       (:status data)
                       {:ok? false
                        :error (merge {:error msg} data)}

                       :else
                       (throw ex)))))
        payload (:payload result)
        verification (when (:ok? result)
                       (let [env (request-env request (:profile payload))]
                         (invariants/maybe-verify-event
                          (:profile payload)
                          {:type :entity/upsert
                           :entity (:entity payload)}
                          (select-keys env [:penholder]))))]
    (cond
      ;; Store-level invariant failure - return 409 with full details
      (:invariant-failure? result)
      (http/ok-json (cond-> (:error result)
                      debug? (assoc :debug {:body body}))
                    409)

      ;; Other creation errors
      (not (:ok? result))
      (http/ok-json (cond-> (:error result)
                      debug? (assoc :debug {:body body}))
                    400)

      ;; Post-creation verification failure
      (and verification (not (:ok? verification)))
      (http/ok-json {:error "Model invariants failed"
                     :profile (:profile payload)
                     :invariants verification
                     :result payload}
                    409)

      :else
      (http/ok-json (cond-> (if debug?
                              (assoc payload :debug {:body body})
                              payload)
                      verification (assoc :invariants verification))))))

(defn- parse-long-param [value]
  (when (some? value)
    (try
      (Long/parseLong (str/trim (str value)))
      (catch Exception _ nil))))

(defn- parse-uuid-param [value]
  (when (and value (not (str/blank? value)))
    (try
      (UUID/fromString (str/trim value))
      (catch Exception _ nil))))

(defn fetch-entity!
  [request]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)}
        entity-id (some-> (get-in request [:path-params :id]) str/trim)
        query (:query-params request)
        version-id (some-> (get query "version") parse-uuid-param)
        as-of (or (parse-long-param (get query "as-of"))
                  (parse-long-param (get query "as_of")))
        entity (when entity-id
                 (commands/fetch-entity ctx {:id entity-id}
                                        (cond-> {}
                                          version-id (assoc :version-id version-id)
                                          as-of (assoc :as-of as-of))))]
    (if entity
      (http/ok-json {:profile profile
                     :entity entity})
      (http/ok-json {:error "Entity not found"
                     :entity-id entity-id}
                    404))))

(defn entity-history!
  [request]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)}
        entity-id (some-> (get-in request [:path-params :id]) str/trim)
        limit (some-> (get-in request [:query-params "limit"]) parse-long-param)
        history (when entity-id
                  (commands/entity-history ctx {:id entity-id}
                                           (when limit {:limit limit})))]
    (if history
      (http/ok-json {:profile profile
                     :entity (:entity history)
                     :versions (:versions history)})
      (http/ok-json {:error "Entity history not found"
                     :entity-id entity-id}
                    404))))

(defn- parse-entity-type [value]
  (when-let [raw (some-> value str/trim not-empty)]
    (let [clean (if (str/starts-with? raw ":") (subs raw 1) raw)]
      (keyword clean))))

(defn entity-latest!
  [request]
  (let [profile (request-profile request)
        ctx {:conn (store-manager/conn profile)}
        query (:query-params request)
        type-param (parse-entity-type (get query "type"))
        limit (or (some-> (get query "limit") parse-long-param) 1)]
    (if (nil? type-param)
      (http/ok-json {:error "type query parameter required"} 400)
      (let [entities (commands/latest-entities ctx {:type type-param :limit limit})]
        (http/ok-json {:profile profile
                       :type (if (keyword? type-param)
                               (subs (str type-param) 1)
                               (str type-param))
                       :entities (or entities [])})))))

(defn upsert-relation!
  [request body]
  (let [profile (request-profile request)
        env (request-env request profile)
        ctx {:conn (store-manager/conn profile)
             :env env
             :record-anchors! (fn [anchors]
                                (store-manager/record-anchors! profile anchors))}
        relation-spec (merge body (maybe-infer-relation-type body))
        {:keys [relation]} (commands/upsert-relation! ctx relation-spec)]
    {:profile profile
     :relation relation
     :lines (fmt/relation-lines relation)}))

(defn upsert-media-lyrics!
  [request body]
  (let [profile (request-profile request)
        env (request-env request profile)
        ctx {:conn (store-manager/conn profile)
             :env env
             :record-anchors! (fn [anchors]
                                (store-manager/record-anchors! profile anchors))}
        result (commands/upsert-media-lyrics! ctx body)]
    (assoc result :profile profile)))

(defn upsert-media-lyrics-handler [request]
  (let [body (:body request)
        debug? (some-> (get-in request [:query-params "debug"]) str (= "1"))
        result (try
                 {:ok? true
                  :payload (upsert-media-lyrics! request body)}
                 (catch clojure.lang.ExceptionInfo ex
                   (let [data (ex-data ex)
                         msg (.getMessage ex)]
                     (cond
                       (= msg "Model invariants failed")
                       {:ok? false
                        :invariant-failure? true
                        :error {:error msg
                                :invariants (:result data)
                                :event (:event data)}}

                       (= msg "XTDB durable proof failed")
                       {:ok? false
                        :error {:error msg
                                :status 503
                                :details (select-keys data [:missing-ids :event])}}

                       (:status data)
                       {:ok? false
                        :error (merge {:error msg} data)}

                       :else
                       (throw ex)))))
        payload (:payload result)
        verification (when (:ok? result)
                       (let [env (request-env request (:profile payload))]
                         (invariants/maybe-verify-event
                          (:profile payload)
                          {:type :media/lyrics-upsert
                           :track (:track payload)
                           :lyrics (:lyrics payload)
                           :relation (:relation payload)}
                          (select-keys env [:penholder]))))]
    (cond
      (:invariant-failure? result)
      (http/ok-json (cond-> (:error result)
                      debug? (assoc :debug {:body body}))
                    409)

      (not (:ok? result))
      (http/ok-json (cond-> (:error result)
                      debug? (assoc :debug {:body body}))
                    (or (:status (:error result)) 400))

      ;; Post-creation verification failure
      (and verification (not (:ok? verification)))
      (http/ok-json {:error "Model invariants failed"
                     :profile (:profile payload)
                     :invariants verification
                     :result payload}
                    409)

      :else
      (http/ok-json (cond-> (if debug?
                              (assoc payload :debug {:body body})
                              payload)
                      verification (assoc :invariants verification))))))

(defn upsert-relation-handler [request]
  (let [body (:body request)
        result (try
                 {:ok? true
                  :payload (upsert-relation! request body)}
                 (catch clojure.lang.ExceptionInfo ex
                   (let [data (ex-data ex)
                         msg (.getMessage ex)]
                     (cond
                       ;; Store-level invariant failure
                       (= msg "Model invariants failed")
                       {:ok? false
                        :invariant-failure? true
                        :error {:error msg
                                :invariants (:result data)
                                :event (:event data)}}
                       (= msg "XTDB durable proof failed")
                       {:ok? false
                        :error {:error msg
                                :status 503
                                :details (select-keys data [:missing-ids :event])}}
                       :else
                       (throw ex)))))
        payload (:payload result)
        verification (when (:ok? result)
                       (let [env (request-env request (:profile payload))]
                         (invariants/maybe-verify-event
                          (:profile payload)
                          {:type :relation/upsert
                           :relation (:relation payload)}
                          (select-keys env [:penholder]))))]
    (cond
      ;; Store-level invariant failure
      (:invariant-failure? result)
      (http/ok-json (:error result) 409)

      ;; Post-creation verification failure
      (and verification (not (:ok? verification)))
      (http/ok-json {:error "Model invariants failed"
                     :profile (:profile payload)
                     :invariants verification
                     :result payload}
                    409)

      :else
      (http/ok-json (cond-> payload
                      verification (assoc :invariants verification))))))

(defn- batch-relations [body]
  (cond
    (and (map? body) (sequential? (:relations body))) (:relations body)
    (sequential? body) body
    :else nil))

(defn- upsert-relation-safe! [ctx relation]
  (try
    {:ok? true
     :relation (:relation (commands/upsert-relation! ctx relation))}
    (catch clojure.lang.ExceptionInfo ex
      {:ok? false
       :error (.getMessage ex)
       :data (ex-data ex)
       :relation relation})
    (catch Throwable ex
      {:ok? false
       :error (.getMessage ex)
       :relation relation})))

(defn upsert-relations-batch-handler [request]
  (let [body (:body request)
        relations (batch-relations body)]
    (when-not (seq relations)
      (throw (ex-info "Missing relations array" {:status 400})))
    (let [profile (request-profile request)
          ctx {:conn (store-manager/conn profile)
               :env (request-env request profile)
               :record-anchors! (fn [anchors]
                                  (store-manager/record-anchors! profile anchors))}
          results (mapv (fn [rel]
                          (let [spec (merge rel (maybe-infer-relation-type rel))]
                            (upsert-relation-safe! ctx spec)))
                        relations)
          ok (filterv :ok? results)
          errors (filterv #(not (:ok? %)) results)
          payload {:profile profile
                   :relation-count (count ok)
                   :error-count (count errors)
                   :relations (mapv :relation ok)
                   :errors (mapv #(dissoc % :ok?) errors)}
          verification (when (and (seq ok) (invariants/verify-on-write?))
                         (let [checks (mapv (fn [rel]
                                              (invariants/verify-event profile
                                                                       {:type :relation/upsert
                                                                        :relation rel}))
                                            (map :relation ok))
                               failures (filterv #(not (:ok? %)) checks)]
                           {:ok? (empty? failures)
                            :checks checks
                            :failures failures
                            :profile profile}))]
      (if (and verification (not (:ok? verification)))
        (http/ok-json {:error "Model invariants failed"
                       :profile profile
                       :invariants verification
                       :result payload}
                      409)
        (http/ok-json (cond-> payload
                        verification (assoc :invariants verification)))))))
