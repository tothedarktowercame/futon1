(ns api.handlers.docbook
  (:require [api.util.http :as http]
            [app.config :as config]
            [app.docbook :as docbook]
            [app.model-docbook :as model-docbook]
            [app.store-manager :as store-manager]
            [clojure.string :as str]))

(defn- request-profile [request]
  (or (some-> (get-in request [:headers "x-profile"]) str/trim not-empty)
      (store-manager/default-profile)))

(defn- verify-docbook? []
  (true? (:model/verify-on-write? (config/config))))

(defn- maybe-verify-docbook [request]
  (when (verify-docbook?)
    (let [profile (request-profile request)
          conn (store-manager/conn profile)
          result (model-docbook/verify conn)]
      (assoc result :profile profile))))

(defn- normalize-book [request]
  (or (some-> (get-in request [:path-params :book]) str/trim not-empty)
      "futon4"))

(defn- truthy-param? [value]
  (contains? #{"1" "true" "yes" "on"}
             (some-> value str/lower-case)))

(defn contents-handler [request]
  (let [book (normalize-book request)
        data (docbook/contents book)]
    (http/ok-json data)))

(defn toc-handler [request]
  (let [book (normalize-book request)
        data (docbook/toc book)]
    (http/ok-json data)))

(defn update-contents-order-handler [request]
  (let [book (normalize-book request)
        body (:body request)
        order (:order body)
        source (some-> (:source body) str/trim not-empty)
        timestamp (some-> (:timestamp body) str/trim not-empty)]
    (if-not (and (sequential? order) (not (string? order)))
      (http/ok-json {:error "order must be a list of doc-ids"
                     :book book}
                    400)
      (let [result (docbook/update-toc-order! book {:order order
                                                    :source source
                                                    :timestamp timestamp})
            verification (maybe-verify-docbook request)]
        (if (and verification (not (:ok? verification)))
          (http/ok-json {:error "Docbook invariants failed"
                         :book book
                         :invariants verification
                         :result result}
                        409)
          (http/ok-json (cond-> result
                          verification (assoc :invariants verification))))))))

(defn heading-handler [request]
  (let [book (normalize-book request)
        doc-id (some-> (get-in request [:path-params :doc-id]) str/trim not-empty)
        data (when doc-id (docbook/heading+entries book doc-id))]
    (if data
      (http/ok-json data)
      (http/ok-json {:error "Heading not found"
                     :book book
                     :doc-id doc-id}
                    404))))

(defn recent-handler [request]
  (let [book (normalize-book request)
        limit (some-> (get-in request [:query-params "limit"]) str/trim not-empty Long/parseLong)
        data (docbook/recent-entries book limit)]
    (http/ok-json data)))

(defn entry-handler [request]
  (let [book (normalize-book request)
        payload (:body request)]
    (if-not (map? payload)
      (http/ok-json {:error "Expected entry payload object"
                     :book book}
                    400)
      (let [result (docbook/upsert-entry! book payload)]
        (if (:ok? result)
          (let [verification (maybe-verify-docbook request)]
            (if (and verification (not (:ok? verification)))
              (http/ok-json {:error "Docbook invariants failed"
                             :book book
                             :invariants verification
                             :result (dissoc result :ok?)}
                            409)
              (http/ok-json (cond-> (dissoc result :ok?)
                              verification (assoc :invariants verification)))))
          (http/ok-json (dissoc result :ok?) 400))))))

(defn entries-handler [request]
  (let [book (normalize-book request)
        payload (:body request)
        entries (cond
                  (sequential? payload) payload
                  (sequential? (:entries payload)) (:entries payload)
                  :else nil)]
    (if-not (sequential? entries)
      (http/ok-json {:error "Expected entries list"
                     :book book}
                    400)
      (let [result (docbook/upsert-entries! book entries)]
        (if (:ok? result)
          (let [verification (maybe-verify-docbook request)]
            (if (and verification (not (:ok? verification)))
              (http/ok-json {:error "Docbook invariants failed"
                             :book book
                             :invariants verification
                             :result (dissoc result :ok?)}
                            409)
              (http/ok-json (cond-> (dissoc result :ok?)
                              verification (assoc :invariants verification)))))
          (http/ok-json (dissoc result :ok?) 400))))))

(defn delete-handler [request]
  (let [book (normalize-book request)
        doc-id (some-> (get-in request [:path-params :doc-id]) str/trim not-empty)]
    ;; TODO: Gate docbook deletes behind an ACL/token before exposing broadly.
    (if-not doc-id
      (http/ok-json {:error "doc-id required"
                     :book book}
                    400)
      (let [{:keys [deleted] :as result} (docbook/delete-doc! book doc-id)]
        (if (pos? deleted)
          (http/ok-json result)
          (http/ok-json (assoc result :error "Doc not found") 404))))))

(defn delete-toc-handler [request]
  (let [book (normalize-book request)
        doc-id (some-> (get-in request [:path-params :doc-id]) str/trim not-empty)
        cascade? (truthy-param? (get-in request [:query-params "cascade"]))]
    ;; TODO: Gate docbook deletes behind an ACL/token before exposing broadly.
    (if-not doc-id
      (http/ok-json {:error "doc-id required"
                     :book book}
                    400)
      (let [{:keys [deleted] :as result} (docbook/delete-toc! book doc-id
                                                             {:cascade? cascade?})]
        (if (pos? deleted)
          (http/ok-json result)
          (http/ok-json (assoc result :error "Doc not found") 404))))))
