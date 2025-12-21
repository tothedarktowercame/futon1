(ns api.handlers.docbook
  (:require [api.util.http :as http]
            [app.docbook :as docbook]
            [clojure.string :as str]))

(defn- normalize-book [request]
  (or (some-> (get-in request [:path-params :book]) str/trim not-empty)
      "futon4"))

(defn contents-handler [request]
  (let [book (normalize-book request)
        data (docbook/contents book)]
    (http/ok-json data)))

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
