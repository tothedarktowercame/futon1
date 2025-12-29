;; Scenario: verify docbook delete handler responses and book normalization.
(ns api.handlers.docbook-test
  (:require
   [api.handlers.docbook :as handler]
   [app.docbook :as docbook]
   [clojure.test :refer [deftest is testing]]))

(deftest delete-handler-rejects-missing-doc-id
  (let [response (handler/delete-handler {:path-params {:book "futon4"}})]
    (is (= 400 (:status response)))
    (is (= {:error "doc-id required"
            :book "futon4"}
           (:body response)))))

(deftest delete-handler-respects-default-book
  (let [seen (atom nil)]
    (with-redefs [docbook/delete-doc! (fn [book doc-id]
                                        (reset! seen [book doc-id])
                                        {:book book :doc-id doc-id :deleted 1})]
      (let [response (handler/delete-handler {:path-params {:doc-id "doc-1"}})]
        (is (= 200 (:status response)))
        (is (= ["futon4" "doc-1"] @seen))))))

(deftest delete-handler-returns-404-when-missing
  (with-redefs [docbook/delete-doc! (fn [book doc-id]
                                      {:book book :doc-id doc-id :deleted 0})]
    (let [response (handler/delete-handler {:path-params {:book "futon4"
                                                          :doc-id "doc-1"}})]
      (is (= 404 (:status response)))
      (is (= {:book "futon4"
              :doc-id "doc-1"
              :deleted 0
              :error "Doc not found"}
             (:body response))))))

(deftest delete-handler-returns-success-on-delete
  (with-redefs [docbook/delete-doc! (fn [book doc-id]
                                      {:book book :doc-id doc-id :deleted 2})]
    (let [response (handler/delete-handler {:path-params {:book "futon4"
                                                          :doc-id "doc-1"}})]
      (is (= 200 (:status response)))
      (is (= {:book "futon4"
              :doc-id "doc-1"
              :deleted 2}
             (:body response))))))

(deftest delete-toc-handler-respects-cascade
  (let [seen (atom nil)]
    (with-redefs [docbook/delete-toc! (fn [book doc-id opts]
                                        (reset! seen [book doc-id opts])
                                        {:book book :doc-id doc-id :deleted 1})]
      (let [response (handler/delete-toc-handler {:path-params {:book "futon4"
                                                                :doc-id "doc-1"}
                                                  :query-params {"cascade" "true"}})]
        (is (= 200 (:status response)))
        (is (= ["futon4" "doc-1" {:cascade? true}] @seen))))))

(deftest delete-toc-handler-returns-404-when-missing
  (with-redefs [docbook/delete-toc! (fn [book doc-id opts]
                                      {:book book :doc-id doc-id :deleted 0})]
    (let [response (handler/delete-toc-handler {:path-params {:book "futon4"
                                                              :doc-id "doc-1"}})]
      (is (= 404 (:status response)))
      (is (= {:book "futon4"
              :doc-id "doc-1"
              :deleted 0
              :error "Doc not found"}
             (:body response))))))

(deftest update-contents-order-rejects-missing-order
  (let [response (handler/update-contents-order-handler {:path-params {:book "futon4"}
                                                         :body {:source "client"}})]
    (is (= 400 (:status response)))
    (is (= {:error "order must be a list of doc-ids"
            :book "futon4"}
           (:body response)))))

(deftest update-contents-order-accepts-list
  (let [seen (atom nil)]
    (with-redefs [docbook/update-toc-order! (fn [book opts]
                                              (reset! seen [book opts])
                                              {:status "ok"
                                               :book book
                                               :count 2})]
      (let [response (handler/update-contents-order-handler
                      {:path-params {:book "futon4"}
                       :body {:order ["doc-1" "doc-2"]
                              :source "client"
                              :timestamp "2025-01-01T00:00:00Z"}})]
        (is (= 200 (:status response)))
        (is (= {:status "ok"
                :book "futon4"
                :count 2}
               (:body response)))
        (is (= ["futon4" {:order ["doc-1" "doc-2"]
                          :source "client"
                          :timestamp "2025-01-01T00:00:00Z"}]
               @seen))))))

(deftest toc-handler-respects-default-book
  (let [seen (atom nil)]
    (with-redefs [docbook/toc (fn [book]
                                (reset! seen book)
                                {:book book :headings []})]
      (let [response (handler/toc-handler {:path-params {}})]
        (is (= 200 (:status response)))
        (is (= "futon4" @seen))))))

(deftest toc-handler-returns-data
  (with-redefs [docbook/toc (fn [book]
                              {:book book
                               :headings [{:doc/id "doc-1"
                                           :doc/title "Title"}]})]
    (let [response (handler/toc-handler {:path-params {:book "futon4"}})]
      (is (= 200 (:status response)))
      (is (= {:book "futon4"
              :headings [{:doc/id "doc-1"
                          :doc/title "Title"}]}
             (:body response))))))
