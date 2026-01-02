(ns api.handlers.patterns-test
  (:require [api.handlers.patterns :as handler]
            [app.patterns :as patterns]
            [app.store-manager :as store-manager]
            [cheshire.core :as json]
            [clojure.test :refer :all]))

(defn- body->json [response]
  (let [body (:body response)]
    (if (string? body)
      (json/parse-string body true)
      body)))

(deftest registry-handler-returns-data
  (with-redefs [store-manager/conn (fn [_] ::conn)
                store-manager/default-profile (fn [] "default")
                patterns/registry (fn [_] {:generated-at 123})]
    (let [response (handler/registry-handler {:headers {"x-profile" "default"}})]
      (let [body (body->json response)]
        (is (= 200 (:status response)))
        (is (= "default" (get-in body [:profile])))
        (is (= 123 (get-in body [:registry :generated-at])))))))
