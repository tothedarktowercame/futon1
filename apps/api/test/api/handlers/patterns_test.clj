(ns api.handlers.patterns-test
  (:require [api.handlers.patterns :as handler]
            [app.patterns :as patterns]
            [app.store-manager :as store-manager]
            [clojure.test :refer :all]))

(deftest registry-handler-returns-data
  (with-redefs [store-manager/conn (fn [_] ::conn)
                store-manager/default-profile (fn [] "default")
                patterns/registry (fn [_] {:generated-at 123})]
    (let [response (handler/registry-handler {:headers {"x-profile" "default"}})]
      (is (= 200 (:status response)))
      (is (= "default" (get-in response [:body :profile])))
      (is (= 123 (get-in response [:body :registry :generated-at]))))))
