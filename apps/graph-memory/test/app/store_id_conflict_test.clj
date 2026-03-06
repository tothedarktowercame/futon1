(ns app.store-id-conflict-test
  (:require [clojure.test :refer [deftest is testing]]
            [datascript.core :as d]
            [app.store :as store]))

(deftest custom-id-conflict-suggests-disambiguation
  (let [conn (d/create-conn store/schema)
        opts {:data-dir (str (java.nio.file.Files/createTempDirectory "store-id"
                                                                     (make-array java.nio.file.attribute.FileAttribute 0)))}]
    (store/ensure-entity! conn opts {:id "alice" :name "Alice" :type :person})
    (testing "conflict on same id with different name/type"
      (try
        (store/ensure-entity! conn opts {:id "alice" :name "Alice Two" :type :org})
        (is false "Expected conflict")
        (catch clojure.lang.ExceptionInfo ex
          (let [data (ex-data ex)]
            (is (= 409 (:status data)))
            (is (= :id-conflict (:reason data)))
            (is (string? (:suggested-id data)))))))))

(deftest custom-id-and-shared-external-id-do-not-collapse-ids
  (let [conn (d/create-conn store/schema)
        opts {:data-dir (str (java.nio.file.Files/createTempDirectory "store-id"
                                                                     (make-array java.nio.file.attribute.FileAttribute 0)))}
        patterns (store/ensure-entity! conn opts {:id "model/descriptor/patterns"
                                                  :name "model/descriptor/patterns"
                                                  :type :model/descriptor
                                                  :external-id "0.1.0"
                                                  :source {:model/scope :patterns
                                                           :schema/version "0.1.0"}})
        meta-model (store/ensure-entity! conn opts {:id "model/descriptor/meta-model"
                                                    :name "model/descriptor/meta-model"
                                                    :type :model/descriptor
                                                    :external-id "0.1.0"
                                                    :source {:model/scope :meta-model
                                                             :schema/version "0.1.0"}})]
    (is (not= (:id patterns) (:id meta-model)))
    (is (= "model/descriptor/patterns" (:name patterns)))
    (is (= "model/descriptor/meta-model" (:name meta-model)))
    (is (= "0.1.0" (:external-id patterns)))
    (is (= "0.1.0" (:external-id meta-model)))))
