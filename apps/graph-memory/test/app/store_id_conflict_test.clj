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
