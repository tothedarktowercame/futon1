(ns app.store-manager-test
  (:require [app.store-manager :as store-manager]
            [clojure.test :refer [deftest is]]))

(deftest default-config-uses-cli-penholder-when-env-missing
  (with-redefs-fn {#'app.store-manager/getenv-trim (fn [_] nil)}
    #(is (= "cli" (:model/penholder (store-manager/default-config))))))
