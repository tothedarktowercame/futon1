(ns open-world-ingest.adapters-interface-test
  (:require [charon.core :as charon]
            [clojure.test :refer [deftest is]]
            [open-world-ingest.adapters.interface :as sut]
            [open-world-ingest.storage :as storage])
  (:import (java.time Instant)))

(deftest process-text-persists-analysis
  (let [text "Alice works at Acme Corp."
        now (Instant/parse "2025-01-01T00:00:00Z")
        result (sut/process-text text {:now now})
        {:keys [entities relations]} result]
    (is (seq entities))
    (is (seq relations))
    (is (= text (:text result)))
    (is (= now (:time result)))
    ;; ensure storage accepted the write
    (is (contains? result :storage))))

(deftest process-text-throws-on-charon-reject
  (with-redefs [storage/start-node! (fn [_] nil)
                storage/store-analysis! (fn [_ _]
                                          (charon/reject :open-world/ingest
                                                         :open-world/missing-relation-endpoints
                                                         {:errors [{:missing [:src]}]}))]
    (try
      (sut/process-text "Alice works at Acme." {})
      (is false "Expected Charon rejection to throw")
      (catch clojure.lang.ExceptionInfo ex
        (is (= "Charon rejected ingest" (.getMessage ex)))
        (is (false? (:ok? (ex-data ex))))))))
