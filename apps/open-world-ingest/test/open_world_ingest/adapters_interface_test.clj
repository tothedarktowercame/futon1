(ns open-world-ingest.adapters-interface-test
  (:require [clojure.test :refer [deftest is]]
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
