(ns nlp-interface.intent-fallback-test
  (:require [clojure.test :refer [deftest is testing]]
            [nlp-interface.intent :as intent]))

(deftest fallback-promotes-structured-requests
  (testing "scheduling phrases promote the scheduling intent when dictionary is unsure"
    (let [{:keys [type conf source intent-candidates]}
          (intent/analyze "Need to schedule project sync tomorrow at 3pm.")]
      (is (= :scheduling type))
      (is (= :fallback source))
      (is (>= conf 0.58))
      (is (seq intent-candidates))
      (is (= :scheduling (:type (first intent-candidates))))))
  (testing "investigation language promotes support requests"
    (let [{:keys [type conf source intent-candidates]}
          (intent/analyze "Service outage persists; investigate failure.")]
      (is (= :support-request type))
      (is (= :fallback source))
      (is (>= conf 0.6))
      (is (seq intent-candidates))
      (is (= :support-request (:type (first intent-candidates)))))))

(deftest fallback-can-leave-intent-unknown
  (let [{:keys [type source intent-candidates]}
        (intent/analyze "The chair is red.")]
    (is (= :unknown type))
    (is (= :fallback source))
    (is (vector? intent-candidates))))
