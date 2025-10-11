(ns nlp-interface.intent-fallback-test
  (:require [clojure.test :refer [deftest is testing]]
            [nlp-interface.intent :as intent]))

(deftest fallback-promotes-structured-requests
  (testing "scheduling phrases promote the scheduling intent"
    (let [{:keys [type conf source intent-candidates]}
          (intent/analyze "Please schedule a project sync tomorrow afternoon.")]
      (is (= :scheduling type))
      (is (= :fallback source))
      (is (>= conf 0.58))
      (is (seq intent-candidates))
      (is (= :scheduling (:type (first intent-candidates))))))
  (testing "negation around failures promotes support requests"
    (let [{:keys [type conf source intent-candidates]}
          (intent/analyze "We can't deploy because the service keeps failing.")]
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
