(ns api.handlers.graph-test
  (:require
   [api.handlers.graph :as graph]
   [clojure.test :refer [deftest is testing]]))

(deftest maybe-infer-relation-type-hoists-props
  (testing "props override the generic type"
    (is (= {:type :pattern-language/includes}
           (#'graph/maybe-infer-relation-type
            {:type :language/catalog
             :props {:relation/type :pattern-language/includes}}))))
  (testing "relation/type inside props becomes :type"
    (is (= {:type :pattern-language/includes}
           (#'graph/maybe-infer-relation-type
            {:props {:relation/type :pattern-language/includes}}))))
  (testing "link/type also hoisted"
    (is (= {:type :language/source}
           (#'graph/maybe-infer-relation-type
            {:props {:link/type :language/source}}))))
  (testing "relation/type on the body stays supported"
    (is (= {:type :relation/labeled}
           (#'graph/maybe-infer-relation-type
            {:relation/type :relation/labeled}))))
  (testing "pattern/type offers a fallback"
    (is (= {:type :pattern-language/includes}
           (#'graph/maybe-infer-relation-type
            {:props {:pattern/type :pattern-language/includes}}))))
  (testing "raw type is used when no semantic props provided"
    (is (= {:type :language/catalog}
           (#'graph/maybe-infer-relation-type
            {:type :language/catalog}))))
  (testing "label string inside props is parsed"
    (is (= {:type :pattern-language/includes}
           (#'graph/maybe-infer-relation-type
            {:props {:label ":pattern-language/includes"}})))))
