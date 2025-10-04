(ns basic-chat-demo.basic-chat-demo-test
  (:require [clojure.test :refer [deftest is testing]]
            [basic-chat-demo.basic-chat-demo :as sut]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.io :as io])) ; system under test

(defn run-script
  ([script-path]
   (run-script "basic-chat/v1" script-path))
  ([protocol script-path]
   (let [{:keys [out err exit]}
         (sh/sh "clojure" "-M:run-m" "--"
                "--protocol" protocol
                "--script" script-path)]
    (is (zero? exit) (str "non-zero exit: " err))
    (edn/read-string out))))

(deftest hello-script
  (let [got (run-script "test/scripts/hello.edn")
        ;; normalize: replace unpredictable bits in :links with {}
        got' (mapv #(update % :links (fn [ls] (mapv (constantly {}) ls))) got)
        exp (-> "test/golden/hello.out.edn" slurp edn/read-string)]
    (is (= exp got'))))

(deftest v2-script
  (let [got (run-script "basic-chat/v2" "test/scripts/v2-basic.edn")
        exp (-> "test/golden/v2-basic.out.edn" slurp edn/read-string)]
    (is (= exp got))))
