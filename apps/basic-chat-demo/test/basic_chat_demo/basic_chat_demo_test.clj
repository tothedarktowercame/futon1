(ns basic-chat-demo.basic-chat-demo-test
  (:require [clojure.test :refer [deftest is testing]]
            [basic-chat-demo.basic-chat-demo :as sut]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.io :as io])) ; system under test

(defn run-script [script-path]
  (let [{:keys [out err exit]}
        (sh/sh "clojure" "-M:run-m" "--" "--script" script-path
               :dir (.getPath (io/file "apps/basic-chat-demo")))]
    (is (zero? exit) (str "non-zero exit: " err))
    (edn/read-string out)))

(deftest hello-script
  (let [got (run-script "test/scripts/hello.edn")
        ;; normalize: replace unpredictable bits in :links with {}
        got' (mapv #(update % :links (fn [ls] (mapv (constantly {}) ls))) got)
        exp (-> "apps/basic-chat-demo/test/golden/hello.out.edn" slurp edn/read-string)]
    (is (= exp got'))))
