(ns api.failure-log-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [api.server :as server])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (str (.toAbsolutePath (Files/createTempDirectory "api-fail-log" (make-array FileAttribute 0)))))

(deftest api-failure-log-captures-invalid-write
  (let [data-dir (temp-dir)
        log-path (str data-dir "/logs/api-failures.log")
        request {:request-method :post
                 :ctx {:env {:data-dir (io/file data-dir)}}
                 :uri "/api/α/entity"
                 :query-string ""
                 :headers {"x-profile" "default"}
                 :body {:type "arxana/media-lyrics"}}
        response {:status 400 :body {:error "Entity name is required"}}
        handler (#'server/wrap-failed-write-log (fn [_] response))]
    (handler request)
    (testing "failure log is written"
      (is (.exists (java.io.File. log-path)))
      (is (pos? (.length (java.io.File. log-path)))))))
