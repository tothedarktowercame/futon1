;; apps/graph-memory/test/app/docbook_test.clj
(ns app.docbook-test
  (:require [app.docbook :as docbook]
            [app.xt :as xt]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import (java.nio.file Files)))

(defn- temp-dir []
  (-> (Files/createTempDirectory "docbook-test" (make-array java.nio.file.attribute.FileAttribute 0))
      .toFile))

(defn- delete-recursively [^java.io.File f]
  (when f
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-recursively child)))
    (io/delete-file f true)))

(deftest ingest-rejects-invalid-docbook-entry
  (let [root (temp-dir)]
    (try
      (let [raw-dir (doto (io/file root "data" "logs" "books" "futon4" "raw") .mkdirs)
            bad-entry {:doc_id "futon4-bad"
                       :entry_id "futon4-bad::org"
                       :book_id "futon4"}]
        (spit (io/file raw-dir "bad.json") (json/write-str bad-entry))
        (with-redefs [docbook/ensure-xt-node! (fn [] nil)
                      xt/submit! (fn [& _] nil)]
          (let [result (docbook/ingest! {:root (.getAbsolutePath root)
                                         :book "futon4"})]
            (is (false? (:ok? result)))
            (is (= :charon/reject (:error result)))
            (is (= :docbook/ingest (:surface result)))
            (is (= :docbook/invalid-entry (:reason result))))))
      (finally
        (delete-recursively root)))))
