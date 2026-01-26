(ns scripts.futon3-ingest-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [app.store-manager :as store-manager]
            [scripts.futon3-ingest :as ingest]
            [datascript.core :as d]
            [clojure.java.io :as io])
  (:import (java.nio.file Files)))

(defn- temp-dir []
  (-> (Files/createTempDirectory "futon3-ingest" (make-array java.nio.file.attribute.FileAttribute 0))
      .toFile))

(defn- delete-recursively [^java.io.File f]
  (when f
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-recursively child)))
    (io/delete-file f true)))

(defn with-temp [f]
  (let [dir (temp-dir)]
    (try
      (f dir)
      (finally
        (delete-recursively dir)))))

(deftest parse-patterns-extracts-sigils
  (with-temp
   (fn [dir]
     (let [library-dir (doto (io/file dir "library") .mkdirs)
           file (io/file library-dir "sample.flexiarg")
           content "@arg sample/pattern\n@title Sample Pattern\n! instantiated-by: Example [🍒/好 🟣/真]\n"]
       (spit file content)
       (let [patterns (#'ingest/parse-patterns dir)]
         (is (= 1 (count patterns)))
         (is (= {:emoji "🍒" :hanzi "好"}
                (-> patterns first :sigils first))))))))

(deftest parse-devmaps-extracts-prototypes
  (with-temp
   (fn [dir]
     (let [holes-dir (doto (io/file dir "holes") .mkdirs)
           file (io/file holes-dir "futon3.devmap")
           content "@multiarg f3/devmap\n! instantiated-by: Prototype 1 — Example [🍒/好]\n"]
       (spit file content)
       (let [devmaps (#'ingest/parse-devmaps dir)]
         (is (= #{"f3/p1"} (set (keys devmaps)))))))))

(deftest ingest-patterns-rejects-on-invariants
  (with-redefs [ingest/ingest-patterns! (fn [& _]
                                          (throw (ex-info "bad pattern" {:pattern-id "bad"})))]
    (let [result (ingest/ingest-patterns* nil {} [{:id "bad"}])]
      (is (false? (:ok? result)))
      (is (= :charon/reject (:error result)))
      (is (= :patterns/ingest (:surface result)))
      (is (= :patterns/ingest-failed (:reason result))))))
