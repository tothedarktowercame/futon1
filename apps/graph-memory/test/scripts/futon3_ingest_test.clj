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

(deftest parse-devmaps-handles-named-entries
  (testing "Named entries like 'Completion' are parsed with kebab-case IDs"
    (with-temp
     (fn [dir]
       (let [holes-dir (doto (io/file dir "holes") .mkdirs)
             file (io/file holes-dir "futon0.devmap")
             content (str "@multiarg f0/devmap\n"
                          "! instantiated-by: Prototype 0 — Core [🙇/亏]\n"
                          "! instantiated-by: Completion — Full Integration [🔚/结]\n")]
         (spit file content)
         (let [devmaps (#'ingest/parse-devmaps dir)]
           (is (= #{"f0/p0" "f0/completion"} (set (keys devmaps))))
           (is (= [{:emoji "🙇" :hanzi "亏"}] (get devmaps "f0/p0")))
           (is (= [{:emoji "🔚" :hanzi "结"}] (get devmaps "f0/completion")))))))))
