(ns basic-chat-demo.basic-chat-demo-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [basic-chat-demo.basic-chat-demo :as sut]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute))) ; system under test

(defn- temp-dir []
  (-> (Files/createTempDirectory "basic-chat-demo-test" (make-array FileAttribute 0))
      (.toFile)))

(defn- delete-recursively [f]
  (when (.exists f)
    (doseq [file (reverse (file-seq f))]
      (io/delete-file file true))))

(use-fixtures
  :each
  (fn [f]
    (let [tmp (temp-dir)
          orig-env @sut/!env
          orig-conn @sut/!conn]
      (reset! sut/!env {:data-dir (.getAbsolutePath tmp)
                        :snapshot-every 100})
      (reset! sut/!conn nil)
      (try
        (f)
        (finally
          (reset! sut/!conn orig-conn)
          (reset! sut/!env orig-env)
          (delete-recursively tmp))))))

(defn- temp-dir []
  (let [file (java.io.File/createTempFile "basic-chat-test" "")]
    (io/delete-file file)
    (.mkdirs file)
    (.deleteOnExit file)
    (.getAbsolutePath file)))

(defn run-script
  ([script-path]
   (run-script "basic-chat/v1" script-path))
  ([protocol script-path]
   (run-script protocol script-path []))
  ([protocol script-path extra-cli]
   (let [args (concat ["--protocol" protocol
                       "--script" script-path]
                      extra-cli)
         out (with-out-str (apply sut/-main args))
         trimmed (str/trim out)]
     (is (seq trimmed) "expected EDN output from script run")
     (edn/read-string trimmed))))

(defn round2 [n]
  (when (number? n)
    (/ (Math/round (* n 100.0)) 100.0)))

(deftest hello-script
  (let [got (run-script "test/scripts/hello.edn")
        ;; normalize: replace unpredictable bits in :links with {}
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context :focus-header :focus-header-json)
                         (update :links (fn [ls] (mapv (constantly {}) ls)))))
                   got)
        exp (-> "test/golden/hello.out.edn" slurp edn/read-string)]
    (is (= exp got'))))

(deftest v2-script
  (let [got (mapv #(dissoc % :context :focus-header :focus-header-json)
                  (run-script "basic-chat/v2" "test/scripts/v2-basic.edn"))
        exp (-> "test/golden/v2-basic.out.edn" slurp edn/read-string)]
    (is (= exp got))))

(deftest v3-script
  (let [got (run-script "basic-chat/v3" "test/scripts/basic-chat/v3/entities.edn")
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context :focus-header :focus-header-json)
                         (update :entities (fn [ents]
                                             (mapv #(select-keys % [:name :type]) ents)))
                         (update :relations (fn [rels]
                                              (mapv #(select-keys % [:type]) rels)))))
                   got)
        exp (-> "test/golden/basic-chat/v3/entities.out.edn" slurp edn/read-string)]
    (is (= exp got'))))

(deftest v4-script
  (let [got (run-script "basic-chat/v4" "test/scripts/basic-chat/v4/entities.edn" ["--ner-fallback"])
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context :focus-header :focus-header-json)
                         (update :intent #(select-keys % [:type :conf]))
                         (update :links (fn [links]
                                          (mapv (constantly {}) links)))
                         (update :entities (fn [ents]
                                             (mapv (fn [ent]
                                                     (-> ent
                                                         (select-keys [:name :type :source :confidence :id :span :aliases :value])
                                                         (update :confidence round2)
                                                         (update :aliases #(vec %))))
                                                   ents)))
                         (update :relations (fn [rels]
                                              (mapv #(select-keys % [:type :src :dst]) rels)))))
                   got)
        exp (-> "test/golden/basic-chat/v4/entities.out.edn" slurp edn/read-string)]
    (is (= exp got'))))
