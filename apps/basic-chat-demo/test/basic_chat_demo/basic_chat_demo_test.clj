(ns basic-chat-demo.basic-chat-demo-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [basic-chat-demo.basic-chat-demo :as sut]
            [app.store-manager :as store-manager]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute))) ; system under test

(defn- temp-dir-file []
  (-> (Files/createTempDirectory "basic-chat-demo-test" (make-array FileAttribute 0))
      (.toFile)))

(defn- delete-recursively [f]
  (when (.exists f)
    (doseq [file (reverse (file-seq f))]
      (io/delete-file file true))))

(def ^:dynamic *data-root* nil)

(use-fixtures
  :each
  (fn [f]
    (let [tmp (temp-dir-file)]
      (binding [*data-root* (.getAbsolutePath tmp)]
        (store-manager/configure! {:data-root *data-root*
                                   :xtdb {:enabled? false}})
        (try
          (f)
          (finally
            (store-manager/shutdown!)
            (delete-recursively tmp)))))))

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
                         (dissoc :context :focus-header :focus-header-json :focus-header-lines)
                         (update :intent #(select-keys % [:type :conf]))
                         (update :links (fn [ls] (mapv (constantly {}) ls)))))
                   got)
        exp (-> "test/golden/hello.out.edn" slurp edn/read-string)]
    (is (= exp got'))))

(deftest v2-script
  (let [got (mapv #(dissoc % :context :focus-header :focus-header-json :focus-header-lines)
                  (run-script "basic-chat/v2" "test/scripts/v2-basic.edn"))
        exp (-> "test/golden/v2-basic.out.edn" slurp edn/read-string)]
    (is (= exp got))))

(deftest v3-script
  (let [got (run-script "basic-chat/v3" "test/scripts/basic-chat/v3/entities.edn" ["--no-context"])
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context :focus-header :focus-header-json :focus-header-lines)
                         (update :intent #(select-keys % [:type :conf]))
                         (update :entities (fn [ents]
                                             (mapv #(select-keys % [:name :type]) ents)))
                         (update :relations (fn [rels]
                                              (mapv #(select-keys % [:type]) rels)))))
                   got)
        exp (-> "test/golden/basic-chat/v3/entities.out.edn" slurp edn/read-string)]
    (is (= exp got'))))



(deftest human-lines-formats-intent
  (let [lines (#'sut/human-lines {:intent {:type :greet :conf 0.99}} nil)
        no-intent (#'sut/human-lines {:in "Who are you?"} nil)]
    (is (= ["Intent: greet (confidence 0.99)"] lines))
    (is (= ["No structured data extracted for: Who are you?"] no-intent))))

(deftest focus-policy-overrides
  (let [f #'sut/focus-policy-overrides
        sample {:neighbors 4
                 :context-cap 12
                 :allow-works? false
                 :focus-days 45}
        partial {:neighbors 2
                 :context-cap nil
                 :allow-works? nil
                 :focus-days nil}
        none {}]
    (is (= {:k-per-anchor 4
            :context-cap-total 12
            :allow-works? false
            :focus-days 45}
           (f sample)))
    (is (= {:k-per-anchor 2}
           (f partial)))
    (is (= {}
           (f none)))))

(deftest supports-entity-commands
  (let [supports? #'sut/supports-entity-commands?]
    (doseq [id ["basic-chat/v3" "basic-chat/v4" "basic-chat/v5" "basic-chat/v6"]]
      (is (supports? id) (str id " should enable slash commands")))
    (doseq [id ["basic-chat/v1" "basic-chat/v2" "unknown"]]
      (is (not (supports? id)) (str id " should not enable slash commands")))))
