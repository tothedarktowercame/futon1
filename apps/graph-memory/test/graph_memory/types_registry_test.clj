(ns graph-memory.types-registry-test
  (:require [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [graph-memory.types-registry :as types])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private xt-config
  (-> "xtdb.edn" io/resource io/file .getAbsolutePath))

(defn- temp-dir []
  (.toAbsolutePath (Files/createTempDirectory "types-registry" (make-array FileAttribute 0))))

(defn- delete-dir! [path]
  (let [file (.toFile path)]
    (when (.exists file)
      (doseq [child (.listFiles file)]
        (if (.isDirectory child)
          (delete-dir! (.toPath child))
          (.delete child)))
      (.delete file))))

(defn- with-node [f]
  (let [dir (temp-dir)]
    (try
      (xt/stop!)
      (xt/start! xt-config {:data-dir (.toString dir)})
      (types/load-cache!)
      (f)
      (finally
        (xt/stop!)
        (delete-dir! dir)))))

(use-fixtures :each with-node)

(defn- doc-of [kind type]
  (some #(when (and (= (:kind %) kind) (= (:id %) type)) %)
        (types/docs)))

(deftest ensure-infers-parents
  (testing "namespaced types infer wildcard parent"
    (types/ensure! :entity :work/project)
    (let [doc (doc-of :entity :work/project)]
      (is (= :work/* (:parent doc)))
      (is (:inferred? doc)))
    (let [parent-doc (doc-of :entity :work/*)]
      (is parent-doc)
      (is (nil? (:parent parent-doc)))))
  (testing "unnamespaced types consult namespace map"
    (types/ensure! :entity :person)
    (is (= :person/* (:parent (doc-of :entity :person))))))

(deftest merge-and-descendants
  (types/ensure! :entity [:person :work/project])
  (types/merge! :entity :person [:human])
  (let [canonical (doc-of :entity :person)
        alias-doc (doc-of :entity :human)]
    (is (contains? (:aliases canonical) :human))
    (is (= :person (:alias-of alias-doc))))
  (testing "descendants include aliases"
    (let [desc (types/descendants-of :entity :person)]
      (is (contains? desc :person))
      (is (contains? desc :human))))
  (testing "descendants resolve aliases"
    (let [desc (types/descendants-of :entity :human)]
      (is (contains? desc :person))
      (is (contains? desc :human))))
  (testing "effective predicate honors descendants"
    (let [pred (types/effective-pred :entity [:person])]
      (is (true? (pred :human)))
      (is (false? (pred :place))))))

(deftest merge-noop-on-empty-aliases
  (types/ensure! :entity :person)
  (let [before (doc-of :entity :person)]
    (types/merge! :entity :person [])
    (types/merge! :entity :person nil)
    (is (= (:aliases before)
           (:aliases (doc-of :entity :person)))))
  (testing "still allow adding aliases afterwards"
    (types/merge! :entity :person [:alias])
    (is (contains? (:aliases (doc-of :entity :person)) :alias))))

(deftest set-parent-overrides-inference
  (types/ensure! :entity :work/project)
  (types/set-parent! :entity :work/project :topic/*)
  (let [doc (doc-of :entity :work/project)]
    (is (= :topic/* (:parent doc)))
    (is (false? (:inferred? doc))))
  (testing "descendants use updated parent"
    (is (contains? (types/descendants-of :entity :topic/*) :work/project)))
  (testing "parent can be cleared"
    (types/set-parent! :entity :work/project nil)
    (is (nil? (:parent (doc-of :entity :work/project))))))

(deftest intent-kind-support
  (testing "intent types register and cache"
    (types/ensure! :intent :inquiry)
    (let [doc (doc-of :intent :inquiry)]
      (is doc)
      (is (= :intent (:kind doc)))
      (is (= :inquiry (:id doc))))
    (testing "descendants for intents"
      (types/set-parent! :intent :inquiry :intent/*)
      (let [doc (doc-of :intent :intent/*)]
        (is doc)
        (is (contains? (types/descendants-of :intent :intent/*) :inquiry))))))
