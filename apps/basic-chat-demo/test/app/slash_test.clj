(ns app.slash-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [app.slash :as slash]
            [app.store :as store]
            [app.command-service :as svc]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

(def ^:dynamic *conn* nil)
(def ^:dynamic *env* nil)

(defn- temp-dir []
  (-> (Files/createTempDirectory "slash-test" (make-array FileAttribute 0))
      (.toFile)))

(defn- delete-recursively [^java.io.File f]
  (when (.exists f)
    (doseq [file (reverse (file-seq f))]
      (io/delete-file file true))))

(use-fixtures
  :each
  (fn [f]
    (let [tmp (temp-dir)
          env {:data-dir (.getAbsolutePath tmp)
               :snapshot-every 100
               :xtdb {:enabled? false}}
          conn (store/restore! env)]
      (store-manager/configure! {:data-root (.getAbsolutePath tmp)})
      (try
        (binding [*conn* conn
                  *env* env]
          (f))
        (finally
          (store-manager/shutdown!)
          (delete-recursively tmp))))))

(defn- run-command
  ([line]
   (run-command line {}))
  ([line state]
   (let [handler (slash/handler *conn* *env*)]
     (handler line state))))

(deftest types-command-with-data
  (testing "types command lists types from the database"
    (store/ensure-entity! *conn* *env* {:name "Barney" :type :person})
    (let [{:keys [message]} (run-command "types")]
      (is (some #(str/includes? % "person") message)))))

(deftest help-includes-new-commands
  (let [{:keys [message]} (run-command "help")]
    (is (seq message))
    (is (some #(str/includes? % "/entity") message))
    (is (some #(str/includes? % "/types") message))))

(deftest entity-and-relation-commands
  (let [{entity-msg :message state :new-state} (run-command "entity Alice person")
        {:keys [message new-state]} (run-command "relation knows Alice Bob" state)
        tail (:message (run-command "tail" new-state))]
    (is (some #(str/includes? % "Entity ensured") entity-msg))
    (is (some #(str/includes? % "Relation ensured") message))
    (is (= ["Recent relations:" "  (none)"] tail))))

(deftest me-summary-command
  (let [{:keys [message]} (run-command "me summary 10")]
    (is (>= (count message) 3))
    (is (str/includes? (first message) "Profile:"))))

(deftest me-summary-includes-me-relations
  (testing "/me summary includes relations linked to the generic :me entity"
    (let [joe-id (UUID/randomUUID)
          willie-id (UUID/randomUUID)
          config-path (-> (io/resource "xtdb-test.edn") io/file .getAbsolutePath)]
      (xt/start! config-path {:data-dir (:data-dir *env*)})
      (try
        (xt/put-entity! {:entity/id joe-id :entity/name "Joe Corneli" :entity/type :person})
        (xt/put-entity! {:entity/id willie-id :entity/name "Willie Dixon" :entity/type :person})
        (xt/put-rel! {:relation/id (UUID/randomUUID) :relation/type :likes :relation/src :me :relation/dst willie-id} nil nil)
        (store-manager/upsert-profile! (store-manager/default-profile) {:name "Joe Corneli"})
        (let [summary (svc/profile-summary {:profile :me} nil)]
          (is (str/includes? (:text summary) "Willie Dixon")))
        (finally
          (xt/stop!))))))

(deftest me-summary-includes-me-relations
  (testing "/me summary includes relations linked to the generic :me entity"
    (let [joe-id (UUID/randomUUID)
          uk-id (UUID/randomUUID)
          config-path (-> (io/resource "xtdb-test.edn") io/file .getAbsolutePath)]
      (xt/start! config-path {:data-dir (:data-dir *env*)})
      (try
        (xt/put-entity! {:entity/id joe-id :entity/name "Joe Corneli" :entity/type :person})
        (xt/put-entity! {:entity/id uk-id :entity/name "UK" :entity/type :place})
        (xt/put-rel! {:relation/id (UUID/randomUUID) :relation/type :lives-in :relation/src :me :relation/dst uk-id} nil nil)
        (store-manager/upsert-profile! (store-manager/default-profile) {:name "Joe Corneli"})
        (let [summary (svc/profile-summary {:profile :me} nil)]
          (is (str/includes? (:text summary) "UK")))
        (finally
          (xt/stop!))))))

(deftest me-summary-includes-me-relations
  (testing "/me summary includes relations linked to the generic :me entity"
    (let [joe-id (UUID/randomUUID)
          uk-id (UUID/randomUUID)
          config-path (-> (io/resource "xtdb-test.edn") io/file .getAbsolutePath)]
      (xt/start! config-path {:data-dir (:data-dir *env*)})
      (try
        (xt/put-entity! {:entity/id joe-id :entity/name "Joe Corneli" :entity/type :person})
        (xt/put-entity! {:entity/id uk-id :entity/name "UK" :entity/type :place})
        (xt/put-rel! {:relation/id (UUID/randomUUID) :relation/type :lives-in :relation/src :me :relation/dst uk-id} nil nil)
        (xt/sync-node!)
        (store-manager/upsert-profile! (store-manager/default-profile) {:name "Joe Corneli"})
        (let [summary (svc/profile-summary {:profile :me} nil)]
          (is (str/includes? (:text summary) "UK")))
        (finally
          (xt/stop!))))))

(deftest types-command
  (let [{:keys [message]} (run-command "types")]
    (is (= "Registered types:" (first message)))
    (is (>= (count message) 2))))

