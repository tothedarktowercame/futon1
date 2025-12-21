(ns app.command-service-test
  (:require [app.command-service :as svc]
            [app.slash.format :as fmt]
            [app.store :as store]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon1.store.compat.alpha :as compat]
            [futon1.store.core :as core])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

(def ^:dynamic *env* nil)
(def ^:dynamic *conn* nil)
(def ^:dynamic *store* nil)

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "command-service-test" (make-array FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when f
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (rm-rf child)))
    (when (.exists f)
      (io/delete-file f true))))

(use-fixtures
  :each
  (fn [f]
    (let [dir (temp-dir)
          env {:data-dir (.getAbsolutePath dir)
               :snapshot-every 100
               :xtdb {:enabled? false}}
          conn (store/restore! env nil)
          arxana (compat/alpha-store {:conn conn :env env})]
      (try
        (binding [*env* env
                  *conn* conn
                  *store* arxana]
          (f))
        (finally
          (rm-rf dir))))))

(deftest tail-prefers-hx-events
  (testing "tail returns hyperedges with all ends and formatted extras"
    (doseq [ident ["math/group:Intro"
                   "math/group:Glossary"
                   "math/topic:Context"]]
      (core/put-article! *store* {:article/ident ident}))
    (let [hx {:hx/type :link/supports
              :hx/labels [:fresh]
              :hx/content {:text "beta"}
              :hx/ends [{:role :source
                         :article {:article/ident "math/group:Intro"}}
                        {:role :target
                         :article {:article/ident "math/group:Glossary"}}
                        {:role :context
                         :article {:article/ident "math/topic:Context"}}]}
          hx-id (core/add-event! *store* hx)
          rows (svc/tail {:arxana-store *store*} 5)
          row (first rows)
          lines (fmt/tail-lines rows)]
      (is (= 1 (count rows)))
      (is (= hx-id (:hx/id row)))
      (is (= 3 (count (:hx/ends row))))
      (is (some #(= :context (:role %)) (:hx/ends row)))
      (is (some #(str/includes? % "others context=") lines))
      (is (some #(str/includes? % "labels fresh") lines))
      (is (some #(str/includes? % "content {:text \"beta\"}") lines)))))

(deftest tail-falls-back-when-no-hx-store
  (testing "tail still works with legacy conn"
    (store/ensure-entity! *conn* *env* {:name "Alice"})
    (store/ensure-entity! *conn* *env* {:name "Bob"})
    (store/upsert-relation! *conn* *env* {:type :link/refers-to
                                         :src {:name "Alice"}
                                         :dst {:name "Bob"}})
    (let [rows (svc/tail {:conn *conn*} 5)]
      (is (= 1 (count rows)))
      (is (= :link/refers-to (:type (first rows)))))))

(deftest ego-includes-scholia-linked-by-identifiers
  (testing "ego merges relations recorded on canonical ident names"
    ;; canonical entity created by ingestion (named with ident)
    (store/ensure-entity! *conn* *env* {:name "arxana:article:Demo Article"})
    (store/ensure-entity! *conn* *env* {:name "Demo Scholium"})
    (store/upsert-relation! *conn* *env* {:type :arxana/scholium
                                         :src {:name "Demo Scholium"}
                                         :dst {:name "arxana:article:Demo Article"}})
    ;; human-friendly projection carrying external-id
    (store/ensure-entity! *conn* *env* {:name "Demo Article"
                                       :type :arxana/article
                                       :id "arxana:article:Demo Article"
                                       :external-id "arxana:article:Demo Article"
                                       :source "external"})
    (let [ego (svc/ego *conn* "Demo Article")
          incoming (:incoming ego)
          outgoing (:outgoing ego)
          neighbor (first incoming)
          links (:links ego)]
      (is (= "Demo Article" (get-in ego [:entity :name])))
      (is (= 1 (count incoming)))
      (is (= :arxana/scholium (:relation neighbor)))
      (is (= "Demo Scholium" (get-in neighbor [:entity :entity/name])))
      (is (= incoming (get links :incoming)))
      (is (= outgoing (get links :outgoing)))
      (is (= (vec (concat outgoing incoming)) (get links :all)))))
    (testing "canonical ident lookups also gain the human-friendly relations"
      (let [ego (svc/ego *conn* "arxana:article:Demo Article")
            incoming (:incoming ego)
            outgoing (:outgoing ego)
            neighbor (first incoming)
            links (:links ego)]
        (is (= 1 (count incoming)))
        (is (= :arxana/scholium (:relation neighbor)))
        (is (= "Demo Scholium" (get-in neighbor [:entity :entity/name])))
        (is (= incoming (get links :incoming)))
        (is (= outgoing (get links :outgoing)))
        (is (= (vec (concat outgoing incoming)) (get links :all))))))

(deftest ego-resolves-uuid-aliases
  (testing "ego walks canonical relations recorded under entity-id strings"
    (let [slug-id (UUID/randomUUID)
          slug-name "p4ng2"
          canonical-name (str slug-id)]
      ;; canonical record uses the UUID string as its :name
      (store/ensure-entity! *conn* *env* {:name canonical-name})
      ;; human-friendly projection uses slug + explicit id
      (store/ensure-entity! *conn* *env* {:name slug-name :id slug-id :type :pattern/language})
      (store/ensure-entity! *conn* *env* {:name "pattern/alpha" :type :pattern/language})
      (store/upsert-relation! *conn* *env* {:type :pattern-language/includes
                                           :src {:name canonical-name}
                                           :dst {:name "pattern/alpha"}})
      (let [ego (svc/ego *conn* slug-name)
            outgoing (:outgoing ego)]
        (is (= 1 (count outgoing)))
        (is (= :pattern-language/includes (:relation (first outgoing))))
        (is (= "pattern/alpha" (get-in (first outgoing) [:entity :entity/name])))))))
