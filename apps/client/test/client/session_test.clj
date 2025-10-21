(ns client.session-test
  (:require [app.command-service :as svc]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [datascript.core :as d]
            [clojure.test :refer [deftest is testing]]
            [client.api :as api])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn temp-dir []
  (-> (Files/createTempDirectory "client-session-test" (into-array FileAttribute []))
      (.toFile)))

(defn with-session-fixture [f]
  (let [dir (.getAbsolutePath (temp-dir))
        session (api/start {:data-root dir})]
    (try
      (f session)
      (finally
        (api/stop session)))))

(defn summary-text [session]
  (let [ctx ((:ctx-provider session))
        profile (:default-profile ctx)
        conn (:conn ctx)
        summary (svc/profile-summary {:profile profile
                                       :conn conn
                                       :now (System/currentTimeMillis)
                                       :xt-node (xt/node)}
                                      nil)]
    (:text summary)))

(deftest process-lines
  (with-session-fixture
   (fn [session]
     (testing "likes accumulate into profile summary"
       (let [like1 (api/run-line session "I like Willie Dixon.")
             like2 (api/run-line session "I like Red Mitchell.")
             text  (summary-text session)]
         (is (= :say (:type like1)))
         (is (= :say (:type like2)))
         (is (re-find #"Willie Dixon" text))
         (is (re-find #"Red Mitchell" text)))))))

(deftest run-script
  (with-session-fixture
   (fn [session]
     (let [lines ["I like Willie Dixon." "I like Red Mitchell." "I like Jimbo Wallace." "I live in Oxford."]
           results (api/run-script session lines)
           text    (summary-text session)
           conn    (:conn (store-manager/ctx :me))
           rel-types (set (map first (d/q '[:find ?t :where [?r :relation/type ?t]] @conn)))]
       (is (= 4 (count results)))
       (is (= [:say :say :say :say] (map :type results)))
       (doseq [name ["Willie Dixon" "Red Mitchell" "Jimbo Wallace" "Oxford"]]
         (is (re-find (re-pattern name) text)))
       (is (contains? rel-types :oxford/live-in))
       (is (not (rel-types :likes)))))))

(deftest me-summary-succeeds-on-first-call
  (with-session-fixture
   (fn [session]
     (testing "first /me summary renders without restarting XT"
       (let [env @(:env-atom session)
             _ (is (map? env))
             ctx ((:ctx-provider session))
             _ (is (some? (:xtdb-node ctx)))
             result (api/run-line session "/me summary")
             message (get-in result [:data :message])]
         (is (= :slash (:type result)))
         (is (vector? message))
         (is (seq message))
         (is (re-find #"Profile: " (first message))))))))

(deftest focus-header-tracks-history-and-context
  (with-session-fixture
   (fn [session]
     (let [run (fn [text]
                 (api/run-line session text))
           first-turn (run "I play piano")
           second-turn (run "You know Jane")
           third-turn (run "Robbie plays piano")
           fh1 (get-in first-turn [:data :focus-header-lines])
           fh2 (get-in second-turn [:data :focus-header-lines])
           fh3 (get-in third-turn [:data :focus-header-lines])]
       (testing "first turn only reflects current extraction"
         (is (seq fh1))
         (is (some #(re-find #"Current:" %) fh1))
         (is (not-any? #(re-find #"Recent:" %) fh1)))

       (testing "second turn includes recent context from prior relation"
         (is (seq fh2))
         (is (some #(re-find #"Recent:" %) fh2))
         (is (some #(re-find #"You -\[know\]-> Jane" %) fh2)))

       (testing "third turn shows enrichment linking back to piano relation"
         (is (seq fh3))
         (is (some #(re-find #"Enriched:" %) fh3))
         (is (some #(re-find #"Me -\[play\]-> piano" %) fh3)))))))
