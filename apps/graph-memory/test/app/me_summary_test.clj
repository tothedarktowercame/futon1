(ns app.me-summary-test
  (:require
   [app.command-service :as svc]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [xtdb.api :as xt])                    ;; ← alias as xt
  (:import (java.util UUID)))

(def mem-kv {:kv-store {:xtdb/module 'xtdb.mem-kv/->kv-store}})

(defn start-mem-node! []
  (xt/start-node {:xtdb/tx-log         mem-kv
                  :xtdb/document-store mem-kv
                  :xtdb/index-store    mem-kv}))

(defn put! [node doc]
  ;; ::xt/put expands to :xtdb.api/put — the required tx-op key
  (xt/await-tx node (xt/submit-tx node [[::xt/put doc]])))

(defn rel! [node rel-doc]
  (xt/await-tx node (xt/submit-tx node [[::xt/put rel-doc]])))

(deftest me-summary-includes-me-relations
  (testing "/me summary includes relations linked to the generic :me entity"
    (let [node (start-mem-node!)]
      (try
        (let [joe-id    (UUID/randomUUID)
              willie-id (UUID/randomUUID)
              uk-id     (UUID/randomUUID)]

          ;; seed entities (each doc needs an :xt/id)
          (put! node {:xt/id joe-id
                      :entity/id joe-id
                      :entity/name "Joe Corneli"
                      :entity/type :person})

          (put! node {:xt/id willie-id
                      :entity/id willie-id
                      :entity/name "Willie Dixon"
                      :entity/type :person})

          ;; relation: :me --likes--> Willie
          (rel! node {:xt/id (UUID/randomUUID)
                      :relation/id (UUID/randomUUID)
                      :relation/type :likes
                      :relation/src  :me
                      :relation/dst  willie-id})

          ;; call your summary (pass the node explicitly)
          (let [summary (svc/profile-summary {:profile :me :xt-node node} nil)
                text    (str (:text summary))]
            (is (str/includes? text "Willie Dixon")))

          ;; add a place + relation
          (put! node {:xt/id uk-id
                      :entity/id uk-id
                      :entity/name "UK"
                      :entity/type :place})

          (rel! node {:xt/id (UUID/randomUUID)
                      :relation/id (UUID/randomUUID)
                      :relation/type :lives-in
                      :relation/src  :me
                      :relation/dst  uk-id})

          (let [summary (svc/profile-summary {:profile :me :xt-node node} nil)
                text    (str (:text summary))]
            (is (str/includes? text "UK"))))
        (finally
          (.close node))))))

;; store_xtdb_test.clj
;; (deftest me-summary-command
;;   (let [node (start-mem-node!)]                    ;; ← private in-mem node for this test
;;     (try
;;       ;; inject the node into the state the command expects
;;       (let [state             (assoc @!state :xt-node node)
;;             {:keys [message]} (run-command "me summary 10" state)
;;             ;; force realization before closing node (avoid lazy eval after shutdown)
;;             message           (mapv str message)]
;;         (is (>= (count message) 3))
;;         (is (str/includes? (first message) "Profile:")))
;;       (finally
;;         (.close node)))))
