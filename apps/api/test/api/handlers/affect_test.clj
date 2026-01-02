(ns api.handlers.affect-test
  (:require
   [api.handlers.affect :as affect]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is testing]]
   [open-world-ingest.affect-transitions :as transitions]))

(deftest affect-transitions-handler-requires-actor
  (let [response (affect/affect-transitions-handler {:ctx {:xt/db ::db
                                                           :now 1}
                                                     :query-params {}})]
    (is (= 400 (:status response)))
    (is (re-find #"actor_id" (:body response)))))

(deftest affect-transitions-handler-returns-payload
  (with-redefs [transitions/affect-transitions (fn [_db opts]
                                                 {:actor_id (name (:actor-id opts))
                                                  :counts {:utterances 1 :transitions 1}})]
    (let [response (affect/affect-transitions-handler {:ctx {:xt/db ::db
                                                             :now 1}
                                                       :query-params {"actor_id" ":me"}})
          body (json/parse-string (:body response) true)]
      (is (= 200 (:status response)))
      (is (= "me" (:actor_id body)))
      (is (= 1 (get-in body [:counts :transitions]))))))
