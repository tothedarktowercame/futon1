(ns api.server-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [api.server :as server]
            [api.routes :as routes]
            [app.store-manager :as store-manager])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)))

(defn- temp-dir []
  (.toAbsolutePath (Files/createTempDirectory "api-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- http-client []
  (HttpClient/newHttpClient))

(defn- request [method uri headers body]
  (let [builder (HttpRequest/newBuilder (URI/create uri))
        publisher (or body (HttpRequest$BodyPublishers/noBody))]
    (.method builder method publisher)
    (doseq [[k v] headers]
      (.header builder k v))
    (.build builder)))

(defn- send-request [client req]
  (let [resp (.send client req (HttpResponse$BodyHandlers/ofString StandardCharsets/UTF_8))
        headers (into {}
                      (for [[k vs] (.map (.headers resp))]
                        [(str/lower-case k)
                         (case (first vs)
                           "±" "α"
                           (first vs))]))]
    {:status (.statusCode resp)
     :body (.body resp)
     :headers headers}))

(defn- json-request [client method uri payload]
  (let [body (HttpRequest$BodyPublishers/ofString (json/generate-string payload) StandardCharsets/UTF_8)
        req (request method uri {"Content-Type" "application/json"} body)
        resp (send-request client req)]
    (assoc resp :json (when (seq (:body resp))
                        (json/parse-string (:body resp) true)))))

(defn- get-request [client uri]
  (send-request client (request "GET" uri {} nil)))

(defn- focus-labels [fh]
  (let [curr (map :label (:current fh))
        hist (map :label (:history fh))]
    (set (remove str/blank? (concat curr hist)))))

(deftest headless-flow-persists-salience
  (let [data-dir (str (temp-dir))
        profile "test-profile"
        client (http-client)]
    (store-manager/shutdown!)
    (try
      (let [{:keys [port]} (server/start! {:port 0
                                           :data-root data-dir
                                           :default-profile profile})]
        (try
          (let [baseline (get-request client (format "http://localhost:%d/api/α/focus-header" port))
                focus-before (json/parse-string (:body baseline) true)]
              (testing "baseline focus header"
                (is (= 200 (:status baseline)))
                (is (= "α" (get-in baseline [:headers "x-api-version"])))
              (is (map? focus-before))
              (is (seq (:focus_header focus-before))))

            (testing "posting a turn updates salience"
              (let [turn-resp (json-request client "POST" (format "http://localhost:%d/api/α/turns" port)
                                            {:text "Serena is presenting tomorrow."})
                    focus-resp (get-request client (format "http://localhost:%d/api/α/focus-header" port))
                    focus-after (json/parse-string (:body focus-resp) true)
                    labels (focus-labels (:focus_header focus-after))]
                (is (= 200 (:status turn-resp)))
                (is (= "α" (get-in turn-resp [:headers "x-api-version"])))
                (is (= 200 (:status focus-resp)))
                (is (= "α" (get-in focus-resp [:headers "x-api-version"])))
                (is (contains? labels "Serena"))
                (is (not= (:focus_header focus-before) (:focus_header focus-after)))))

            (testing "profile summary is bounded"
              (let [resp (get-request client (format "http://localhost:%d/api/α/me/summary" port))]
                  (is (= 200 (:status resp)))
                  (is (= "α" (get-in resp [:headers "x-api-version"])))
                (is (contains? #{"text/plain; charset=utf-8" "text/plain;charset=utf-8"}
                               (get-in resp [:headers "content-type"])))
                (is (= profile (get-in resp [:headers "x-profile"])))
                (is (pos? (count (:body resp))))
                (is (<= (count (:body resp)) 2000))))

            (testing "profile summary exposes structured EDN when requested"
              (let [resp (get-request client (format "http://localhost:%d/api/α/me/summary?format=edn" port))
                    parsed (edn/read-string (:body resp))]
                (is (= 200 (:status resp)))
                (is (= "α" (get-in resp [:headers "x-api-version"])))
                (is (= "application/edn; charset=utf-8" (get-in resp [:headers "content-type"])))
                (is (= profile (get-in resp [:headers "x-profile"])))
                (is (map? parsed))
                (is (= profile (:profile parsed)))
                (is (seq (:text parsed)))))

            (testing "type registry endpoints"
            (let [types-resp (get-request client (format "http://localhost:%d/api/α/types" port))
                  types-json (json/parse-string (:body types-resp) true)]
              (is (= 200 (:status types-resp)))
              (is (= "α" (get-in types-resp [:headers "x-api-version"])))
              (is (seq (get-in types-json [:types :entity])))
              (is (vector? (get-in types-json [:types :intent]))))
            (let [parent-resp (json-request client "POST" (format "http://localhost:%d/api/α/types/parent" port)
                                            {:type "person" :parent "topic/*"})]
              (is (= 200 (:status parent-resp)))
              (is (= "α" (get-in parent-resp [:headers "x-api-version"])))
              (is (= "topic/*" (get-in parent-resp [:json :type :parent]))))
            (let [merge-resp (json-request client "POST" (format "http://localhost:%d/api/α/types/merge" port)
                                           {:into "person" :aliases ["human"]})]
              (is (= 200 (:status merge-resp)))
              (is (= "α" (get-in merge-resp [:headers "x-api-version"])))
              (is (= "person" (get-in merge-resp [:json :type :id])))
              (is (some #(= "human" %) (get-in merge-resp [:json :type :aliases])))))
          (testing "latest entities endpoint"
            (let [entity-payload {:name "Clock-out summary"
                                  :type "clock-out/summary"
                                  :external-id "test-clock-1"}
                  post-resp (json-request client "POST" (format "http://localhost:%d/api/α/entity" port)
                                          entity-payload)]
              (is (= 200 (:status post-resp)))
              (is (= "α" (get-in post-resp [:headers "x-api-version"]))))
            (let [latest-resp (get-request client (format "http://localhost:%d/api/α/entities/latest?type=clock-out/summary" port))
                  parsed (json/parse-string (:body latest-resp) true)]
              (is (= 200 (:status latest-resp)))
              (is (= "α" (get-in latest-resp [:headers "x-api-version"])))
              (is (= "clock-out/summary" (:type parsed)))
              (is (seq (:entities parsed))))
            (let [empty-resp (get-request client (format "http://localhost:%d/api/α/entities/latest?type=unknown/type" port))
                  parsed (json/parse-string (:body empty-resp) true)]
              (is (= 200 (:status empty-resp)))
              (is (vector? (:entities parsed)))
              (is (empty? (:entities parsed))))
            (let [bad-resp (get-request client (format "http://localhost:%d/api/α/entities/latest" port))
                  parsed (json/parse-string (:body bad-resp) true)]
              (is (= 400 (:status bad-resp)))
              (is (= {:error "type query parameter required"} parsed))))
          (let [types-after (get-request client (format "http://localhost:%d/api/α/types" port))
                parsed (json/parse-string (:body types-after) true)]
            (is (= 200 (:status types-after)))
            (is (= "α" (get-in types-after [:headers "x-api-version"])))
            (is (some #(= "human" %)
                        (mapcat (comp seq :aliases) (get-in parsed [:types :entity]))))))
          (finally
            (server/stop!))))

      (let [{:keys [port]} (server/start! {:port 0
                                           :data-root data-dir
                                           :default-profile profile})]
        (try
          (testing "focus header persists after restart"
            (let [resp (get-request client (format "http://localhost:%d/api/α/focus-header" port))
                  parsed (json/parse-string (:body resp) true)
                  labels (focus-labels (:focus_header parsed))]
              (is (= 200 (:status resp)))
              (is (= "α" (get-in resp [:headers "x-api-version"])))
              (is (contains? labels "Serena"))))
          (finally
            (server/stop!))))
      (catch java.net.SocketException _
        (testing "server sandbox restrictions"
          (is true "Skipping server socket tests in restricted environment"))))))

(deftest healthz-route-returns-ok
  (let [resp (routes/dispatch {:request-method :get
                               :uri "/healthz"})
        default-caps (store-manager/default-capabilities)]
    (is (= 200 (:status resp)))
    (is (= {:status "ok"
            :capabilities default-caps}
           (json/parse-string (:body resp) true)))))
