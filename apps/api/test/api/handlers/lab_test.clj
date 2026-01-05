(ns api.handlers.lab-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [api.server :as server]
            [app.store-manager :as store-manager])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)))

(defn- temp-dir []
  (.toAbsolutePath (Files/createTempDirectory "lab-test" (make-array java.nio.file.attribute.FileAttribute 0))))

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
  (let [resp (.send client req (HttpResponse$BodyHandlers/ofString StandardCharsets/UTF_8))]
    {:status (.statusCode resp)
     :body (.body resp)
     :headers (.map (.headers resp))}))

(defn- json-request [client method uri payload]
  (let [body (HttpRequest$BodyPublishers/ofString (json/generate-string payload) StandardCharsets/UTF_8)
        req (request method uri {"Content-Type" "application/json"} body)
        resp (send-request client req)]
    (assoc resp :json (when (seq (:body resp))
                        (json/parse-string (:body resp) true)))))

(defn- get-request [client uri]
  (send-request client (request "GET" uri {} nil)))

(deftest lab-session-ingest-and-fetch
  (let [data-dir (str (temp-dir))
        client (http-client)
        session-id "lab-001"
        payload {:lab/session-id session-id
                 :lab/repo-root "/tmp/futon3"
                 :lab/timestamp-start "2026-01-05T00:00:00Z"
                 :lab/timestamp-end "2026-01-05T01:00:00Z"
                 :lab/user-messages []
                 :lab/assistant-messages []
                 :lab/files-touched []}]
    (store-manager/shutdown!)
    (let [{:keys [port]} (server/start! {:port 0 :data-root data-dir})]
      (try
        (testing "ingest lab session"
          (let [resp (json-request client "POST"
                                   (format "http://localhost:%d/api/α/lab/session" port)
                                   payload)]
            (is (= 200 (:status resp)))
            (is (= true (get-in resp [:json :ok])))
            (is (= session-id (get-in resp [:json :session-id])))))
        (testing "fetch lab session"
          (let [resp (get-request client
                                  (format "http://localhost:%d/api/α/lab/session/%s" port session-id))
                body (json/parse-string (:body resp) true)]
            (is (= 200 (:status resp)))
            (is (= true (:ok body)))
            (is (= session-id (:session-id body)))
            (is (contains? #{:lab/session "lab/session"}
                           (get-in body [:doc :entity/type])))
            (is (= session-id (get-in body [:doc :lab/session-id])))))
        (finally
          (server/stop!))))))

(deftest lab-session-requires-id
  (let [data-dir (str (temp-dir))
        client (http-client)]
    (store-manager/shutdown!)
    (let [{:keys [port]} (server/start! {:port 0 :data-root data-dir})]
      (try
        (let [resp (json-request client "POST"
                                 (format "http://localhost:%d/api/α/lab/session" port)
                                 {:lab/repo-root "/tmp"})]
          (is (= 400 (:status resp))))
        (finally
          (server/stop!))))))
