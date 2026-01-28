(ns scripts.rehydrate
  "Optional helper to rehydrate a running Futon1 API after ingest scripts run."
  (:require [clojure.string :as str])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)))

(def ^:private default-rehydrate-url "http://localhost:8080/api/alpha/__diag/rehydrate")

(defn- normalize-url [value]
  (some-> value str str/trim not-empty))

(defn resolve-url
  "Return the rehydrate URL or nil when disabled."
  [{:keys [rehydrate? rehydrate-url]}]
  (let [env-url (or (normalize-url (System/getenv "FUTON1_REHYDRATE_URL"))
                    (normalize-url (System/getenv "ALPHA_REHYDRATE_URL")))
        explicit (normalize-url rehydrate-url)]
    (cond
      (false? rehydrate?) nil
      (and (nil? rehydrate?) (nil? explicit) (nil? env-url)) nil
      :else (or explicit env-url default-rehydrate-url))))

(defn maybe-rehydrate!
  "POST to the rehydrate endpoint when configured. Logs outcome either way."
  [opts]
  (if-let [url (resolve-url opts)]
    (do
      (println (format "[rehydrate] POST %s" url))
      (try
        (let [client (HttpClient/newHttpClient)
              body (HttpRequest$BodyPublishers/ofString "{}" StandardCharsets/UTF_8)
              req (-> (HttpRequest/newBuilder (URI/create url))
                      (.header "Content-Type" "application/json")
                      (.POST body)
                      (.build))
              resp (.send client req (HttpResponse$BodyHandlers/ofString StandardCharsets/UTF_8))
              status (.statusCode resp)]
          (println (format "[rehydrate] status=%d" status))
          {:ok? (<= 200 status 299)
           :status status
           :body (.body resp)})
        (catch Exception ex
          (binding [*out* *err*]
            (println (format "[rehydrate] failed: %s" (.getMessage ex))))
          {:ok? false :error (.getMessage ex)})))
    (do
      (println "[rehydrate] skipped (no rehydrate URL configured)")
      {:ok? false :skipped? true})))
