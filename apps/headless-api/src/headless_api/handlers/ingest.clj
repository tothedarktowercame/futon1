(ns headless_api.handlers.ingest
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [headless_api.handlers.turns :as turns]))

(defn- parse-ndjson [body]
  (->> (str/split-lines body)
       (map str/trim)
       (remove str/blank?)
       (map (fn [line]
              (json/parse-string line true)))))

(defn- split-sentences [text]
  (->> (str/split (or text "") #"(?<=[.!?])\s+")
       (map str/trim)
       (remove str/blank?)
       vec))

(defn- chunk-body [request body]
  (let [headers (:headers request)
        content-type (some-> (get headers "content-type") str/lower-case)
        chunking (some-> (get headers "x-chunking") str/lower-case)]
    (cond
      (and content-type (str/includes? content-type "application/x-ndjson"))
      (parse-ndjson body)

      (= chunking "sentences")
      (map (fn [sentence] {:text sentence}) (split-sentences body))

      :else
      (let [trimmed (str/trim body)]
        (if (str/blank? trimmed)
          []
          [{:text trimmed}])))))

(defn ingest!
  [request raw-body]
  (let [chunks (vec (chunk-body request raw-body))
        results (mapv (fn [chunk]
                        (turns/process-turn! request chunk))
                      chunks)]
    {:processed (count results)
     :results results}))
