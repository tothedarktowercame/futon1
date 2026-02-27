(ns api.handlers.nlp
  "Lightweight NLP endpoints for entity extraction (no storage)."
  (:require [api.util.http :as http]
            [clojure.string :as str]
            [open-world-ingest.nlp :as nlp])
  (:import (java.time Instant)))

(defn- normalize-entity
  "Convert internal entity format to API response format."
  [{:entity/keys [id label kind lower-label]}]
  {:id (str id)
   :label label
   :kind (name kind)
   :lower-label lower-label})

(defn analyze-handler
  "Extract entities from text using Stanford CoreNLP.

   POST /api/alpha/nlp/analyze
   Body: {:text \"Text to analyze\"}

   Returns: {:entities [{:id :label :kind :lower-label} ...]
             :relations [{:subject :object :relation ...} ...]
             :text \"original text\"}"
  [request]
  (let [body (:body request)
        text (some-> (:text body) str str/trim)]
    (if (str/blank? text)
      (http/ok-json {:error "text required"} 400)
      (try
        (let [now (Instant/now)
              {:keys [entities relations]} (nlp/analyze text {:now now})
              normalized-entities (->> entities
                                       (map normalize-entity)
                                       (distinct)
                                       vec)]
          (http/ok-json {:text text
                         :entities normalized-entities
                         :entity-count (count normalized-entities)
                         :relation-count (count relations)}))
        (catch Exception e
          (http/ok-json {:error "NLP analysis failed"
                         :message (.getMessage e)}
                        500))))))

(defn entities-handler
  "Extract just entity labels from text (lighter response).

   POST /api/alpha/nlp/entities
   Body: {:text \"Text to analyze\"}

   Returns: {:labels [\"entity1\" \"entity2\" ...]
             :entities [{:label :kind} ...]}"
  [request]
  (let [body (:body request)
        text (some-> (:text body) str str/trim)]
    (if (str/blank? text)
      (http/ok-json {:error "text required"} 400)
      (try
        (let [now (Instant/now)
              {:keys [entities]} (nlp/analyze text {:now now})
              ;; Filter to interesting entity kinds (not generic nouns)
              interesting-kinds #{:person :org :place :date :proper}
              filtered (->> entities
                            (filter #(or (interesting-kinds (:entity/kind %))
                                         (> (count (:entity/label %)) 5)))
                            (map (fn [{:entity/keys [label kind lower-label]}]
                                   {:label label
                                    :kind (name kind)
                                    :lower-label lower-label}))
                            distinct
                            vec)
              labels (->> filtered
                          (map :lower-label)
                          distinct
                          vec)]
          (http/ok-json {:text text
                         :labels labels
                         :entities filtered
                         :count (count labels)}))
        (catch Exception e
          (http/ok-json {:error "NLP analysis failed"
                         :message (.getMessage e)}
                        500))))))
