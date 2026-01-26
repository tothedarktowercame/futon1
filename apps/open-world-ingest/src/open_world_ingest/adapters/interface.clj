(ns open-world-ingest.adapters.interface
  (:require [charon.core :as charon]
            [open-world-ingest.nlp :as nlp]
            [open-world-ingest.storage :as storage]))

(defn process-text
  "Normalize, analyze, and persist TEXT.

  Options map may include:
  - :now (java.time.Instant or ms since epoch)
  - :trace-openie configuration for tracing triples
  - :profile metadata (not persisted, but returned for caller context)

  Returns a map {:text … :entities […] :relations […] :storage/result …}."
  [text {:keys [now trace-openie] :as opts}]
  (let [_ (storage/start-node! {:data-dir "data/open-world"})
        instant (cond
                  (instance? java.time.Instant now) now
                  (some? now) (java.time.Instant/ofEpochMilli (long now))
                  :else (java.time.Instant/now))
        analysis (nlp/analyze text {:now instant
                                    :trace-openie trace-openie})
        storage-result (storage/store-analysis! text analysis)]
    (charon/ensure-ok storage-result)
    {:text text
     :time instant
     :entities (:entities analysis)
     :relations (:relations analysis)
     :storage storage-result
     :context opts}))
