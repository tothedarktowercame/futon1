(ns protocols.basic-chat.v6
  (:require [clojure.string :as str]
            [graph-memory.main :as gm]
            [open-world-ingest.nlp :as ingest-nlp])
  (:import (java.io File)))

(def intro
  ["Protocol basic-chat/v6 — routes utterances through open-world ingest so"
   "entities/relations reflect the OpenIE pipeline and persist in XTDB."])

(defn init [] {:node (atom nil)})
(defn configure [ctx _] ctx)

;; Optional env flag for quick visibility while debugging
(def ^:private debug? (some? (System/getenv "BASIC_CHAT_V6_DEBUG")))

(defn handle
  "Analyze text and return {:entities … :relations …} without persisting.
   API layer (process-turn!) is responsible for persistence and focus header."
  [ctx line ts]
  (let [trimmed (str/trim (or line ""))]
    (when debug?
      (println "[v6] handle text:" (pr-str trimmed) "ts:" ts))
    (let [now-inst (java.time.Instant/ofEpochMilli ts)
          analysis (try
                     (ingest-nlp/analyze trimmed {:now now-inst})
                     (catch Throwable t
                       (when debug? (println "[v6] analyze error:" (.getMessage t)))
                       nil))
          ents     (vec (for [{:entity/keys [label kind id] :as e} (or (:entities analysis) [])
                              :when (some? label)]
                          {:name label
                           :type kind
                           :id   id}))
          rels     (vec (for [{:relation/keys [label src dst subject object confidence polarity]}
                              (or (:relations analysis) [])
                              :when (and subject object label)]
                          {:type       label
                           :src        subject
                           :dst        object
                           :confidence confidence
                           :polarity   polarity
                           :src-id     src
                           :dst-id     dst}))]
      (when debug?
        (println "[v6] ents:" (count ents) "rels:" (count rels)))
      {:in        line
       :intent    nil
       :open-world {:entities (:entities analysis)
                    :relations (:relations analysis)}
       :entities  ents
       :relations rels
       :tokens    []
       :pos       []
       :links     []})))
