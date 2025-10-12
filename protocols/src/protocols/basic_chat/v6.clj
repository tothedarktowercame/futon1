(ns protocols.basic-chat.v6
  (:require [clojure.string :as str]
            [graph-memory.main :as gm]
            [open-world-ingest.nlp :as ingest-nlp]
            [open-world-ingest.storage :as ingest-storage])
  (:import (java.io File)))

(def intro
  ["Protocol basic-chat/v6 — routes utterances through open-world ingest so"
   "entities/relations reflect the OpenIE pipeline and persist in XTDB."])

(defn init []
  {:node (atom nil)})

(defn configure [ctx _]
  ctx)

(defn- data-root []
  (or (System/getProperty "basic-chat.data-root")
      (System/getenv "BASIC_CHAT_DATA_DIR")
      (.getAbsolutePath (File. "data"))))

(defn- open-world-dir []
  (.getAbsolutePath (File. (File. (data-root)) "open-world")))

(defn- ensure-node! [ctx]
  (if-let [node @(:node ctx)]
    node
    (let [node (ingest-storage/start-node! {:data-dir (open-world-dir)})]
      (reset! (:node ctx) node)
      node)))

(defn shutdown [_]
  (ingest-storage/stop!))

(defn handle [ctx line ts]
  (let [trimmed (str/trim line)
        _ (ensure-node! ctx)
        analysis (ingest-nlp/analyze trimmed {:now (java.time.Instant/ofEpochMilli ts)})
        {:keys [entities relations]} analysis
        _ (ingest-storage/store-analysis! trimmed analysis)]
    {:in line
     :intent nil
     :entities (mapv (fn [{:entity/keys [label kind id]}]
                       {:name label :type kind :id id})
                     entities)
     :relations (mapv (fn [{:relation/keys [label src dst subject object confidence polarity]}]
                        {:type label
                         :src subject
                         :dst object
                         :confidence confidence
                         :polarity polarity
                         :src-id src
                         :dst-id dst})
                      relations)
     :tokens []
     :pos []
     :links []}))
