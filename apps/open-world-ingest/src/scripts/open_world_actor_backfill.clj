(ns scripts.open-world-actor-backfill
  "Backfill open-world utterances with actor metadata and optionally re-ingest DS utterances."
  (:require [app.config :as config]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [open-world-ingest.nlp :as nlp]
            [open-world-ingest.storage :as ow]
            [xtdb.api :as xta]))

(defn- usage []
  (str "Usage: clojure -M -m scripts.open-world-actor-backfill "
       "[--actor-name NAME] [--profile PROFILE] [--ingest-ds]\n"))

(defn- parse-args [args]
  (loop [remaining args
         opts {:actor-name "Joe Corneli"
               :profile nil
               :ingest-ds? false}]
    (if (empty? remaining)
      opts
      (let [[flag value & rest] remaining]
        (case flag
          "--actor-name" (recur rest (assoc opts :actor-name value))
          "--profile" (recur rest (assoc opts :profile value))
          "--ingest-ds" (recur rest (assoc opts :ingest-ds? true))
          (do
            (println "Unknown option:" flag)
            (println (usage))
            (System/exit 1)))))))

(defn- ensure-xt-node! []
  (when-not (xt/started?)
    (let [cfg (config/config)
          cfg-path (:xtdb/config-path cfg "apps/graph-memory/resources/xtdb.edn")
          data-dir (:app/data-dir cfg)]
      (xt/start! cfg-path {:data-dir (when data-dir (str (io/file data-dir "xtdb")))
                           :xt/created-by "open-world-actor-backfill"}))))

(defn- ensure-actor! [profile name]
  (let [conn (store-manager/conn profile)
        env (store-manager/env profile)
        trimmed (some-> name str str/trim)]
    (when-not (seq trimmed)
      (throw (ex-info "actor-name required" {:name name})))
    (store/ensure-entity! conn env {:name trimmed :type :person :pinned? true})))

(defn- open-world-utterances
  [db]
  (->> (xta/q db '{:find [?id]
                  :where [[?u :utterance/id ?id]
                          [?u :utterance/text _]]})
       (map first)
       (remove nil?)))

(defn- update-utterances!
  [node actor]
  (let [db (xta/db node)
        ids (open-world-utterances db)
        actor-id (:entity/id actor)
        actor-name (:entity/name actor)
        actor-type (:entity/type actor)]
    (when (seq ids)
      (let [docs (->> ids
                      (map (fn [id]
                             (when-let [doc (xt/entity id)]
                               (cond-> doc
                                 actor-id (assoc :utterance/actor-id actor-id)
                                 actor-name (assoc :utterance/actor-name actor-name)
                                 actor-type (assoc :utterance/actor-type actor-type)))))
                      (remove nil?)
                      (map (fn [doc] [::xta/put doc]))
                      vec)]
        (when (seq docs)
          (let [tx (xta/submit-tx node docs)]
            (xta/await-tx node tx)))))
    {:updated (count ids)}))

(defn- ds-utterances
  [conn]
  (when conn
    (->> (d/q '[:find ?text ?ts
                :where
                [?u :utterance/text ?text]
                [?u :utterance/ts ?ts]]
              (d/db conn))
         (map (fn [[text ts]] {:text text :ts ts})))))

(defn- ingest-ds-utterances!
  [node actor utterances]
  (let [actor-id (:entity/id actor)
        actor-name (:entity/name actor)
        actor-type (:entity/type actor)]
    (doseq [{:keys [text ts]} utterances
            :when (seq (str text))]
      (let [analysis (nlp/analyze text {:now (java.time.Instant/ofEpochMilli (long ts))})]
        (ow/store-analysis-with-node!
         node
         text
         (assoc analysis
                :ego-id actor-id
                :actor-id actor-id
                :actor-name actor-name
                :actor-type actor-type))))))

(defn -main [& args]
  (let [{:keys [actor-name profile ingest-ds?]} (parse-args args)
        profile (or profile (store-manager/default-profile))
        _ (ensure-xt-node!)
        node (xt/node)
        actor (ensure-actor! profile actor-name)
        _ (println "Using actor" (:entity/name actor) "id" (:entity/id actor))
        backfill-result (update-utterances! node actor)]
    (println "Updated open-world utterances:" (:updated backfill-result))
    (when ingest-ds?
      (let [conn (store-manager/conn profile)
            utts (ds-utterances conn)]
        (println "Re-ingesting DS utterances:" (count utts))
        (ingest-ds-utterances! node actor utts)))))
