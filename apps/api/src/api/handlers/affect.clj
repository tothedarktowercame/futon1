(ns api.handlers.affect
  (:require [api.util.http :as http]
            [clojure.string :as str]
            [nlp-interface.intent :as intent]
            [open-world-ingest.affect-transitions :as affect-transitions]
            [xtdb.api :as xt])
  (:import (java.time Instant)))

(def ^:private affect-intents
  {:positive-affect "positive-affect"
   :anxiety "anxiety"
   :sadness "sadness"
   :affection "affection"
   :aggression "aggression"
   :passivity "passivity"})

(defn- parse-long-param [value]
  (when (some? value)
    (try
      (Long/parseLong (str/trim (str value)))
      (catch Exception _ nil))))

(defn- parse-instant [value]
  (when (some? value)
    (let [raw (str/trim (str value))]
      (when (seq raw)
        (if-let [ms (parse-long-param raw)]
          (Instant/ofEpochMilli ms)
          (try
            (Instant/parse raw)
            (catch Exception _ nil)))))))

(defn- parse-actor-ids [value]
  (when-let [raw (some-> value str str/trim not-empty)]
    (if (str/starts-with? raw ":")
      [(keyword (subs raw 1))]
      [(keyword raw) raw])))

(defn- window-range
  [now-ms query]
  (let [lookback (or (parse-long-param (get query "lookback_hours")) 24)
        now-inst (Instant/ofEpochMilli (long now-ms))
        since (or (parse-instant (get query "since"))
                  (.minusSeconds now-inst (* 3600 (max 1 lookback))))
        until (or (parse-instant (get query "until"))
                  now-inst)]
    {:since since
     :until until
     :lookback-hours lookback}))

(defn- base-query
  [limit actor-id]
  (let [with-actor? (some? actor-id)
        where (cond-> [["?u" :utterance/id "?id"]
                       ["?u" :utterance/text "?text"]
                       ["?u" :utterance/ts "?ts"]
                       [(list '<= "?since" "?ts")]
                       [(list '< "?ts" "?until")]]
                with-actor? (conj ["?u" :utterance/actor-id "?actor"]))
        in (cond-> ["?since" "?until"]
             with-actor? (conj "?actor"))]
    (cond-> {:find ["?id" "?text" "?ts"]
             :in in
             :where where
             :order-by [["?ts" :desc]]}
      limit (assoc :limit limit))))

(defn- fetch-utterances
  [db {:keys [since until]} limit actor-id]
  (let [query (base-query limit actor-id)]
    (if actor-id
      (xt/q db query since until actor-id)
      (xt/q db query since until))))

(defn- summarize-affect
  [utterances]
  (let [seed (into {}
                   (map (fn [[intent label]]
                          [intent {:id intent :label label :count 0 :conf-sum 0.0}])
                        affect-intents))
        totals (reduce (fn [acc [_ text _]]
                         (let [{:keys [type conf]} (intent/analyze text)
                               entry (get acc type)]
                           (if entry
                             (-> acc
                                 (update-in [type :count] inc)
                                 (update-in [type :conf-sum] + (double (or conf 0.0))))
                             acc)))
                       seed
                       utterances)
        utterance-count (count utterances)
        affect-count (reduce + 0 (map (comp :count val) totals))
        signals (->> totals
                     (map (fn [[_ {:keys [id label count conf-sum]}]]
                            (let [avg-conf (when (pos? count)
                                             (/ conf-sum count))]
                              {:id (name id)
                               :label label
                               :count count
                               :share (if (pos? utterance-count)
                                        (/ count utterance-count)
                                        0.0)
                               :avg_conf (or avg-conf 0.0)})))
                     (sort-by :id)
                     vec)]
    {:utterance_count utterance-count
     :affect_count affect-count
     :signals signals}))

(defn affect-labels-handler
  [request]
  (let [ctx (:ctx request)
        db (:xt/db ctx)]
    (if (nil? db)
      (http/ok-json {:error "XTDB not available"} 503)
      (let [query (:query-params request)
            actor-ids (parse-actor-ids (or (get query "actor_id")
                                           (get query "actor")))
            limit (parse-long-param (get query "limit"))
            {:keys [since until lookback-hours]} (window-range (:now ctx) query)
            primary-id (first actor-ids)
            fallback-id (second actor-ids)
            primary-results (fetch-utterances db {:since since :until until} limit primary-id)
            fallback-results (when (and fallback-id (empty? primary-results))
                               (fetch-utterances db {:since since :until until} limit fallback-id))
            utterances (or fallback-results primary-results)
            actor-id (cond
                       (seq fallback-results) fallback-id
                       (seq primary-results) primary-id
                       :else primary-id)
            summary (summarize-affect utterances)]
        (http/ok-json (merge
                       {:telemetry "affect-labels"
                        :experimental true
                        :window {:since (str since)
                                 :until (str until)
                                 :lookback_hours lookback-hours}
                        :actor_id (when actor-id
                                    (if (keyword? actor-id)
                                      (name actor-id)
                                      (str actor-id)))
                        :notes ["Dictionary-based label telemetry; not affect transitions."]}
                       summary))))))

(defn affect-deprecated-handler
  [_request]
  (http/ok-json {:error "Endpoint deprecated"
                 :message "Use /api/alpha/affect-labels for label telemetry or /api/alpha/affect-transitions for transitions."
                 :deprecated "/api/alpha/affect"
                 :replacement "/api/alpha/affect-labels"}
                410))

(defn affect-transitions-handler
  [request]
  (let [ctx (:ctx request)
        db (:xt/db ctx)]
    (if (nil? db)
      (http/ok-json {:error "XTDB not available"} 503)
      (let [query (:query-params request)
            actor-ids (parse-actor-ids (or (get query "actor_id")
                                           (get query "actor")))
            actor-id (first actor-ids)
            fallback-id (second actor-ids)
            {:keys [since until lookback-hours]} (window-range (:now ctx) query)
            lookahead-minutes (parse-long-param (get query "lookahead_minutes"))
            lookahead-utterances (parse-long-param (get query "lookahead_utterances"))
            novelty-days (parse-long-param (get query "novelty_days"))
            limit (parse-long-param (get query "limit"))
            max-transitions (parse-long-param (get query "max_transitions"))
            max-terms (parse-long-param (get query "max_terms"))
            base-opts (cond-> {:since since
                               :until until
                               :lookback_hours lookback-hours}
                        lookahead-minutes (assoc :lookahead-minutes lookahead-minutes)
                        lookahead-utterances (assoc :lookahead-utterances lookahead-utterances)
                        novelty-days (assoc :novelty-days novelty-days)
                        limit (assoc :limit limit)
                        max-transitions (assoc :max-transitions max-transitions)
                        max-terms (assoc :max-terms max-terms))]
        (if-not actor-id
          (http/ok-json {:error "actor_id required"} 400)
          (let [primary (affect-transitions/affect-transitions db (assoc base-opts :actor-id actor-id))
                fallback (when (and fallback-id (zero? (get-in primary [:counts :utterances] 0)))
                           (affect-transitions/affect-transitions db (assoc base-opts :actor-id fallback-id)))
                payload (or fallback primary)]
            (http/ok-json payload)))))))
