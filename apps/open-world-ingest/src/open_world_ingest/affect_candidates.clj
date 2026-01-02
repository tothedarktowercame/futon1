(ns open-world-ingest.affect-candidates
  (:require [nlp-interface.intent :as intent]
            [xtdb.api :as xt])
  (:import (java.time Instant)))

(def ^:private affect-intents
  #{:positive-affect
    :anxiety
    :sadness
    :affection
    :aggression
    :passivity})

(def ^:private default-config
  {:lookahead-minutes 10
   :novelty-days 30
   :max-pending 200})

(defonce ^:private !config (atom default-config))
(defonce ^:private !pending (atom {})) ;; actor-id -> vector of pending items

(defn configure!
  [opts]
  (swap! !config merge (or opts {})))

(defn pending-state
  []
  @!pending)

(defn- ->instant [value]
  (cond
    (instance? Instant value) value
    (number? value) (Instant/ofEpochMilli (long value))
    (string? value) (try
                      (Instant/parse value)
                      (catch Exception _ nil))
    :else nil))

(defn- affect-intent [text]
  (let [{:keys [type conf]} (intent/analyze text)]
    (when (contains? affect-intents type)
      {:type type
       :conf (double (or conf 0.0))})))

(defn- novelty-window [ts novelty-days]
  (.minusSeconds ts (* 86400 (long novelty-days))))

(defn- entity-recently-mentioned?
  [db actor-id entity-id since until]
  (let [query {:find ["?ts"]
               :in ["?entity" "?actor" "?since" "?until"]
               :where [["?m" :mention/entity "?entity"]
                       ["?m" :mention/utterance "?utt"]
                       ["?u" :utterance/id "?utt"]
                       ["?u" :utterance/actor-id "?actor"]
                       ["?u" :utterance/ts "?ts"]
                       [(list '>= "?ts" "?since")]
                       [(list '< "?ts" "?until")]]
               :limit 1}]
    (boolean (seq (xt/q db query entity-id actor-id since until)))))

(defn- new-term?
  [db actor-id entity-id ts novelty-days]
  (let [since (novelty-window ts novelty-days)]
    (not (entity-recently-mentioned? db actor-id entity-id since ts))))

(defn- prune-expired
  [pending ts]
  (filterv (fn [{:keys [expires-at]}]
             (and expires-at (not (.isBefore expires-at ts))))
           pending))

(defn- add-pending
  [pending {:keys [affect utterance-id ts]}]
  (let [{:keys [lookahead-minutes max-pending]} @!config
        expires-at (.plusSeconds ts (* 60 (long lookahead-minutes)))
        item {:affect affect
              :utterance-id utterance-id
              :ts ts
              :expires-at expires-at
              :capacity-tokens {}}]
    (let [items (conj (vec pending) item)
          keep-from (max 0 (- (count items) (long max-pending)))]
      (subvec items keep-from))))

(defn- update-capacity
  [pending terms ts]
  (mapv (fn [item]
          (if (.isBefore (:expires-at item) ts)
            item
            (reduce (fn [acc {:keys [entity-id label kind utterance-id]}]
                      (update acc :capacity-tokens
                              (fn [tokens]
                                (let [entry (get tokens entity-id)
                                      first-ts (or (:first-ts entry) ts)
                                      utterance-ids (conj (set (:utterance-ids entry)) utterance-id)]
                                  (assoc tokens entity-id
                                         {:entity-id entity-id
                                          :label label
                                          :kind kind
                                          :first-ts first-ts
                                          :utterance-ids utterance-ids})))))
                    item
                    terms)))
        pending))

(defn record-utterance!
  [{:keys [node text ts utterance-id actor-id entities]}]
  (when (and node actor-id)
    (let [db (xt/db node)
          ts (or (->instant ts) (Instant/now))
          affect (affect-intent text)
          {:keys [novelty-days]} @!config
          clean-entities (remove (fn [{:entity/keys [id]}]
                                   (or (= id actor-id)
                                       (= id :open-world-ingest.nlp/ego)))
                                 (or entities []))
          terms (->> clean-entities
                     (map (fn [{:entity/keys [id label kind]}]
                            {:entity-id id
                             :label label
                             :kind kind
                             :utterance-id utterance-id}))
                     (filter (fn [{:keys [entity-id]}]
                               (new-term? db actor-id entity-id ts novelty-days)))
                     vec)]
      (swap! !pending
             (fn [state]
               (let [current (get state actor-id [])
                     current (prune-expired current ts)
                     current (if affect
                               (add-pending current {:affect affect
                                                     :utterance-id utterance-id
                                                     :ts ts})
                               current)
                     current (if (seq terms)
                               (update-capacity current terms ts)
                               current)]
                 (assoc state actor-id current)))))))

(defn record-utterance-async!
  [payload]
  (future
    (try
      (record-utterance! payload)
      (catch Throwable _ nil))))
