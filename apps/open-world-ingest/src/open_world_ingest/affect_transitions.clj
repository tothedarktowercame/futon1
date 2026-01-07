(ns open-world-ingest.affect-transitions
  (:require [clojure.string :as str]
            [nlp-interface.intent :as intent]
            [open-world-ingest.util :as util]
            [xtdb.api :as xt])
  (:import (java.time Instant)))

(def ^:private affect-intents
  #{:activation
    :attraction
    :joy
    :fatigue
    :anxiety
    :withdrawal
    :frustration
    :sadness
    :numbness
    :orientation
    :social
    :regulation})

(def ^:private default-opts
  {:lookahead-minutes 10
   :lookahead-utterances 3
   :novelty-days 30
   :limit 200
   :max-transitions 80
   :max-terms 5})

(defn- valid-transition?
  [transition]
  (let [affect (get transition :affect_token)
        terms (get transition :capacity_tokens)]
    (and (map? affect)
         (seq terms))))

(defn- ->instant [value]
  (cond
    (instance? Instant value) value
    (number? value) (Instant/ofEpochMilli (long value))
    (string? value) (try
                      (Instant/parse (str/trim value))
                      (catch Exception _ nil))
    :else nil))

(defn- normalize-opts [opts]
  (let [opts (merge default-opts (or opts {}))
        since (->instant (:since opts))
        until (->instant (:until opts))]
    (assoc opts
           :since since
           :until until)))

(defn- base-utterance-query [limit actor-id]
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

(defn- fetch-utterances [db {:keys [since until limit actor-id]}]
  (let [query (base-utterance-query limit actor-id)
        rows (if actor-id
               (xt/q db query since until actor-id)
               (xt/q db query since until))]
    (->> rows
         (map (fn [[id text ts]]
                {:id id
                 :text text
                 :ts ts}))
         (sort-by :ts)
         vec)))

(defn- mentions-by-utterance [db utterance-ids]
  (if (empty? utterance-ids)
    {}
    (let [query {:find ["?utt" "?entity" "?label" "?kind"]
                 :in ["?utt-ids"]
                 :where [["?m" :mention/utterance "?utt"]
                         [(list 'contains? "?utt-ids" "?utt")]
                         ["?m" :mention/entity "?entity"]
                         ["?e" :entity/id "?entity"]
                         ["?e" :entity/label "?label"]
                         ["?e" :entity/kind "?kind"]]}
          rows (xt/q db query (set utterance-ids))]
      (reduce (fn [acc [utt-id entity-id label kind]]
                (update acc utt-id (fnil conj [])
                        {:entity-id entity-id
                         :label label
                         :kind kind}))
              {}
              rows))))

(defn- affect-intent [text]
  (let [{intent-type :type conf :conf} (intent/analyze text)]
    (when (contains? affect-intents intent-type)
      {:type intent-type
       :conf (double (or conf 0.0))})))

(defn- evidence-window
  [utterances idx {:keys [lookahead-minutes lookahead-utterances]}]
  (let [start (nth utterances idx)
        start-ts (:ts start)
        deadline (.plusSeconds start-ts (* 60 (long lookahead-minutes)))
        end-idx (min (count utterances) (+ idx (inc (long lookahead-utterances))))
        window (subvec utterances idx end-idx)]
    (->> window
         (take-while (fn [{:keys [ts]}]
                       (not (.isAfter ts deadline))))
         vec)))

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

(defn- novelty-window
  [ts novelty-days]
  (.minusSeconds ts (* 86400 (long novelty-days))))

(defn- new-term?
  [db actor-id entity-id ts novelty-days]
  (let [since (novelty-window ts novelty-days)]
    (not (entity-recently-mentioned? db actor-id entity-id since ts))))

(defn- build-term-index
  [mentions-by-id evidence]
  (reduce (fn [acc {:keys [id ts]}]
            (reduce (fn [inner {:keys [entity-id label kind]}]
                      (let [entry (get inner entity-id)
                            first-ts (or (:first-ts entry) ts)
                            first-utt (or (:first-utterance-id entry) id)
                            utterance-ids (conj (set (:utterance-ids entry)) id)]
                        (assoc inner entity-id
                               {:entity-id entity-id
                                :label label
                                :kind kind
                                :first-ts first-ts
                                :first-utterance-id first-utt
                                :utterance-ids utterance-ids})))
                    acc
                    (get mentions-by-id id [])))
          {}
          evidence))

(defn- proximity-score [start-ts end-ts lookahead-minutes]
  (let [window-seconds (max 1 (* 60 (long lookahead-minutes)))
        delta-seconds (max 0 (/ (- (.toEpochMilli end-ts)
                                   (.toEpochMilli start-ts))
                                1000.0))
        ratio (min 1.0 (/ (double delta-seconds) window-seconds))]
    (max 0.1 (- 1.0 ratio))))

(defn- transition->doc
  [affect terms {:keys [lookahead-minutes novelty-days]}]
  (let [start-ts (:ts affect)
        end-ts (->> terms (map :first-ts) (sort) last)
        proximities (mapv (fn [term]
                            (proximity-score start-ts (:first-ts term) lookahead-minutes))
                          terms)
        proximity (if (seq proximities)
                    (/ (reduce + proximities) (count proximities))
                    0.0)
        conf (:conf affect)
        score (-> (+ (* 0.6 conf) (* 0.4 proximity))
                  (max 0.0)
                  (min 1.0))
        transition-id (util/sha1 (str (:type affect)
                                      "|"
                                      (:id affect)
                                      "|"
                                      (str/join "," (sort (map :entity-id terms)))))]
    {:id transition-id
     :affect_token {:id (name (:type affect))
                    :label (name (:type affect))
                    :confidence conf}
     :capacity_tokens (mapv (fn [term]
                              {:id (str (:entity-id term))
                               :label (:label term)
                               :kind (some-> (:kind term) name)})
                            terms)
     :direction "affect->consequence"
     :time_bounds {:start (str start-ts)
                   :end (str end-ts)}
     :certificate {:affect_utterance_id (:id affect)
                   :term_utterance_ids (vec (sort (mapcat :utterance-ids terms)))
                   :term_mentions (mapv (fn [term]
                                          {:term (:label term)
                                           :utterance_ids (vec (sort (:utterance-ids term)))})
                                        terms)
                   :utterance_ids (vec (sort (conj (mapcat :utterance-ids terms) (:id affect))))
                   :rationale (str "Affect intent detected, followed by novel terms within "
                                   (long lookahead-minutes)
                                   " minutes (novelty window "
                                   (long novelty-days)
                                   " days).")}
     :link_strength {:score score
                     :method "affect-intent + novel terms in lookahead window"
                     :components {:intent_confidence conf
                                  :proximity proximity
                                  :novelty_days (long novelty-days)
                                  :term_count (count terms)}}}))

(defn affect-transitions
  "Return candidate affect transitions with evidence based on open-world utterances."
  [db opts]
  (let [{:keys [since until actor-id lookahead-minutes lookahead-utterances novelty-days
                limit max-transitions max-terms]} (normalize-opts opts)]
    (when-not (and since until)
      (throw (ex-info "since/until required" {:since since :until until})))
    (when-not actor-id
      (throw (ex-info "actor-id required" {:actor-id actor-id})))
    (let [utterances (fetch-utterances db {:since since
                                           :until until
                                           :limit limit
                                           :actor-id actor-id})
          utterance-ids (mapv :id utterances)
          mentions (mentions-by-utterance db utterance-ids)
          transitions (reduce
                       (fn [acc idx]
                         (if (>= (count acc) max-transitions)
                           (reduced acc)
                           (let [utt (nth utterances idx)
                                 affect (affect-intent (:text utt))]
                             (if-not affect
                               acc
                               (let [evidence (evidence-window utterances idx
                                                               {:lookahead-minutes lookahead-minutes
                                                                :lookahead-utterances lookahead-utterances})
                                     terms (build-term-index mentions evidence)
                                     filtered (->> terms
                                                   vals
                                                   (remove (fn [{:keys [entity-id]}]
                                                             (= entity-id actor-id)))
                                                   (filter (fn [{:keys [entity-id first-ts]}]
                                                             (new-term? db actor-id entity-id first-ts novelty-days)))
                                                   (sort-by :first-ts)
                                                   (take max-terms)
                                                   vec)
                                     doc (when (seq filtered)
                                           (transition->doc (assoc affect :id (:id utt) :ts (:ts utt))
                                                            filtered
                                                            {:lookahead-minutes lookahead-minutes
                                                             :novelty-days novelty-days}))]
                                 (if (and doc (valid-transition? doc))
                                   (conj acc doc)
                                   acc))))))
                       []
                       (range (count utterances)))]
      {:actor_id (if (keyword? actor-id)
                   (name actor-id)
                   (str actor-id))
       :window {:since (str since)
                :until (str until)}
       :params {:lookahead_minutes (long lookahead-minutes)
                :lookahead_utterances (long lookahead-utterances)
                :novelty_days (long novelty-days)
                :limit (long limit)
                :max_transitions (long max-transitions)
                :max_terms (long max-terms)}
       :counts {:utterances (count utterances)
                :transitions (count transitions)}
       :notes ["Affect transitions require an affect label plus a novel consequence term."
               "Consequences are terms used with or shortly after the affect utterance."
               "Novelty is based on lack of prior mentions in the configured window."]
       :transitions transitions})))
