(ns open-world-ingest.storage
  (:require [charon.core :as charon]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [graph-memory.types-registry :as types]
            [open-world-ingest.affect-candidates :as affect-candidates]
            [open-world-ingest.util :as util]
            [xtdb.api :as xt])
  (:import (java.time Instant)
           (java.util UUID)))

(defonce ^:private !node (atom nil))

(defn stop!
  []
  (when-let [node @!node]
    (.close ^java.lang.AutoCloseable node)
    (reset! !node nil)))

(defn- ensure-dir! [path]
  (when path
    (let [file (io/file path)]
      (.mkdirs file)
      (.getAbsolutePath file))))

(defn- prepare-config
  [config {:keys [data-dir]}]
  (if data-dir
    (let [base (ensure-dir! data-dir)]
      (walk/postwalk
       (fn [value]
         (if (and (map? value) (:db-dir value))
           (assoc value :db-dir (ensure-dir! (io/file base (:db-dir value))))
           value))
       config))
    config))

(defn start-node!
  [{:keys [config-path data-dir]}]
  (stop!)
  (let [resource-path (or config-path (some-> (io/resource "xtdb.edn") io/file .getAbsolutePath))]
    (when-not resource-path
      (throw (ex-info "XTDB configuration not found" {})))
    (let [config (-> resource-path slurp edn/read-string (prepare-config {:data-dir data-dir}))
          node (xt/start-node config)]
      (reset! !node node)
      node)))

(defn node []
  (or @!node (throw (ex-info "XTDB node not started" {}))))

(defn current-db []
  (xt/db (node)))

(defn- entity-doc
  [existing now {:entity/keys [id label lower-label kind]} alias-labels]
  (let [doc {:xt/id id
             :entity/id id
             :entity/label label
             :entity/lower-label lower-label
             :entity/kind kind
             :entity/aliases (into (set (:entity/aliases existing)) alias-labels)
             :entity/first-seen (or (:entity/first-seen existing) now)
             :entity/updated-at now}]
    (merge existing doc)))

(def ^:private required-entity-keys
  [:entity/id :entity/label :entity/lower-label :entity/kind])

(def ^:private open-world-entity-keys
  [:entity/label :entity/lower-label :entity/kind :entity/first-seen :entity/updated-at])

(defn- missing-value? [value]
  (cond
    (nil? value) true
    (string? value) (str/blank? value)
    (sequential? value) (empty? value)
    :else false))

(defn- missing-entity-fields [entity]
  (->> required-entity-keys
       (filter #(missing-value? (get entity %)))
       vec))

(defn- missing-open-world-fields [entity]
  (->> open-world-entity-keys
       (filter #(missing-value? (get entity %)))
       vec))

(defn- normalize-label [value]
  (when-not (missing-value? value)
    (let [label (str/trim (str value))]
      (when-not (str/blank? label)
        label))))

(defn- ->instant [value]
  (cond
    (nil? value) nil
    (instance? Instant value) value
    (integer? value) (Instant/ofEpochMilli (long value))
    (number? value) (Instant/ofEpochMilli (long value))
    :else nil))

(defn- backfill-open-world-entity
  [doc now]
  (let [label (or (normalize-label (:entity/label doc))
                  (normalize-label (:entity/name doc))
                  (normalize-label (:entity/external-id doc)))
        lower-label (or (:entity/lower-label doc)
                        (some-> label str/lower-case))
        kind-raw (or (:entity/kind doc) (:entity/type doc))
        kind (when kind-raw (util/canonical-kind kind-raw))
        first-seen (or (->instant (:entity/first-seen doc))
                       (->instant (:entity/updated-at doc))
                       (->instant (:entity/last-seen doc))
                       now)
        updated-at (or (->instant (:entity/updated-at doc))
                       (->instant (:entity/last-seen doc))
                       now)
        updates (cond-> {}
                  (and (missing-value? (:entity/label doc)) label)
                  (assoc :entity/label label)

                  (and (missing-value? (:entity/lower-label doc)) lower-label)
                  (assoc :entity/lower-label lower-label)

                  (and (missing-value? (:entity/kind doc)) kind)
                  (assoc :entity/kind kind)

                  (missing-value? (:entity/first-seen doc))
                  (assoc :entity/first-seen first-seen)

                  (missing-value? (:entity/updated-at doc))
                  (assoc :entity/updated-at updated-at))
        doc' (merge doc updates)]
    {:doc doc'
     :updates updates
     :missing (missing-open-world-fields doc')}))

(defn- reject-missing-entities [entities]
  (let [failures (->> entities
                      (keep (fn [entity]
                              (when-let [missing (seq (missing-entity-fields entity))]
                                {:entity/id (:entity/id entity)
                                 :entity/label (:entity/label entity)
                                 :missing missing})))
                      vec)]
    (when (seq failures)
      (charon/reject :open-world/ingest
                     :open-world/missing-entity-fields
                     {:count (count failures)
                      :failures failures}
                     "Open-world entities must include id/label/lower-label/kind."))))

(defn- mention-docs
  [utterance-id now entity-id occurrences]
  (keep (fn [{:entity/keys [label sentence] :mention/keys [span]}]
          (when span
            (let [mention-id (str (UUID/randomUUID))]
              {:xt/id mention-id
               :mention/id mention-id
               :mention/entity entity-id
               :mention/utterance utterance-id
               :mention/text label
               :mention/span span
               :mention/sentence sentence
               :mention/ts now})))
        occurrences))

(defn- relation-parent
  [label]
  (when (keyword? label)
    (let [segments (->> (str/split (name label) #"-")
                        (remove str/blank?))]
      (when-let [segment (first segments)]
        (keyword "relation" (str segment "/*"))))))

(defn- register-relation-type!
  [{:relation/keys [label type-aliases]}]
  (when (keyword? label)
    (let [parent (relation-parent label)
          opts (cond-> {}
                 parent (assoc :parent parent))
          aliases (->> type-aliases
                       (remove nil?)
                       (remove #(= % label))
                       distinct
                       vec)]
      (types/ensure! :relation label opts)
      (when (seq aliases)
        (types/merge! :relation label aliases)))))

(defn- relation-docs
  [utterance-id now relations]
  (map (fn [{:relation/keys [src dst label polarity confidence subject object sentence time loc]}]
         (let [rid (util/sha1 (str src ":" label ":" dst ":" utterance-id))]
           (cond-> {:xt/id rid
                     :relation/id rid
                     :relation/src src
                     :relation/dst dst
                    :relation/label label
                    :relation/type label
                    :relation/subject subject
                    :relation/object object
                    :relation/polarity polarity
                    :relation/confidence confidence
                    :relation/utterance utterance-id
                    :relation/sentence sentence
                    :relation/ts now}
             time (assoc :relation/time time)
             loc (assoc :relation/loc loc))))
       relations))

(defn- canonical-ego-id
  [ego-id id]
  (let [resolved (or ego-id :me)]
    (if (= :open-world-ingest.nlp/ego id)
      resolved
      id)))

(defn- canonical-ego-doc
  [ego-id doc]
  (-> doc
      (update :xt/id #(canonical-ego-id ego-id %))
      (update :entity/id #(canonical-ego-id ego-id %))
      (update :relation/src #(canonical-ego-id ego-id %))
      (update :relation/dst #(canonical-ego-id ego-id %))))

(defn- store-analysis*
  [node text {:keys [entities relations ego-id actor-name actor-id actor-type]}]
  (let [now (util/now)
        utterance-id (str (UUID/randomUUID))
        actor-name (some-> actor-name str str/trim not-empty)
        invalid-entities (reject-missing-entities entities)
        grouped (vals (group-by :entity/id entities))
        db (xt/db node)
        canonical-id (fn [entity-id] (canonical-ego-id ego-id entity-id))
        entity-results (map (fn [occurrences]
                              (let [entity (first occurrences)
                                    entity-id (:entity/id entity)
                                    existing (xt/entity db (canonical-id entity-id))
                                    labels (set (map :entity/label occurrences))
                                    doc (entity-doc existing now entity labels)]
                                {:doc doc
                                 :entity entity
                                 :id entity-id
                                 :new? (nil? existing)
                                 :occurrences occurrences}))
                            grouped)
        distinct-relations (distinct relations)
        entity-ids (set (map (comp canonical-id :id) entity-results))
        backfill-docs (atom {})
        relation-dropped (atom [])
        ensure-open-world (fn [eid]
                            (if (contains? entity-ids eid)
                              {:ok? true}
                              (when-let [existing (xt/entity db eid)]
                                (let [{:keys [doc updates missing]} (backfill-open-world-entity existing now)]
                                  (when (seq updates)
                                    (swap! backfill-docs assoc eid doc))
                                  {:ok? (empty? missing)
                                   :missing missing}))))
        valid-relations (->> distinct-relations
                             (filter (fn [{:relation/keys [src dst] :as rel}]
                                       (let [src-id (canonical-ego-id ego-id src)
                                             dst-id (canonical-ego-id ego-id dst)
                                             src-state (ensure-open-world src-id)
                                             dst-state (ensure-open-world dst-id)
                                             src-ok (:ok? src-state)
                                             dst-ok (:ok? dst-state)]
                                         (when-not (and src-ok dst-ok)
                                           (swap! relation-dropped conj
                                                  {:relation rel
                                                   :missing (cond-> []
                                                              (not src-ok) (conj {:issue :missing-src
                                                                                 :entity-id src-id
                                                                                 :missing (:missing src-state)})
                                                              (not dst-ok) (conj {:issue :missing-dst
                                                                                 :entity-id dst-id
                                                                                 :missing (:missing dst-state)}))}))
                                         (and src-ok dst-ok))))
                             vec)
        _ (run! register-relation-type! distinct-relations)]
    (cond
      invalid-entities invalid-entities
      (seq @relation-dropped)
      (charon/reject :open-world/ingest
                     :open-world/missing-relation-endpoints
                     {:errors (vec @relation-dropped)
                      :entities entities
                      :relations distinct-relations}
                     "Ensure entities exist for relation endpoints before ingest.")
      :else
      (let [relation-docs' (relation-docs utterance-id now valid-relations)
            mention-docs' (mapcat (fn [{:keys [id occurrences]}]
                                    (mention-docs utterance-id now id occurrences))
                                  entity-results)
            utterance-doc (cond-> {:xt/id utterance-id
                                   :utterance/id utterance-id
                                   :utterance/text text
                                   :utterance/ts now
                                   :utterance/entity-count (count grouped)
                                   :utterance/relation-count (count distinct-relations)}
                            actor-id (assoc :utterance/actor-id actor-id)
                            actor-name (assoc :utterance/actor-name actor-name)
                            actor-type (assoc :utterance/actor-type actor-type))
            ops (-> []
                    (into (map (fn [{:keys [doc]}]
                                 [::xt/put (canonical-ego-doc ego-id doc)]) entity-results))
                    (into (map (fn [[_ doc]]
                                 [::xt/put (canonical-ego-doc ego-id doc)])
                               (seq @backfill-docs)))
                    (into (map (fn [doc]
                                 [::xt/put (canonical-ego-doc ego-id doc)]) mention-docs'))
                    (into (map (fn [doc]
                                 [::xt/put (canonical-ego-doc ego-id doc)]) relation-docs'))
                    (conj [::xt/put utterance-doc]))
            tx (xt/submit-tx node ops)]
        (xt/await-tx node tx)
        (affect-candidates/record-utterance-async!
         {:node node
          :text text
          :ts now
          :utterance-id utterance-id
          :actor-id (or actor-id ego-id)
          :entities entities})
        (charon/ok
         :open-world/ingest
         {:utterance/id utterance-id
          :entities (map (fn [{:keys [entity new?]}]
                           {:id (:entity/id entity)
                            :label (:entity/label entity)
                            :kind (:entity/kind entity)
                            :new? new?})
                         entity-results)
          :relations (map (fn [{:relation/keys [subject object label polarity confidence time loc]}]
                            (cond-> {:subject subject
                                     :object object
                                     :relation label
                                     :polarity polarity
                                     :confidence confidence}
                              time (assoc :time time)
                              loc (assoc :loc loc)))
                          distinct-relations)})))))

(defn store-analysis!
  [text {:keys [entities relations] :as analysis}]
  (store-analysis* (node) text analysis))

(defn store-analysis-with-node!
  "Store open-world analysis using an existing XTDB node.
   Optional :ego-id, :actor-id, :actor-name, :actor-type annotate the utterance."
  [node text {:keys [entities relations ego-id] :as analysis}]
  (store-analysis* node text analysis))

(defn entity-by-name
  [name]
  (let [needle (-> name str/trim str/lower-case)]
    (when (seq needle)
      (when-let [result (first (xt/q (current-db)
                                      '{:find [?id ?label ?kind]
                                        :in [?needle]
                                        :where [[?e :entity/lower-label ?needle]
                                                [?e :entity/id ?id]
                                                [?e :entity/label ?label]
                                                [?e :entity/kind ?kind]]}
                                      needle))]
        (let [[id label kind] result]
          {:id id :label label :kind kind})))))

(defn recent-relations
  [limit]
  (let [limit (long (or limit 5))
        base-query '{:find [?rel-id ?rel ?src-id ?src-label ?src-kind ?dst-id ?dst-label ?dst-kind ?pol ?ts]
                      :where [[?r :relation/id ?rel-id]
                              [?r :relation/label ?rel]
                              [?r :relation/src ?src-id]
                              [?r :relation/dst ?dst-id]
                              [?r :relation/polarity ?pol]
                              [?r :relation/ts ?ts]
                              [?src :entity/id ?src-id]
                              [?src :entity/label ?src-label]
                              [?src :entity/kind ?src-kind]
                              [?dst :entity/id ?dst-id]
                              [?dst :entity/label ?dst-label]
                              [?dst :entity/kind ?dst-kind]]
                      :order-by [[?ts :desc]]
                      :limit limit}
        query (assoc base-query :limit limit)
        results (xt/q (current-db) query)]
    (map (fn [[rel-id rel src-id src-label src-kind dst-id dst-label dst-kind pol ts]]
           {:id rel-id
            :relation rel
            :src {:id src-id :label src-label :kind src-kind}
            :dst {:id dst-id :label dst-label :kind dst-kind}
            :polarity pol
            :ts ts})
         results)))

(defn ego-neighbors
  [entity-id]
  (let [db (current-db)
        outgoing (xt/q db '{:find [?rel ?dst-id ?dst-label ?dst-kind ?pol ?ts]
                             :in [?entity]
                             :where [[?r :relation/src ?entity]
                                     [?r :relation/dst ?dst-id]
                                     [?r :relation/label ?rel]
                                     [?r :relation/polarity ?pol]
                                     [?r :relation/ts ?ts]
                                     [?dst :entity/id ?dst-id]
                                     [?dst :entity/label ?dst-label]
                                     [?dst :entity/kind ?dst-kind]]
                             :order-by [[?ts :desc]]}
                           entity-id)
        incoming (xt/q db '{:find [?rel ?src-id ?src-label ?src-kind ?pol ?ts]
                             :in [?entity]
                             :where [[?r :relation/dst ?entity]
                                     [?r :relation/src ?src-id]
                                     [?r :relation/label ?rel]
                                     [?r :relation/polarity ?pol]
                                     [?r :relation/ts ?ts]
                                     [?src :entity/id ?src-id]
                                     [?src :entity/label ?src-label]
                                     [?src :entity/kind ?src-kind]]
                             :order-by [[?ts :desc]]}
                           entity-id)]
    {:outgoing (map (fn [[rel dst-id dst-label dst-kind pol ts]]
                      {:relation rel
                       :direction :out
                       :entity {:id dst-id :label dst-label :kind dst-kind}
                       :polarity pol
                       :ts ts})
                    outgoing)
     :incoming (map (fn [[rel src-id src-label src-kind pol ts]]
                      {:relation rel
                       :direction :in
                       :entity {:id src-id :label src-label :kind src-kind}
                       :polarity pol
                       :ts ts})
                    incoming)}))

(defn cooccurring-entities
  [entity-id]
  (let [results (xt/q (current-db)
                       '{:find [?other-id ?other-label ?other-kind (count ?m2)]
                         :in [?entity]
                         :where [[?m1 :mention/entity ?entity]
                                 [?m1 :mention/utterance ?utt]
                                 [?m2 :mention/utterance ?utt]
                                 [?m2 :mention/entity ?other-id]
                                 [?other :entity/id ?other-id]
                                 [?other :entity/label ?other-label]
                                 [?other :entity/kind ?other-kind]
                                 [(not= ?other-id ?entity)]]}
                       entity-id)]
    (->> results
         (map (fn [[id label kind count]]
                {:id id :label label :kind kind :count count}))
         (sort-by :count >))))
