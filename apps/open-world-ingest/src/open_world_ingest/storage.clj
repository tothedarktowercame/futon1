(ns open-world-ingest.storage
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [open-world-ingest.util :as util]
            [xtdb.api :as xt])
  (:import (java.util UUID)))

(defonce ^:private !node (atom nil))

(defn stop!
  []
  (when-let [node @!node]
    (.close ^AutoCloseable node)
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
  {:xt/id [:entity/id id]
   :entity/id id
   :entity/label label
   :entity/lower-label lower-label
   :entity/kind kind
   :entity/aliases (into (set (:entity/aliases existing)) alias-labels)
   :entity/first-seen (or (:entity/first-seen existing) now)
   :entity/updated-at now})

(defn- mention-docs
  [utterance-id now entity-id occurrences]
  (map (fn [{:entity/keys [label sentence] :mention/keys [span]}]
         (let [mention-id (str (UUID/randomUUID))]
           {:xt/id [:mention/id mention-id]
            :mention/id mention-id
            :mention/entity entity-id
            :mention/utterance utterance-id
            :mention/text label
            :mention/span span
            :mention/sentence sentence
            :mention/ts now}))
       occurrences))

(defn- relation-docs
  [utterance-id now relations]
  (map (fn [{:relation/keys [src dst label polarity confidence subject object sentence]}]
         (let [rid (util/sha1 (str src ":" label ":" dst ":" utterance-id))]
           {:xt/id [:relation/id rid]
            :relation/id rid
            :relation/src src
            :relation/dst dst
            :relation/label label
            :relation/subject subject
            :relation/object object
            :relation/polarity polarity
            :relation/confidence confidence
            :relation/utterance utterance-id
            :relation/sentence sentence
            :relation/ts now}))
       relations))

(defn store-analysis!
  [text {:keys [entities relations]}]
  (let [now (util/now)
        utterance-id (str (UUID/randomUUID))
        grouped (vals (group-by :entity/id entities))
        db (current-db)
        entity-results (map (fn [occurrences]
                              (let [entity (first occurrences)
                                    entity-id (:entity/id entity)
                                    existing (xt/entity db [:entity/id entity-id])
                                    labels (set (map :entity/label occurrences))
                                    doc (entity-doc existing now entity labels)]
                                {:doc doc
                                 :entity entity
                                 :id entity-id
                                 :new? (nil? existing)
                                 :occurrences occurrences}))
                            grouped)
        distinct-relations (distinct relations)
        relation-docs' (relation-docs utterance-id now distinct-relations)
        mention-docs' (mapcat (fn [{:keys [id occurrences]}]
                                (mention-docs utterance-id now id occurrences))
                              entity-results)
        utterance-doc {:xt/id [:utterance/id utterance-id]
                       :utterance/id utterance-id
                       :utterance/text text
                       :utterance/ts now
                       :utterance/entity-count (count grouped)
                       :utterance/relation-count (count distinct-relations)}
        ops (-> []
                (into (map (fn [{:keys [doc]}]
                             [::xt/put doc]) entity-results))
                (into (map (fn [doc]
                             [::xt/put doc]) mention-docs'))
                (into (map (fn [doc]
                             [::xt/put doc]) relation-docs'))
                (conj [::xt/put utterance-doc]))
        tx (xt/submit-tx (node) ops)]
    (xt/await-tx (node) tx)
    {:utterance/id utterance-id
     :entities (map (fn [{:keys [entity new?]}]
                      {:id (:entity/id entity)
                       :label (:entity/label entity)
                       :kind (:entity/kind entity)
                       :new? new?})
                    entity-results)
     :relations (map (fn [{:relation/keys [subject object label polarity confidence]}]
                       {:subject subject
                        :object object
                        :relation label
                        :polarity polarity
                        :confidence confidence})
                     distinct-relations)}))

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
        query {:find [?rel-id ?rel ?src-id ?src-label ?src-kind ?dst-id ?dst-label ?dst-kind ?pol ?ts]
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
        results (xt/q (current-db) query)]
    (map (fn [[rel-id rel src-id src-label src-kind dst-id dst-label dst-kind pol ts]]
           {:id rel-id
            :relation rel
            :src {:id src-id :label src-label :kind src-kind}
            :dst {:id dst-id :label dst-label :kind dst-kind}
            :polarity pol
            :ts ts})
         results))

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
