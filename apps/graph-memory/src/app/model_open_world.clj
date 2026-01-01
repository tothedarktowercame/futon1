(ns app.model-open-world
  "Model descriptor + invariant checks for open-world ingest storage."
  (:require [app.store :as store]
            [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(def ^:private descriptor-name "model/descriptor/open-world-ingest")
(def ^:private descriptor-type :model/descriptor)

(def ^:private default-entity-kinds
  #{:person :org :place :date :proper})

(def ^:private default-descriptor
  {:model/scope :open-world-ingest
   :schema/version "0.1.0"
   :schema/certificate {:penholder "code" :issued-at 0}
   :client/schema-min "0.1.0"
   :entities
   {:open-world/entity {:required [:entity/id :entity/label :entity/lower-label :entity/kind
                                   :entity/first-seen :entity/updated-at]
                        :id-strategy :custom}
    :open-world/mention {:required [:mention/id :mention/entity :mention/utterance :mention/text
                                    :mention/span :mention/sentence :mention/ts]
                         :id-strategy :custom}
    :open-world/relation {:required [:relation/id :relation/src :relation/dst :relation/label
                                     :relation/type :relation/subject :relation/object
                                     :relation/polarity :relation/confidence
                                     :relation/utterance :relation/sentence :relation/ts]
                          :id-strategy :custom}
    :open-world/utterance {:required [:utterance/id :utterance/text :utterance/ts
                                      :utterance/entity-count :utterance/relation-count]
                           :id-strategy :custom}
    :open-world/type {:required [:type/id :type/kind] :id-strategy :custom}}
   :entity-kinds {:known default-entity-kinds
                  :fallback :proper
                  :allow-custom? true}
   :operations
   {:open-world/ingest {:inputs [:text :actor] :outputs [:xtdb]}}
   :stores
   {:xtdb {:role :canonical}
    :datascript {:role :cache}}
   :migrations {}
   :invariants
   [:open-world/entity-required
    :open-world/entity-stub-detected
    :open-world/entity-kind-valid
    :open-world/mention-required
    :open-world/mention-links
    :open-world/mention-span-valid
   :open-world/relation-required
   :open-world/relation-links
   :open-world/relation-type-registered
    :open-world/utterance-required
    :open-world/type-required]})

(defn descriptor-template []
  default-descriptor)

(defn- normalize-for-hash [value]
  (cond
    (map? value)
    (into (sorted-map)
          (map (fn [[k v]] [k (normalize-for-hash v)]))
          value)

    (set? value)
    (vec (sort-by pr-str (map normalize-for-hash value)))

    (sequential? value)
    (mapv normalize-for-hash value)

    :else value))

(defn descriptor-hash [descriptor]
  (let [normalized (normalize-for-hash descriptor)
        payload (pr-str normalized)
        digest (MessageDigest/getInstance "SHA-256")
        bytes (.digest digest (.getBytes payload "UTF-8"))]
    (.encodeToString (Base64/getEncoder) bytes)))

(defn- descriptor-entity [conn]
  (store/fetch-entity conn {:name descriptor-name} {}))

(defn- parse-descriptor [source]
  (cond
    (map? source) source
    (string? source) (try (edn/read-string source) (catch Exception _ nil))
    :else nil))

(defn descriptor
  "Return the stored descriptor map, or nil when missing."
  [conn]
  (some-> (descriptor-entity conn) :source parse-descriptor))

(defn ensure-descriptor!
  "Insert the default descriptor when missing. Returns the descriptor."
  [conn env]
  (let [existing (descriptor conn)]
    (or existing
        (:source
         (store/ensure-entity! conn env
                               {:id descriptor-name
                                :name descriptor-name
                                :type descriptor-type
                                :external-id (:schema/version default-descriptor)
                                :source default-descriptor})))))

(defn upsert-descriptor!
  "Upsert the descriptor entity with the provided descriptor map."
  [conn env descriptor]
  (:source
   (store/ensure-entity! conn env
                         {:id descriptor-name
                          :name descriptor-name
                          :type descriptor-type
                          :external-id (:schema/version descriptor)
                          :source descriptor})))

(defn describe
  [conn]
  (when-let [desc (descriptor conn)]
    {:descriptor desc
     :hash (descriptor-hash desc)
     :schema/version (:schema/version desc)
     :client/schema-min (:client/schema-min desc)}))

(defn- invariant-result [key failures]
  {:key key
   :ok? (empty? failures)
   :failures failures})

(defn- missing? [value]
  (cond
    (nil? value) true
    (string? value) (str/blank? value)
    (sequential? value) (empty? value)
    :else false))

(defn- missing-fields [m fields]
  (->> fields (filter #(missing? (get m %))) vec))

(def ^:private entity-keys
  [:entity/id :entity/label :entity/lower-label :entity/kind :entity/first-seen :entity/updated-at])

(def ^:private mention-keys
  [:mention/id :mention/entity :mention/utterance :mention/text :mention/span :mention/sentence :mention/ts])

(def ^:private relation-keys
  [:relation/id :relation/src :relation/dst :relation/label :relation/type
   :relation/subject :relation/object :relation/polarity :relation/confidence
   :relation/utterance :relation/sentence :relation/ts])

(def ^:private utterance-keys
  [:utterance/id :utterance/text :utterance/ts :utterance/entity-count :utterance/relation-count])

(def ^:private type-keys
  [:type/id :type/kind])

(def ^:private stub-attrs
  [:entity/name
   :entity/type
   :entity/last-seen
   :entity/seen-count
   :entity/pinned?
   :entity/external-id
   :entity/source])

(defn- open-world-entity-ids []
  (let [mention-ids (->> (xt/q '{:find [?eid]
                                 :where [[?m :mention/text _]
                                         [?m :mention/entity ?eid]]})
                         (map first))
        relation-ids (->> (xt/q '{:find [?eid]
                                  :where [[?r :relation/label _]
                                          [?r :relation/src ?eid]]})
                          (map first))
        relation-ids' (->> (xt/q '{:find [?eid]
                                   :where [[?r :relation/label _]
                                           [?r :relation/dst ?eid]]})
                           (map first))
        labeled-ids (let [queries [{:attr :entity/label
                                    :query '{:find [?eid]
                                             :where [[?e :entity/id ?eid]
                                                     [?e :entity/label _]]}}
                                   {:attr :entity/lower-label
                                    :query '{:find [?eid]
                                             :where [[?e :entity/id ?eid]
                                                     [?e :entity/lower-label _]]}}
                                   {:attr :entity/kind
                                    :query '{:find [?eid]
                                             :where [[?e :entity/id ?eid]
                                                     [?e :entity/kind _]]}}
                                   {:attr :entity/first-seen
                                    :query '{:find [?eid]
                                             :where [[?e :entity/id ?eid]
                                                     [?e :entity/first-seen _]]}}
                                   {:attr :entity/updated-at
                                    :query '{:find [?eid]
                                             :where [[?e :entity/id ?eid]
                                                     [?e :entity/updated-at _]]}}]]
                      (->> queries
                           (mapcat (fn [{:keys [query]}]
                                     (map first (xt/q query))))
                           vec))]
    (->> (concat mention-ids relation-ids relation-ids' labeled-ids)
         (remove nil?)
         set)))

(defn- entity-entities []
  (let [ids (open-world-entity-ids)]
    (->> ids
         (map xt/entity)
         (remove nil?)
         (map #(select-keys % entity-keys)))))

(defn- mention-entities []
  (->> (xt/q '{:find [?id]
               :where [[?e :mention/text _]
                       [?e :mention/id ?id]]})
       (map first)
       (map xt/entity)
       (remove nil?)
       (map #(select-keys % mention-keys))))

(defn- relation-entities []
  (->> (xt/q '{:find [?id]
               :where [[?e :relation/label _]
                       [?e :relation/id ?id]]})
       (map first)
       (map xt/entity)
       (remove nil?)
       (map #(select-keys % relation-keys))))

(defn- utterance-entities []
  (->> (xt/q '{:find [?id]
               :where [[?e :utterance/entity-count _]
                       [?e :utterance/id ?id]]})
       (map first)
       (map xt/entity)
       (remove nil?)
       (map #(select-keys % utterance-keys))))

(defn- type-entities []
  (->> (xt/q '{:find [?id]
               :where [[?e :type/id ?id]]})
       (map first)
       (map xt/entity)
       (remove nil?)
       (map #(select-keys % type-keys))))

(defn- check-entity-required [conn]
  (let [failures (vec
                  (keep (fn [entity]
                          (when-let [missing (seq (missing-fields entity entity-keys))]
                            {:entity-id (:entity/id entity)
                             :label (:entity/label entity)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (entity-entities)))]
    (invariant-result :open-world/entity-required failures)))

(defn- check-entity-stubs [conn]
  (let [ids (open-world-entity-ids)
        failures (vec
                  (keep (fn [eid]
                          (when-let [doc (xt/entity eid)]
                            (let [present (some #(contains? doc %) stub-attrs)]
                              (when-not present
                                {:entity-id eid
                                 :issue :dangling-stub}))))
                        ids))]
    (invariant-result :open-world/entity-stub-detected failures)))

(defn- check-entity-kind-valid [conn]
  (let [desc (descriptor conn)
        known (set (or (get-in desc [:entity-kinds :known])
                       default-entity-kinds))
        allow-custom? (get-in desc [:entity-kinds :allow-custom?] true)
        failures (vec
                  (keep (fn [entity]
                          (let [kind (:entity/kind entity)]
                            (cond
                              (not (keyword? kind))
                              {:entity-id (:entity/id entity)
                               :label (:entity/label entity)
                               :issue :invalid-kind
                               :kind kind}
                              (or allow-custom? (contains? known kind))
                              nil
                              :else
                              {:entity-id (:entity/id entity)
                               :label (:entity/label entity)
                               :issue :unknown-kind
                               :kind kind})))
                        (entity-entities)))]
    (invariant-result :open-world/entity-kind-valid failures)))

(defn- check-mention-required [conn]
  (let [failures (vec
                  (keep (fn [mention]
                          (when-let [missing (seq (missing-fields mention mention-keys))]
                            {:mention-id (:mention/id mention)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (mention-entities)))]
    (invariant-result :open-world/mention-required failures)))

(defn- valid-span? [span]
  (and (vector? span)
       (= 2 (count span))
       (every? integer? span)
       (<= (first span) (second span))))

(defn- check-mention-span-valid [conn]
  (let [failures (vec
                  (keep (fn [mention]
                          (let [span (:mention/span mention)]
                            (when (and (some? span) (not (valid-span? span)))
                              {:mention-id (:mention/id mention)
                               :issue :invalid-span
                               :span span})))
                        (mention-entities)))]
    (invariant-result :open-world/mention-span-valid failures)))

(defn- check-mention-links [conn]
  (let [entity-ids (->> (xt/q '{:find [?id]
                                :where [[?e :entity/id ?id]]})
                         (map first)
                         set)
        utterance-ids (->> (xt/q '{:find [?id]
                                   :where [[?e :utterance/id ?id]]})
                           (map first)
                           set)
        failures (vec
                  (mapcat (fn [mention]
                            (let [entity-id (:mention/entity mention)
                                  utterance-id (:mention/utterance mention)]
                              (cond-> []
                                (and entity-id (not (contains? entity-ids entity-id)))
                                (conj {:mention-id (:mention/id mention)
                                       :issue :missing-entity
                                       :entity-id entity-id})
                                (and utterance-id (not (contains? utterance-ids utterance-id)))
                                (conj {:mention-id (:mention/id mention)
                                       :issue :missing-utterance
                                       :utterance-id utterance-id}))))
                          (mention-entities)))]
    (invariant-result :open-world/mention-links failures)))

(defn- check-relation-required [conn]
  (let [failures (vec
                  (keep (fn [rel]
                          (when-let [missing (seq (missing-fields rel relation-keys))]
                            {:relation-id (:relation/id rel)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (relation-entities)))]
    (invariant-result :open-world/relation-required failures)))

(defn- check-relation-links [conn]
  (let [entity-ids (->> (xt/q '{:find [?id]
                                :where [[?e :entity/id ?id]]})
                         (map first)
                         set)
        utterance-ids (->> (xt/q '{:find [?id]
                                   :where [[?e :utterance/id ?id]]})
                           (map first)
                           set)
        failures (vec
                  (mapcat (fn [rel]
                            (let [src (:relation/src rel)
                                  dst (:relation/dst rel)
                                  utterance-id (:relation/utterance rel)]
                              (cond-> []
                                (and src (not (contains? entity-ids src)))
                                (conj {:relation-id (:relation/id rel)
                                       :issue :missing-src
                                       :entity-id src})
                                (and dst (not (contains? entity-ids dst)))
                                (conj {:relation-id (:relation/id rel)
                                       :issue :missing-dst
                                       :entity-id dst})
                                (and utterance-id (not (contains? utterance-ids utterance-id)))
                                (conj {:relation-id (:relation/id rel)
                                       :issue :missing-utterance
                                       :utterance-id utterance-id}))))
                          (relation-entities)))]
    (invariant-result :open-world/relation-links failures)))

(defn- check-relation-type-registered [conn]
  (let [registered (->> (xt/q '{:find [?id]
                                :where [[?t :type/id ?id]
                                        [?t :type/kind :relation]]})
                         (map first)
                         set)
        failures (vec
                  (keep (fn [rel]
                          (let [rtype (:relation/type rel)]
                            (when (and (keyword? rtype)
                                       (not (contains? registered rtype)))
                              {:relation-id (:relation/id rel)
                               :issue :unregistered-type
                               :relation-type rtype})))
                        (relation-entities)))]
    (invariant-result :open-world/relation-type-registered failures)))

(defn- check-utterance-required [conn]
  (let [failures (vec
                  (keep (fn [utterance]
                          (when-let [missing (seq (missing-fields utterance utterance-keys))]
                            {:utterance-id (:utterance/id utterance)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (utterance-entities)))]
    (invariant-result :open-world/utterance-required failures)))

(defn- check-type-required [conn]
  (let [failures (vec
                  (keep (fn [t]
                          (when-let [missing (seq (missing-fields t type-keys))]
                            {:type-id (:type/id t)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (type-entities)))]
    (invariant-result :open-world/type-required failures)))

(def ^:private invariant-registry
  {:open-world/entity-required check-entity-required
   :open-world/entity-stub-detected check-entity-stubs
   :open-world/entity-kind-valid check-entity-kind-valid
   :open-world/mention-required check-mention-required
   :open-world/mention-links check-mention-links
   :open-world/mention-span-valid check-mention-span-valid
   :open-world/relation-required check-relation-required
   :open-world/relation-links check-relation-links
   :open-world/relation-type-registered check-relation-type-registered
   :open-world/utterance-required check-utterance-required
   :open-world/type-required check-type-required})

(defn verify
  "Run invariants listed in the descriptor. Returns {:ok? ... :results ...}."
  [conn]
  (if-let [desc (descriptor conn)]
    (let [keys (or (:invariants desc) [])
          results (vec (keep (fn [k]
                               (when-let [f (get invariant-registry k)]
                                 (f conn)))
                             keys))
          ok? (every? :ok? results)]
      {:ok? ok?
       :results results})
    {:ok? false
     :error :descriptor/missing}))
