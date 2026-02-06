(ns app.model-media
  "Model descriptor + invariant checks for media storage."
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(def ^:private descriptor-name "model/descriptor/media")
(def ^:private descriptor-type :model/descriptor)

(def ^:private default-descriptor
  {:model/scope :media
   :schema/version "0.1.2"
   :schema/certificate {:penholder "code" :issued-at 0}
   :client/schema-min "0.1.2"
   :entities
   {:arxana/media-track {:required [:entity/name :entity/external-id]
                         :id-strategy :custom}
    :arxana/media-lyrics {:required [:entity/name :entity/external-id :entity/source :media/sha256]
                          :id-strategy :custom}}
   :operations
   {:media/ingest {:inputs [:filesystem :editor] :outputs [:xtdb]}}
   :stores
   {:xtdb {:role :canonical}
    :datascript {:role :cache}
    :filesystem {:role :working-copy}}
   :migrations {}
   :invariants
   [:media/track-required
    :media/source-content
    :media/lyrics-required
    :media/lyrics-source-content
    :media/lyrics-linked
    :media/lyrics-link-types]})

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

(declare upsert-descriptor!)

(defn ensure-descriptor!
  "Insert the default descriptor when missing. Returns the descriptor."
  [conn env]
  (let [existing (descriptor conn)]
    (if (or (nil? existing)
            (not= (:schema/version existing) (:schema/version default-descriptor)))
      (upsert-descriptor! conn env default-descriptor)
      existing)))

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

(defn- placeholder-source? [value]
  (and (string? value)
       (= "external" (str/trim value))))

(defn- missing-fields [m fields]
  (->> fields (filter #(missing? (get m %))) vec))

(def ^:private track-keys
  [:entity/id :entity/name :entity/external-id :entity/source :entity/type])

(def ^:private lyrics-keys
  [:entity/id :entity/name :entity/external-id :entity/source :media/sha256 :entity/type])

(def ^:private relation-keys
  [:relation/id :relation/type
   {:relation/src [:entity/id :entity/type :entity/name]}
   {:relation/dst [:entity/id :entity/type :entity/name]}])

(defn- track-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-track]]
                      db)
                  (map first))]
    (map #(d/pull db track-keys %) ids)))

(defn- lyrics-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db lyrics-keys %) ids)))

(defn- lyrics-relations [db]
  (let [ids (->> (d/q '[:find ?r
                        :where
                        [?r :relation/type :media/lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db relation-keys %) ids)))

(defn- check-track-required [conn]
  (let [db @conn
        failures (vec
                  (keep (fn [track]
                          (when-let [missing (seq (missing-fields track (remove #{:entity/id :entity/type} track-keys)))]
                            {:id (:entity/id track)
                             :name (:entity/name track)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (track-entities db)))]
    (invariant-result :media/track-required failures)))

(defn- check-lyrics-required [conn]
  (let [db @conn
        failures (vec
                  (keep (fn [lyrics]
                          (when-let [missing (seq (missing-fields lyrics (remove #{:entity/id :entity/type} lyrics-keys)))]
                            {:id (:entity/id lyrics)
                             :name (:entity/name lyrics)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (lyrics-entities db)))]
    (invariant-result :media/lyrics-required failures)))

(defn- check-lyrics-source-content [conn]
  (let [db @conn
        failures (vec
                  (keep (fn [lyrics]
                          (when (placeholder-source? (:entity/source lyrics))
                            {:id (:entity/id lyrics)
                             :name (:entity/name lyrics)
                             :issue :placeholder-source}))
                        (lyrics-entities db)))]
    (invariant-result :media/lyrics-source-content failures)))

(defn- check-media-source-content [conn]
  (let [db @conn
        failures (vec
                  (concat
                   (keep (fn [track]
                           (when (placeholder-source? (:entity/source track))
                             {:id (:entity/id track)
                              :name (:entity/name track)
                              :type :arxana/media-track
                              :issue :placeholder-source}))
                         (track-entities db))
                   (keep (fn [lyrics]
                           (when (placeholder-source? (:entity/source lyrics))
                             {:id (:entity/id lyrics)
                              :name (:entity/name lyrics)
                              :type :arxana/media-lyrics
                              :issue :placeholder-source}))
                         (lyrics-entities db))))]
    (invariant-result :media/source-content failures)))

(defn- check-lyrics-linked [conn]
  (let [db @conn
        relations (lyrics-relations db)
        linked-ids (->> relations
                        (map #(get-in % [:relation/dst :entity/id]))
                        (remove nil?)
                        set)
        failures (vec
                  (keep (fn [lyrics]
                          (when-not (contains? linked-ids (:entity/id lyrics))
                            {:id (:entity/id lyrics)
                             :name (:entity/name lyrics)
                             :issue :missing-lyrics-relation}))
                        (lyrics-entities db)))]
    (invariant-result :media/lyrics-linked failures)))

(defn- check-lyrics-link-types [conn]
  (let [db @conn
        failures (vec
                  (keep (fn [rel]
                          (let [src (get-in rel [:relation/src :entity/type])
                                dst (get-in rel [:relation/dst :entity/type])]
                            (when (or (not= src :arxana/media-track)
                                      (not= dst :arxana/media-lyrics))
                              {:id (:relation/id rel)
                               :issue :unexpected-endpoints
                               :src-type src
                               :dst-type dst
                               :src-name (get-in rel [:relation/src :entity/name])
                               :dst-name (get-in rel [:relation/dst :entity/name])})))
                        (lyrics-relations db)))]
    (invariant-result :media/lyrics-link-types failures)))

(def ^:private invariant-registry
  {:media/track-required check-track-required
   :media/source-content check-media-source-content
   :media/lyrics-required check-lyrics-required
   :media/lyrics-source-content check-lyrics-source-content
   :media/lyrics-linked check-lyrics-linked
   :media/lyrics-link-types check-lyrics-link-types})

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
