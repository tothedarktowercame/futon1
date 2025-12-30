(ns app.model-meta
  "Model descriptor + invariant checks for model/descriptor entities."
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [datascript.core :as d])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(def ^:private descriptor-name "model/descriptor/meta-model")
(def ^:private descriptor-type :model/descriptor)

(def ^:private default-descriptor
  {:model/scope :meta-model
   :schema/version "0.1.0"
   :client/schema-min "0.1.0"
   :entities
   {:model/descriptor {:required [:name :type :source :external-id]
                       :id-strategy :uuid}}
   :operations {}
   :stores
   {:xtdb {:role :canonical}
    :datascript {:role :cache}}
   :migrations {}
   :invariants
   [:meta-model/descriptor-source-present
    :meta-model/descriptor-has-schema
    :meta-model/descriptor-has-scope]})

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
                               {:name descriptor-name
                                :type descriptor-type
                                :external-id (:schema/version default-descriptor)
                                :source default-descriptor})))))

(defn upsert-descriptor!
  "Upsert the descriptor entity with the provided descriptor map."
  [conn env descriptor]
  (:source
   (store/ensure-entity! conn env
                         {:name descriptor-name
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
   :failures (vec failures)})

(defn- descriptor-docs [db]
  (->> (d/q '[:find ?id ?name ?source
              :in $ ?dtype
              :where
              [?e :entity/type ?dtype]
              [?e :entity/id ?id]
              [?e :entity/name ?name]
              [(get-else $ ?e :entity/source nil) ?source]]
            db descriptor-type)
       (map (fn [[id name source]]
              {:id id :name name :source source}))))

(defn- check-source-present [conn]
  (let [docs (descriptor-docs @conn)
        failures (->> docs
                      (filter (fn [{:keys [source]}] (nil? source)))
                      (mapv (fn [{:keys [id name]}]
                              [id name :missing-source])))]
    (invariant-result :meta-model/descriptor-source-present failures)))

(defn- check-schema-present [conn]
  (let [docs (descriptor-docs @conn)
        failures (->> docs
                      (filter (fn [{:keys [source]}]
                                (not (contains? (or source {}) :schema/version))))
                      (mapv (fn [{:keys [id name]}]
                              [id name :missing-schema])))]
    (invariant-result :meta-model/descriptor-has-schema failures)))

(defn- check-scope-present [conn]
  (let [docs (descriptor-docs @conn)
        failures (->> docs
                      (filter (fn [{:keys [source]}]
                                (not (contains? (or source {}) :model/scope))))
                      (mapv (fn [{:keys [id name]}]
                              [id name :missing-scope])))]
    (invariant-result :meta-model/descriptor-has-scope failures)))

(def ^:private invariant-handlers
  {:meta-model/descriptor-source-present check-source-present
   :meta-model/descriptor-has-schema check-schema-present
   :meta-model/descriptor-has-scope check-scope-present})

(defn verify
  [conn]
  (let [checks (mapv (fn [key]
                       (if-let [handler (get invariant-handlers key)]
                         (handler conn)
                         (invariant-result key [[:missing-handler key]])))
                     (:invariants default-descriptor))]
    {:ok? (every? :ok? checks)
     :results checks}))
