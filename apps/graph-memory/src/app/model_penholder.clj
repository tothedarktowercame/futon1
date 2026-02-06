;; apps/graph-memory/src/app/model_penholder.clj
(ns app.model-penholder
  "Model descriptor + invariant checks for penholder registry entries."
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(def ^:private descriptor-name "model/descriptor/penholder")
(def ^:private descriptor-type :model/descriptor)

(def ^:private default-descriptor
  {:model/scope :penholder-registry
   :schema/version "0.1.0"
   :schema/certificate {:penholder "code" :issued-at 0}
   :client/schema-min "0.1.0"
   :entities
   {:model/penholder {:required [:entity/name :entity/type :entity/source]
                      :id-strategy :uuid}}
   :operations {}
   :stores
   {:xtdb {:role :canonical}
    :datascript {:role :cache}}
   :migrations {}
   :invariants
   [:penholder/entry-required
    :penholder/entry-schema]})

(def ^:private default-descriptors
  ["model/descriptor/patterns"
   "model/descriptor/media"
   "model/descriptor/meta-model"
   "model/descriptor/open-world-ingest"
   "model/descriptor/docbook"
   "model/descriptor/penholder"])

(defn- normalize-penholder-name [value]
  (when-let [raw (cond
                   (keyword? value) (name value)
                   (string? value) value
                   :else nil)]
    (let [clean (str/trim raw)]
      (when (seq clean)
        (str/lower-case clean)))))

(defn- env-user []
  (normalize-penholder-name (or (System/getenv "USER")
                                (System/getenv "LOGNAME"))))

(def ^:private default-penholders
  (->> ["api"
        "api:post:/api/entity"
        "api:post:/api/relation"
        "cli"
        (env-user)]
       (remove nil?)
       vec))

(defn- default-penholder []
  (or (System/getenv "MODEL_PENHOLDER")
      (System/getenv "BASIC_CHAT_PENHOLDER")
      (env-user)
      "cli"))

(defn- existing-entries [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :model/penholder]]
                      db)
                 (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/source] %) ids)))

(defn- entry-by-descriptor [entries]
  (reduce (fn [acc entry]
            (let [source (:entity/source entry)
                  descriptor (:descriptor source)]
              (if descriptor
                (assoc acc descriptor entry)
                acc)))
          {}
          entries))

(defn- penholder-entry [descriptor penholders strict? certificate]
  {:name (str "penholder/" descriptor)
   :type :model/penholder
   :source {:descriptor descriptor
            :penholders penholders
            :certificate certificate
            :strict? strict?}})

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

(defn ensure-registry!
  "Ensure baseline penholder registry entries exist. Returns a map of entries by descriptor."
  [conn env]
  (ensure-descriptor! conn env)
  (let [now (System/currentTimeMillis)
        holder (or (:penholder env) (default-penholder))
        certificate {:penholder holder
                     :issued-at now}
        entries (existing-entries @conn)
        by-descriptor (entry-by-descriptor entries)]
    (doseq [descriptor default-descriptors]
      (let [existing (get by-descriptor descriptor)
            source (:entity/source existing)
            prior (->> (:penholders source)
                       (map normalize-penholder-name)
                       (remove nil?)
                       vec)
            penholders (vec (distinct (concat prior default-penholders)))]
        (store/ensure-entity! conn env
                              (penholder-entry descriptor penholders true certificate))))
    (entry-by-descriptor (existing-entries @conn))))

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

(def ^:private entry-keys
  [:entity/id :entity/name :entity/source :entity/type])

(defn- parse-source [source]
  (cond
    (map? source) source
    (string? source) (try (edn/read-string source) (catch Exception _ nil))
    :else nil))

(defn- normalize-descriptor [value]
  (cond
    (keyword? value) (let [ns (namespace value)
                           nm (name value)]
                       (if ns
                         (str ns "/" nm)
                         nm))
    (string? value) (some-> value str/trim not-empty)
    :else nil))

(defn- normalize-penholder [value]
  (when-let [raw (cond
                   (keyword? value) (name value)
                   (string? value) value
                   :else nil)]
    (let [clean (str/trim raw)]
      (when (seq clean)
        (str/lower-case clean)))))

(defn- normalize-certificate [value]
  (cond
    (map? value) (when (seq value) value)
    (string? value) (let [clean (str/trim value)]
                      (when (seq clean) clean))
    :else nil))

(defn- entry-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :model/penholder]]
                      db)
                  (map first))]
    (map #(d/pull db entry-keys %) ids)))

(defn- check-entry-required [conn]
  (let [db @conn
        failures (vec
                  (keep (fn [entry]
                          (when-let [missing (seq (missing-fields entry (remove #{:entity/id :entity/type} entry-keys)))]
                            {:id (:entity/id entry)
                             :name (:entity/name entry)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (entry-entities db)))]
    (invariant-result :penholder/entry-required failures)))

(defn- check-entry-schema [conn]
  (let [db @conn
        failures (vec
                  (keep (fn [entry]
                          (let [source (parse-source (:entity/source entry))
                                descriptor (normalize-descriptor (:descriptor source))
                                holders (->> (:penholders source)
                                             (map normalize-penholder)
                                             (remove nil?)
                                             vec)
                                certificate (normalize-certificate (:certificate source))]
                            (when (or (nil? source)
                                      (nil? descriptor)
                                      (empty? holders)
                                      (nil? certificate))
                              {:id (:entity/id entry)
                               :name (:entity/name entry)
                               :issue :invalid-source
                               :descriptor descriptor
                               :penholders holders
                               :certificate (boolean certificate)})))
                        (entry-entities db)))]
    (invariant-result :penholder/entry-schema failures)))

(def ^:private invariant-registry
  {:penholder/entry-required check-entry-required
   :penholder/entry-schema check-entry-schema})

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
