(ns app.model
  "Model descriptor + invariant checks for patterns."
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(def ^:private descriptor-name "model/descriptor/patterns")
(def ^:private descriptor-type :model/descriptor)

(def ^:private relation-type :arxana/scholium)
(def ^:private catalog-name "pattern-language/catalog")
(def ^:private language-includes ":pattern-language/includes")
(def ^:private pattern-includes ":pattern/includes")
(def ^:private futon3-source-token "/futon3/library/")

(def ^:private default-descriptor
  {:model/scope :patterns
   :schema/version "0.1.0"
   :schema/certificate {:penholder "code" :issued-at 0}
   :client/schema-min "0.1.0"
   :entities
   {:pattern/language {:required [:name] :id-strategy :uuid}
   :pattern/library {:required [:name] :id-strategy :uuid}
   :pattern/component {:required [:name] :id-strategy :uuid}
   :pattern/language-source {:required [:name] :id-strategy :uuid}
   :pattern/language-status {:required [:name] :id-strategy :uuid}
    :pattern/language-catalog {:required [:name] :id-strategy :uuid}
    :sigil {:required [:name] :id-strategy :uuid}}
   :operations
   {:pattern/sync {:inputs [:filesystem] :outputs [:xtdb]}}
   :stores
   {:xtdb {:role :canonical}
    :datascript {:role :cache}
    :filesystem {:role :working-copy}}
   :migrations {}
   :invariants
   [:patterns/language-has-source
    :patterns/language-has-status
    :patterns/language-in-catalog
    :patterns/language-has-includes
    :patterns/pattern-core-components]})

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

(defn- languages-without-relation [db dst-type]
  (d/q '[:find ?lang-id ?lang-name
         :in $ ?dst-type
         :where
         [?lang :entity/id ?lang-id]
         [?lang :entity/type :pattern/language]
         [?lang :entity/name ?lang-name]
         (not-join [?lang]
                   [?r :relation/src ?lang]
                   [?r :relation/dst ?dst]
                   [?dst :entity/type ?dst-type])]
       db dst-type))

(defn- relations-with-note [db note]
  (let [pattern '[:relation/id :relation/type :relation/provenance
                  {:relation/src [:entity/id :entity/name :entity/type :entity/external-id]}
                  {:relation/dst [:entity/id :entity/name :entity/type :entity/external-id]}]]
    (->> (d/q '[:find (pull ?r pattern)
                :in $ pattern ?rel-type
                :where
                [?r :relation/type ?rel-type]]
              db pattern relation-type)
         (map first)
         (filter (fn [doc]
                   (= note (get-in doc [:relation/provenance :note])))))))

(defn- relations-by-types [db src-type dst-type]
  (let [pattern '[:relation/id :relation/type :relation/provenance
                  {:relation/src [:entity/id :entity/name :entity/type :entity/external-id]}
                  {:relation/dst [:entity/id :entity/name :entity/type :entity/external-id]}]]
    (->> (d/q '[:find (pull ?r pattern)
                :in $ pattern ?rel-type ?src-type ?dst-type
                :where
                [?r :relation/type ?rel-type]
                [?r :relation/src ?src]
                [?r :relation/dst ?dst]
                [?src :entity/type ?src-type]
                [?dst :entity/type ?dst-type]]
              db pattern relation-type src-type dst-type)
         (map first))))

(defn- entities-by-type [db etype]
  (d/q '[:find ?id ?name ?source
         :in $ ?etype
         :where
         [?e :entity/type ?etype]
         [?e :entity/id ?id]
         [?e :entity/name ?name]
         [(get-else $ ?e :entity/source "") ?source]]
       db etype))

(defn- scoped-languages [db]
  (->> (entities-by-type db :pattern/language)
       (filter (fn [[_ _ source]]
                 (and source (str/includes? source futon3-source-token))))
       (mapv (fn [[id name _]] [id name]))))

(defn- scoped-pattern-ids [db]
  (let [language-ids (set (map first (scoped-languages db)))
        relations (relations-by-types db :pattern/language :pattern/library)]
    (->> relations
         (filter (fn [doc]
                   (contains? language-ids (get-in doc [:relation/src :entity/id]))))
         (map #(get-in % [:relation/dst :entity/id]))
         set)))

(defn- languages-missing-catalog [db]
  (when-let [catalog (ffirst (d/q '[:find ?e
                                   :where [?e :entity/name "pattern-language/catalog"]]
                                 db))]
    (d/q '[:find ?lang-id ?lang-name
           :in $ ?catalog
           :where
           [?lang :entity/type :pattern/language]
           [?lang :entity/id ?lang-id]
           [?lang :entity/name ?lang-name]
           (not-join [?lang ?catalog]
                     [?r :relation/src ?catalog]
                     [?r :relation/dst ?lang])]
         db catalog)))

(defn- invariant-result [key failures]
  {:key key
   :ok? (empty? failures)
   :failures (vec failures)})

(defn- check-language-has-source [conn]
  (let [db @conn
        scoped (set (map first (scoped-languages db)))
        missing (->> (languages-without-relation db :pattern/language-source)
                     (filter (fn [[lang-id _]] (contains? scoped lang-id)))
                     vec)]
    (invariant-result :patterns/language-has-source missing)))

(defn- check-language-has-status [conn]
  (let [db @conn
        scoped (set (map first (scoped-languages db)))
        missing (->> (languages-without-relation db :pattern/language-status)
                     (filter (fn [[lang-id _]] (contains? scoped lang-id)))
                     vec)]
    (invariant-result :patterns/language-has-status missing)))

(defn- check-language-in-catalog [conn]
  (let [db @conn
        scoped (set (map first (scoped-languages db)))
        missing (->> (or (languages-missing-catalog db) [])
                     (filter (fn [[lang-id _]] (contains? scoped lang-id)))
                     vec)]
    (invariant-result :patterns/language-in-catalog missing)))

(defn- check-language-has-includes [conn]
  (let [db @conn
        languages (scoped-languages db)
        relations (relations-by-types db :pattern/language :pattern/library)
        has-include? (->> relations
                          (map #(get-in % [:relation/src :entity/id]))
                          set)
        missing (->> languages
                     (remove (fn [[lang-id _]]
                               (contains? has-include? lang-id)))
                     vec)]
    (invariant-result :patterns/language-has-includes missing)))

(def ^:private core-components
  ["context" "if" "however" "then" "because" "next-steps"])

(def ^:private core-aliases
  {"next-step" "next-steps"})

(def ^:private summary-components
  #{"conclusion" "instantiated-by"})

(defn- normalize-label [value]
  (when value
    (let [raw (-> (str value)
                  str/lower-case
                  str/trim)
          stripped (-> raw
                       (str/replace #"^[^a-z0-9]+" "")
                       (str/replace #"[^a-z0-9]+" "-")
                       (str/replace #"^-+|-+$" ""))]
      (get core-aliases stripped stripped))))

(defn- component-order [relation]
  (let [order (get-in relation [:relation/provenance :order])]
    (cond
      (number? order) (long order)
      (string? order) (try (Long/parseLong order) (catch Exception _ nil))
      :else nil)))

(defn- name-order [name]
  (when-let [[_ digits] (re-find #"/(\\d{2,})-" (str name))]
    (try (Long/parseLong digits) (catch Exception _ nil))))

(defn- component-label [component]
  (or (normalize-label (get component :entity/external-id))
      (normalize-label (second (re-find #"/\\d{2,}-([^/]+)$" (str (:entity/name component)))))
      (normalize-label (last (str/split (str (:entity/name component)) #"/")))))

(defn- check-pattern-core-components [conn]
  (let [db @conn
        scoped-ids (scoped-pattern-ids db)
        patterns (->> (entities-by-type db :pattern/library)
                      (filter (fn [[pattern-id _ _]]
                                (contains? scoped-ids pattern-id))))
        relations (relations-by-types db :pattern/library :pattern/component)
        grouped (group-by #(get-in % [:relation/src :entity/id]) relations)
        failures (vec
                  (mapcat
                   (fn [[pattern-id pattern-name _]]
                     (let [rels (get grouped pattern-id)
                           components (map (fn [rel]
                                             (let [dst (:relation/dst rel)
                                                   label (component-label dst)
                                                   order (or (component-order rel)
                                                             (name-order (:entity/name dst)))]
                                               {:label label
                                                :order order}))
                                           rels)
                           present (frequencies (keep :label components))
                           missing (remove #(contains? present %) core-components)
                           duplicates (->> present
                                           (filter (fn [[_ cnt]] (> cnt 1)))
                                           (map first))
                           summary-present (->> present
                                                (map first)
                                                (filter summary-components)
                                                set)
                           ordered (->> components
                                        (filter #(contains? (set core-components) (:label %)))
                                        (sort-by :order)
                                        (map :label))
                           order-missing? (some #(nil? (:order %)) components)
                           order-ok? (= ordered core-components)]
                       (cond-> []
                         (empty? summary-present)
                         (conj {:pattern-id pattern-id
                                :pattern-name pattern-name
                                :issue :missing-summary
                                :expected ["conclusion" "instantiated-by"]})
                         (> (count summary-present) 1)
                         (conj {:pattern-id pattern-id
                                :pattern-name pattern-name
                                :issue :multiple-summary
                                :present (vec summary-present)})
                         (seq missing)
                         (conj {:pattern-id pattern-id
                                :pattern-name pattern-name
                                :issue :missing-core
                                :missing (vec missing)})
                         (seq duplicates)
                         (conj {:pattern-id pattern-id
                                :pattern-name pattern-name
                                :issue :duplicate-core
                                :duplicates (vec duplicates)})
                         (and (not (seq missing)) order-missing?)
                         (conj {:pattern-id pattern-id
                                :pattern-name pattern-name
                                :issue :core-order-missing})
                         (and (not (seq missing)) (not order-missing?) (not order-ok?))
                         (conj {:pattern-id pattern-id
                                :pattern-name pattern-name
                                :issue :core-order-invalid
                                :observed (vec ordered)}))))
                   patterns))]
    (invariant-result :patterns/pattern-core-components failures)))

(def ^:private invariant-registry
  {:patterns/language-has-source check-language-has-source
   :patterns/language-has-status check-language-has-status
   :patterns/language-in-catalog check-language-in-catalog
   :patterns/language-has-includes check-language-has-includes
   :patterns/pattern-core-components check-pattern-core-components})

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
