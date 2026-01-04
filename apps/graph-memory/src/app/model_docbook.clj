(ns app.model-docbook
  "Model descriptor + invariant checks for docbook storage."
  (:require [app.store :as store]
            [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:import (java.security MessageDigest)
           (java.util Base64)))

(def ^:private descriptor-name "model/descriptor/docbook")
(def ^:private descriptor-type :model/descriptor)

(def ^:private default-descriptor
  {:model/scope :docbook
   :schema/version "0.1.0"
   :schema/certificate {:penholder "code" :issued-at 0}
   :client/schema-min "0.1.0"
   :entities
   {:docbook/heading {:required [:doc/id :doc/book :doc/title :doc/outline_path :doc/path_string :doc/level]
                      :id-strategy :custom}
    :docbook/entry {:required [:doc/id :doc/entry-id :doc/book]
                    :id-strategy :custom}
    :docbook/toc {:required [:doc/book :doc/toc-order]
                  :id-strategy :custom}}
   :operations
   {:docbook/ingest {:inputs [:filesystem] :outputs [:xtdb]}}
   :stores
   {:xtdb {:role :canonical}
    :datascript {:role :cache}
    :filesystem {:role :working-copy}}
   :migrations {}
   :invariants
   [:docbook/heading-required
    :docbook/entry-required
    :docbook/heading-has-entry
    :docbook/entry-body-required
    :docbook/entry-has-heading
    :docbook/entry-book-matches-heading
    :docbook/toc-covers-headings
    :docbook/toc-known-headings]})

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

(def ^:private heading-keys
  [:doc/id :doc/book :doc/title :doc/outline_path :doc/path_string :doc/level])

(def ^:private entry-keys
  [:doc/id :doc/entry-id :doc/book :doc/status :doc/body])

(defn- heading-entities [db]
  (let [ids (->> (xt/q db '{:find [?e]
                           :where [[?e :doc/id _]
                                   (not [?e :doc/entry-id _])
                                   (not [?e :doc/toc-order _])]})
                  (map first))]
    (->> ids
         (map #(some-> (xtdb/entity db %) (select-keys heading-keys)))
         (remove nil?))))

(defn- entry-entities [db]
  (let [ids (->> (xt/q db '{:find [?e]
                           :where [[?e :doc/entry-id _]]})
                  (map first))]
    (->> ids
         (map #(some-> (xtdb/entity db %) (select-keys entry-keys)))
         (remove nil?))))

(defn- toc-entities [db]
  (let [ids (->> (xt/q db '{:find [?e]
                           :where [[?e :doc/toc-order _]]})
                  (map first))]
    (->> ids
         (map #(some-> (xtdb/entity db %) (select-keys [:doc/book :doc/toc-order])))
         (remove nil?))))

(defn- headings-by-book [db]
  (->> (heading-entities db)
       (filter :doc/book)
       (group-by :doc/book)))

(defn- heading-book-map [db]
  (->> (heading-entities db)
       (filter (fn [h] (and (:doc/id h) (:doc/book h))))
       (reduce (fn [acc h]
                 (assoc acc (:doc/id h) (:doc/book h)))
               {})))

(defn- check-heading-required [db]
  (let [failures (vec
                  (keep (fn [heading]
                          (when-let [missing (seq (missing-fields heading heading-keys))]
                            {:doc-id (:doc/id heading)
                             :book (:doc/book heading)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (heading-entities db)))]
    (invariant-result :docbook/heading-required failures)))

(defn- check-entry-required [db]
  (let [failures (vec
                  (keep (fn [entry]
                          (when-let [missing (seq (missing-fields entry (remove #{:doc/status :doc/body} entry-keys)))]
                            {:doc-id (:doc/id entry)
                             :entry-id (:doc/entry-id entry)
                             :book (:doc/book entry)
                             :issue :missing-fields
                             :missing (vec missing)}))
                        (entry-entities db)))]
    (invariant-result :docbook/entry-required failures)))

(defn- check-heading-has-entry [db]
  (let [headings (heading-entities db)
        entries (entry-entities db)
        entries-by-doc (group-by :doc/id entries)
        failures (vec
                  (keep (fn [heading]
                          (when (empty? (get entries-by-doc (:doc/id heading)))
                            {:doc-id (:doc/id heading)
                             :book (:doc/book heading)
                             :issue :missing-entry}))
                        headings))]
    (invariant-result :docbook/heading-has-entry failures)))

(defn- check-entry-body-required [db]
  (let [failures (vec
                  (keep (fn [entry]
                          (let [status (:doc/status entry)
                                body (:doc/body entry)
                                placeholder? (and (string? body)
                                                  (str/includes?
                                                   (str/lower-case body)
                                                   "no summary yet"))]
                            (cond
                              (and (not= :removed status)
                                   (missing? body))
                              {:doc-id (:doc/id entry)
                               :entry-id (:doc/entry-id entry)
                               :book (:doc/book entry)
                               :status status
                               :issue :missing-body}

                              (and (not= :removed status) placeholder?)
                              {:doc-id (:doc/id entry)
                               :entry-id (:doc/entry-id entry)
                               :book (:doc/book entry)
                               :status status
                               :issue :placeholder-body})))
                        (entry-entities db)))]
    (invariant-result :docbook/entry-body-required failures)))

(defn- check-entry-has-heading [db]
  (let [heading-ids (->> (heading-entities db)
                         (keep :doc/id)
                         set)
        failures (vec
                  (keep (fn [entry]
                          (when-not (contains? heading-ids (:doc/id entry))
                            {:doc-id (:doc/id entry)
                             :entry-id (:doc/entry-id entry)
                             :book (:doc/book entry)
                             :issue :missing-heading}))
                        (entry-entities db)))]
    (invariant-result :docbook/entry-has-heading failures)))

(defn- check-entry-book-matches-heading [db]
  (let [heading-books (heading-book-map db)
        failures (vec
                  (keep (fn [entry]
                          (let [doc-id (:doc/id entry)
                                heading-book (get heading-books doc-id)]
                            (when (and heading-book
                                       (:doc/book entry)
                                       (not= heading-book (:doc/book entry)))
                              {:doc-id doc-id
                               :entry-id (:doc/entry-id entry)
                               :book (:doc/book entry)
                               :heading-book heading-book
                               :issue :book-mismatch})))
                        (entry-entities db)))]
    (invariant-result :docbook/entry-book-matches-heading failures)))

(defn- check-toc-covers-headings [db]
  (let [headings (headings-by-book db)
        failures (vec
                  (mapcat
                   (fn [{:doc/keys [book toc-order]}]
                     (let [heading-ids (->> (get headings book)
                                            (keep :doc/id)
                                            set)
                           missing (remove (set toc-order) heading-ids)]
                       (when (seq missing)
                         [{:book book
                           :issue :toc-missing-headings
                           :missing (vec missing)}])))
                   (toc-entities db)))]
    (invariant-result :docbook/toc-covers-headings failures)))

(defn- check-toc-known-headings [db]
  (let [headings (headings-by-book db)
        failures (vec
                  (mapcat
                   (fn [{:doc/keys [book toc-order]}]
                     (let [heading-ids (->> (get headings book)
                                            (keep :doc/id)
                                            set)
                           unknown (remove heading-ids toc-order)]
                       (when (seq unknown)
                         [{:book book
                           :issue :toc-unknown-headings
                           :unknown (vec unknown)}])))
                   (toc-entities db)))]
    (invariant-result :docbook/toc-known-headings failures)))

(def ^:private invariant-registry
  {:docbook/heading-required check-heading-required
   :docbook/entry-required check-entry-required
   :docbook/heading-has-entry check-heading-has-entry
   :docbook/entry-body-required check-entry-body-required
   :docbook/entry-has-heading check-entry-has-heading
   :docbook/entry-book-matches-heading check-entry-book-matches-heading
   :docbook/toc-covers-headings check-toc-covers-headings
   :docbook/toc-known-headings check-toc-known-headings})

(defn verify
  "Run invariants listed in the descriptor. Returns {:ok? ... :results ...}."
  [conn]
  (if-let [desc (descriptor conn)]
    (if-not (xt/started?)
      {:ok? false
       :error :xtdb/unavailable}
      (let [db (xt/db)
            keys (or (:invariants desc) [])
            results (vec (keep (fn [k]
                                 (when-let [f (get invariant-registry k)]
                                   (f db)))
                               keys))
            ok? (every? :ok? results)]
        {:ok? ok?
         :results results}))
    {:ok? false
     :error :descriptor/missing}))
