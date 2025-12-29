(ns app.patterns
  "Pattern registry queries for fast client diffs."
  (:require [app.xt :as xt]
            [datascript.core :as d]))

(def ^:private pattern-entity-types
  #{:pattern/language
    :pattern/library
    :pattern/component
    :pattern/language-source
    :pattern/language-status
    :pattern/language-catalog})

(def ^:private relation-type :arxana/scholium)
(def ^:private catalog-name "pattern-language/catalog")

(defn- type->string [value]
  (cond
    (keyword? value) (subs (str value) 1)
    (string? value) value
    :else (str value)))

(defn- infer-label [src dst]
  (let [src-name (:entity/name src)
        dst-name (:entity/name dst)
        src-type (:entity/type src)
        dst-type (:entity/type dst)]
    (cond
      (= catalog-name src-name) ":language/catalog"
      (and dst-name (clojure.string/starts-with? dst-name "pattern-language/source/")) ":language/source"
      (and dst-name (clojure.string/starts-with? dst-name "pattern-language/status/")) ":language/status"
      (and (= src-type :pattern/language) (= dst-type :pattern/library)) ":pattern-language/includes"
      (and (= src-type :pattern/library) (= dst-type :pattern/component)) ":pattern/includes"
      :else nil)))

(defn- query-entities [db types]
  (->> (d/q '[:find (pull ?e [:entity/id :entity/name :entity/type :entity/external-id])
              :in $ ?types
              :where
              [?e :entity/id _]
              [?e :entity/type ?type]
              [(contains? ?types ?type)]]
            db types)
       (map first)
       (map (fn [doc]
              {:id (:entity/id doc)
               :name (:entity/name doc)
               :type (type->string (:entity/type doc))
               :external-id (:entity/external-id doc)}))
       (sort-by :name)
       vec))

(defn- query-relations [db types]
  (let [pattern '[:relation/id :relation/type :relation/provenance
                  {:relation/src [:entity/id :entity/name :entity/type]}
                  {:relation/dst [:entity/id :entity/name :entity/type]}]]
    (->> (d/q '[:find (pull ?r pattern)
                :in $ pattern ?types
                :where
                [?r :relation/type _]
                [?r :relation/src ?src]
                [?r :relation/dst ?dst]
                [?src :entity/type ?src-type]
                [?dst :entity/type ?dst-type]
                [(contains? ?types ?src-type)]
                [(contains? ?types ?dst-type)]]
              db pattern types)
         (map first)
         (map (fn [doc]
                (let [prov (:relation/provenance doc)
                      note (when (map? prov) (:note prov))
                      src (:relation/src doc)
                      dst (:relation/dst doc)]
                  {:type (type->string (:relation/type doc))
                   :label (or note (infer-label src dst))
                   :provenance prov
                   :src-id (:entity/id src)
                   :src-name (:entity/name src)
                   :dst-id (:entity/id dst)
                   :dst-name (:entity/name dst)})))
         vec)))

(defn- query-xt-entities [db types]
  (->> (xt/q db {:find '[(pull ?e [:entity/id :entity/name :entity/type :entity/external-id])]
                 :in ['?types]
                 :where '[[?e :entity/id _]
                          [?e :entity/type ?type]
                          [(contains? ?types ?type)]]}
             types)
       (map first)
       (map (fn [doc]
              {:id (:entity/id doc)
               :name (:entity/name doc)
               :type (type->string (:entity/type doc))
               :external-id (:entity/external-id doc)}))
       (sort-by :name)
       vec))

(defn- registry-from-datascript [conn include-relations?]
  (let [db @conn
        entities (query-entities db pattern-entity-types)
        relations (when include-relations?
                    (query-relations db pattern-entity-types))]
    {:generated-at (System/currentTimeMillis)
     :source "datascript"
     :entity-types (->> pattern-entity-types (map type->string) sort vec)
     :relation-type (type->string relation-type)
     :entities entities
     :relations (vec (or relations []))
     :counts {:entities (count entities)
              :relations (count relations)}}))

(defn- registry-from-xtdb [conn include-relations?]
  (let [db (xt/db)
        entities (query-xt-entities db pattern-entity-types)
        relations (when include-relations?
                    (query-relations @conn pattern-entity-types))]
    {:generated-at (System/currentTimeMillis)
     :source "xtdb"
     :entity-types (->> pattern-entity-types (map type->string) sort vec)
     :relation-type (type->string relation-type)
     :entities entities
     :relations (vec (or relations []))
     :counts {:entities (count entities)
              :relations (count relations)}}))

(defn registry
  "Return a lightweight registry of pattern entities + relations for fast diffs."
  ([conn] (registry conn {}))
  ([conn {:keys [include-relations?]
          :or {include-relations? true}}]
   (if (xt/started?)
     (registry-from-xtdb conn include-relations?)
     (registry-from-datascript conn include-relations?))))
