(ns headless_api.handlers.types
  (:require [clojure.string :as str]
            [graph-memory.types_registry :as types]))

(defn- normalize-kind [value]
  (let [raw (some-> value name str/lower-case)]
    (case raw
      ("relation" "relations" "rel") :relation
      :entity)))

(defn- parse-type [value]
  (cond
    (keyword? value) value
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (if (str/starts-with? trimmed ":")
                          (when (> (count trimmed) 1)
                            (keyword (subs trimmed 1)))
                          (keyword trimmed))))
    :else nil))

(defn- type->string [kw]
  (when kw
    (if-let [ns (namespace kw)]
      (str ns "/" (name kw))
      (name kw))))

(defn- doc->response [{:keys [id kind parent alias-of aliases inferred?]}]
  (cond-> {:id (type->string id)
           :kind (name kind)}
    parent (assoc :parent (type->string parent))
    alias-of (assoc :alias_of (type->string alias-of))
    (seq aliases) (assoc :aliases (->> aliases
                                       (map type->string)
                                       (remove nil?)
                                       (sort)
                                       vec))
    (some? inferred?) (assoc :inferred_parent inferred?)))

(defn list-types [_]
  (let [cache (types/load-cache!)
        docs (:docs cache)
        grouped (group-by :kind docs)
        build (fn [kind]
                (->> (get grouped kind [])
                     (sort-by (comp #(or % "") type->string :id))
                     (mapv doc->response)))]
    {:types {:entity (build :entity)
             :relation (build :relation)}}))

(defn set-parent! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (let [kind (normalize-kind (:kind body))
        type (or (parse-type (:type body))
                 (throw (ex-info "Type is required" {:status 400})))
        parent (parse-type (:parent body))
        doc (types/set-parent! kind type parent)]
    {:type (doc->response doc)}))

(defn merge-aliases! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (let [kind (normalize-kind (:kind body))
        canonical (or (parse-type (or (:into body) (:type body)))
                      (throw (ex-info "Canonical type required" {:status 400})))
        raw-aliases (concat (when-let [alias (:alias body)] [alias])
                            (when-let [aliases (:aliases body)]
                              (if (sequential? aliases) aliases [aliases])))
        aliases (->> raw-aliases (map parse-type) (remove nil?) set)]
    (when (empty? aliases)
      (throw (ex-info "At least one alias required" {:status 400})))
    (let [doc (types/merge! kind canonical aliases)]
      {:type (doc->response doc)})))
