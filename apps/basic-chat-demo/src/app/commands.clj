(ns app.commands
  "Lightweight parser + executor for inline CLI commands (e.g. !entity / !rel)."
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [graph-memory.main :as gm]))

(defn- read-tokens [line]
  (try
    (vec (edn/read-string (str "[" line "]")))
    (catch Exception e
      (throw (ex-info "Unable to parse command" {:line line} e)))))

(defn- token->string [token]
  (cond
    (string? token) token
    (symbol? token) (name token)
    (keyword? token) (name token)
    (number? token) (str token)
    :else (str token)))

(defn- token->keyword [token]
  (-> token token->string str/lower-case keyword))

(defn- parse-entity-command [tokens]
  (let [[raw-name maybe-type] tokens
        name (some-> raw-name token->string str/trim)]
    (when (str/blank? name)
      (throw (ex-info "Entity name required" {:tokens tokens})))
    (let [entity (cond-> {:name name}
                   maybe-type (assoc :type (token->keyword maybe-type)))]
      {:command :entity
       :entity entity})))

(defn- parse-provenance [tokens]
  (loop [pairs tokens
         acc {}]
    (if (seq pairs)
      (let [[k v & more] pairs
            key (token->keyword k)]
        (when (nil? v)
          (throw (ex-info "Missing value for provenance field" {:key k})))
        (case key
          (:since :until)
          (recur more (assoc acc key (token->string v)))

          :note
          (recur more (assoc acc key (token->string v)))

          (throw (ex-info "Unknown provenance key" {:key k}))))
      acc)))

(defn- parse-relation-command [tokens]
  (let [[src raw-type dst & rest] tokens]
    (when (or (nil? src) (nil? raw-type) (nil? dst))
      (throw (ex-info "Relation requires src, type, dst" {:tokens tokens})))
    (let [relation {:type (token->keyword raw-type)
                    :src {:name (token->string src)}
                    :dst {:name (token->string dst)}}
          provenance (parse-provenance rest)]
      {:command :relation
       :relation (cond-> relation
                    (seq provenance) (assoc :provenance provenance))})))

(defn- parse-links-command [tokens]
  (let [[raw-name] tokens
        name (some-> raw-name token->string str/trim)]
    (when (str/blank? name)
      (throw (ex-info "Name required" {:tokens tokens})))
    {:command :links
     :name name}))

(defn- parse-command [line]
  (let [tokens (read-tokens line)
        [head & tail] tokens
        cmd (-> head token->string str/lower-case)]
    (case cmd
      "entity" (parse-entity-command tail)
      "rel"    (parse-relation-command tail)
      "links"  (parse-links-command tail)
      (throw (ex-info "Unknown command" {:token head})))))

(defn- entity-message [before after]
  (cond
    (nil? before)
    (format "entity '%s' created%s"
            (:name after)
            (if-let [t (:type after)] (str " as " (name t)) ""))

    (not= (:type before) (:type after))
    (format "entity '%s' type updated to %s"
            (:name after)
            (if-let [t (:type after)] (name t) "unknown"))

    :else
    (format "entity '%s' unchanged" (:name after))))

(defn- relation-message [rel]
  (let [{:keys [src dst type provenance]} rel
        base (format "relation %s %s %s saved"
                     (:name src)
                     (name type)
                     (:name dst))]
    (if (seq provenance)
      (str base " " (str/join ", " (for [[k v] provenance]
                                      (str (name k) " " v))))
      base)))

(defn handle
  "Execute a raw command line (without the leading '!').
   Returns {:message .. :result ..}."
  [conn opts line]
  (let [trimmed (str/trim line)]
    (when (str/blank? trimmed)
      (throw (ex-info "Empty command" {})))
    (let [{:keys [command entity relation name]} (parse-command trimmed)]
      (case command
        :entity
        (let [before (store/resolve-name->eid conn (:name entity))
              after (store/ensure-entity! conn opts entity)]
          {:message (entity-message before after)
           :result after})

        :relation
        (let [src-entity (store/ensure-entity! conn opts (:src relation))
              dst-entity (store/ensure-entity! conn opts (:dst relation))
              relation-spec (-> relation
                                (assoc :src (select-keys src-entity [:id :name :type]))
                                (assoc :dst (select-keys dst-entity [:id :name :type])))
              result (store/upsert-relation! conn opts relation-spec)]
          {:message (relation-message result)
           :result result})

        :links
        (let [target name
              entity-info (store/resolve-name->eid conn target)]
          (if-not entity-info
            {:message (str "entity not found: " target)
             :result nil}
            (let [neighbors (gm/neighbors @conn (:id entity-info))
                  message (if (seq neighbors)
                            (str target " links: "
                                 (->> neighbors
                                      (map (fn [{:keys [relation direction entity]}]
                                             (let [arrow (if (= direction :out) "->" "<-")]
                                               (str (name relation) arrow (:entity/name entity)))))
                                      (str/join ", ")))
                            (str target " links: none"))]
              {:message message
               :result {:entity entity-info
                        :neighbors neighbors}})))))))
