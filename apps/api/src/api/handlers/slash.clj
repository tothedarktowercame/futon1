(ns api.handlers.slash
  (:require [api.util.http :as http]
            [app.command-service :as commands]
            [app.slash.format :as fmt]
            [clojure.string :as str]))

(def ^:private default-tail-limit 5)
(def ^:private max-tail-limit 50)

(defn- ds-conn [request]
  (or (get-in request [:ctx :conn])
      (throw (ex-info "Datascript connection unavailable" {:status 500}))))

(defn- env-opts [request]
  (or (get-in request [:ctx :env]) {}))

(defn- parse-int-safe [value]
  (when (some? value)
    (try (Integer/parseInt (str value))
         (catch Exception _ nil))))

(defn- sanitize-limit [raw]
  (-> (or (parse-int-safe raw) default-tail-limit)
      (max 1)
      (min max-tail-limit)))

(defn tail [request]
  (let [conn (ds-conn request)
        raw-limit (get-in request [:query-params "limit"])
        limit (sanitize-limit raw-limit)
        relations (commands/tail conn limit)
        lines (fmt/tail-lines relations)]
    (http/ok-json {:command "/tail"
                   :limit limit
                   :relations relations
                   :lines lines})))

(defn- requested-name [request]
  (or (some-> (get-in request [:path-params :name]) str/trim)
      (some-> (get-in request [:query-params "name"]) str/trim)))

(defn ego [request]
  (let [entity-name (requested-name request)]
    (if (str/blank? entity-name)
      (http/ok-json {:command "/ego"
                     :lines ["Usage: /ego <entity>"]})
      (let [conn (ds-conn request)
            data (commands/ego conn entity-name)
            lines (fmt/ego-lines data entity-name)]
        (http/ok-json {:command "/ego"
                       :name entity-name
                       :lines lines
                       :ego data})))))

(defn cooccur [request]
  (let [entity-name (requested-name request)]
    (if (str/blank? entity-name)
      (http/ok-json {:command "/cooccur"
                     :lines ["Usage: /cooccur <entity>"]})
      (let [conn (ds-conn request)
            data (commands/cooccurring conn entity-name)
            lines (fmt/cooccur-lines data entity-name)]
        (http/ok-json {:command "/cooccur"
                       :name entity-name
                       :lines lines
                       :cooccurrences data})))))

(defn forget! [request]
  (let [entity-name (some-> (get-in request [:body :name]) str/trim)]
    (if (str/blank? entity-name)
      (http/ok-json {:command "/forget"
                     :lines ["Usage: /forget <entity>"]})
      (let [conn (ds-conn request)
            opts (env-opts request)
            result (commands/forget-entity! conn opts entity-name)
            lines (fmt/forget-lines result entity-name)]
        (http/ok-json {:command "/forget"
                       :name entity-name
                       :lines lines
                       :result result})))))

(defn expire! [request]
  (let [entity-name (some-> (get-in request [:body :name]) str/trim)]
    (if (str/blank? entity-name)
      (http/ok-json {:command "/expire"
                     :lines ["Usage: /expire <entity>"]})
      (let [conn (ds-conn request)
            opts (env-opts request)
            entity (commands/expire-entity! conn opts entity-name)
            lines (fmt/expire-lines entity entity-name)]
        (http/ok-json {:command "/expire"
                       :name entity-name
                       :lines lines
                       :entity entity})))))

(defn help [_]
  (http/ok-json {:command "/help"
                 :commands fmt/help-lines
                 :lines fmt/help-lines}))
