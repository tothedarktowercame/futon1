(ns api.handlers.me
  (:require [api.util.http :as http]
            [app.command-service :as commands]
            [app.focus :as focus]
            [app.slash.format :as fmt]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]
            [datascript.core :as d]
            [xtdb.api :as xta]))

(defn- qparam [req k]
  (get-in req [:query-params (name k)]))

(defn- long-or-nil [s]
  (when (some? s)
    (try (long (if (string? s) (Long/parseLong s) s))
         (catch Throwable _ nil))))

(defn- clamp [x lo hi]
  (-> x (max lo) (min hi)))

(def ^:private default-limit 5)
(def ^:private max-limit 50)

(defn- normalize-profile [value]
  (when-let [trimmed (some-> value str/trim not-empty)]
    (let [lower (str/lower-case trimmed)]
      (case lower
        "me" :me
        trimmed))))

(defn- request-profile [request]
  (or (normalize-profile (qparam request :profile))
      (normalize-profile (get-in request [:headers "x-profile"]))
      (normalize-profile (get-in request [:ctx :profile/id]))
      (normalize-profile (get-in request [:ctx :profile]))
      :me))

(defn- request-limit [request]
  (let [raw (or (get-in request [:query-params :limit])
                (get-in request [:route-params :limit]))]
    (-> (or (long-or-nil raw) default-limit)
        (clamp 1 max-limit))))

(defn- enrich-ctx [request]
  (let [base    (:ctx request)
        qps     (:query-params request)
        now     (System/currentTimeMillis)
        prof    (request-profile request)
        limit   (request-limit request)
        conn    (store-manager/conn prof)
        env     (store-manager/env prof)
        ds-db   (try (d/db conn) (catch Throwable _ nil))
        xt-node (or (:xt-node base)
                    (:xtdb-node base)
                    (xt/node))
        xt-db   (when xt-node (try (xta/db xt-node) (catch Throwable _ nil)))]
    (-> base
        (assoc :profile prof
               :profile/id prof
               :query-params qps
               :limit limit
               :now now
               :conn conn
               :env env)
        (cond-> ds-db (assoc :ds/db ds-db)
                xt-node (assoc :xt-node xt-node)
                xt-db (assoc :xt/db xt-db)))))

(defn fetch [request]
  (let [ctx* (enrich-ctx request)]
    (commands/fetch-profile ctx*)))

(defn fetch-handler [request]
  (http/ok-json (fetch request)))

(defn upsert! [request patch]
  (commands/upsert-profile! {:profile (request-profile request)} patch))

(defn upsert-handler [request]
  (http/ok-json (upsert! request (:body request))))

(defn slim-ctx [ctx]
  (let [db (-> ctx :ds/db)
        stats (when db
                {:entities (:max-eid db)
                 :has-me (boolean (some #(= :me (:v %)) (:avet db)))})]
    (-> ctx
        (select-keys [:now :limit :profile :profile/id :env])
        (assoc :ds stats
               :has-xtdb (boolean (-> ctx :xt-node))
               :has-xtdb-db (boolean (-> ctx :xt/db))))))

(defn summary [request]
  (let [debug?  (= "1" (qparam request :debug))
        profile (request-profile request)
        limit   (request-limit request)
        ctx     (enrich-ctx request)]
    (tap> (slim-ctx ctx))
    (if debug?
      (let [ds-db     (:ds/db ctx)
            xt-db     (:xt/db ctx)
            eid       (try (commands/resolve-me-id ctx) (catch Throwable _ nil))
            activated (try (or (some-> (focus/ds-activated-ids ds-db eid) set)
                               (some-> (focus/ds-activated-ids ds-db) set)
                               #{})
                           (catch Throwable _ #{}))
            neighbors (try (when (and xt-db eid)
                             (focus/top-neighbors ds-db xt-db eid
                                                  {:k (:limit ctx)
                                                   :k-per-anchor 3
                                                   :time-hint (:time-hint ctx)}))
                           (catch Throwable _ nil))
            recent    (try (store/recent-relations ctx) (catch Throwable _ nil))]
        (http/ok-json
         {:ctx-keys          (-> ctx keys sort vec)
          :have-ds           (boolean ds-db)
          :have-xt           (boolean xt-db)
          :profile           (:profile ctx)
          :limit             (:limit ctx)
          :entity-id         (str eid)
          :activated-ids     (map str activated)
          :neighbors-count   (count neighbors)
          :neighbors-sample  (->> neighbors (take 5))
          :recent-count      (count recent)
          :recent-sample     (->> recent (take 5))
          :query-params      (:query-params request)}))
      (let [raw-summary      (try (commands/profile-summary ctx nil)
                                  (catch clojure.lang.ArityException _
                                    (commands/profile-summary ctx limit)))
            resolved-profile (or profile (:profile raw-summary))
            profile-header   (some-> resolved-profile str)]
        (if (and raw-summary (:text raw-summary))
          (let [summary (assoc raw-summary :profile resolved-profile)
                lines   (fmt/profile-summary-lines summary)
                text    (str/join "\n" lines)]
            (http/ok-text text 200 (cond-> {}
                                     profile-header (assoc "X-Profile" profile-header))))
          (http/ok-text "No profile data recorded." 200 (cond-> {}
                                                          profile-header (assoc "X-Profile" profile-header))))))))

(defn summary-handler [request]
  (http/ok-json (summary request)))
