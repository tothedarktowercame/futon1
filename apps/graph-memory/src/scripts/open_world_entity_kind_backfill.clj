(ns scripts.open-world-entity-kind-backfill
  "Backfill :entity/type (and thus :entity/kind) for entities missing open-world kind."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]
            [datascript.core]))

(def ^:private media-lyrics-prefix "arxana/media-lyrics/")
(def ^:private media-track-prefix "arxana/media/")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.open-world-entity-kind-backfill [--dry-run]"
    "Environment:"
    "  ALPHA_PROFILE       Profile name (defaults to configured profile)"
    "  BASIC_CHAT_DATA_DIR Data root (required if server uses a non-default data dir)"]))

(defn- parse-args [args]
  (loop [opts {:dry-run? false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--dry-run" (recur (assoc opts :dry-run? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- infer-kind [doc existing]
  (let [raw-type (or (:entity/type existing)
                     (:entity/type doc)
                     (:entity/kind doc))
        raw-id (or (:entity/external-id existing)
                   (:entity/external-id doc)
                   (:entity/label doc)
                   (:entity/name doc))]
    (cond
      (keyword? raw-type) raw-type
      (and (string? raw-type)
           (contains? #{"arxana/media-track" "arxana/media-lyrics"} (str/trim raw-type)))
      (keyword (str/trim raw-type))
      (string? raw-id)
      (cond
        (str/starts-with? raw-id media-lyrics-prefix) :arxana/media-lyrics
        (str/starts-with? raw-id media-track-prefix) :arxana/media-track
        :else nil)
      :else nil)))

(defn- candidate-ids-xt []
  (->> (xt/q '{:find [?eid]
               :where [[?e :entity/id ?eid]
                       [?e :entity/label _]]})
       (map first)
       (remove nil?)))

(defn- candidate-ids-ds [db]
  (->> (datascript.core/q '[:find ?id
                            :where
                            [?e :entity/id ?id]
                            (not [?e :entity/kind _])
                            [?e :entity/type _]]
                          db)
       (map first)
       (remove nil?)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)
          db @conn
          ids (distinct (concat (candidate-ids-xt) (candidate-ids-ds db)))
          fixed (atom 0)
          skipped (atom 0)]
      (println (format "Open-world kind backfill: candidates=%d dry-run=%s"
                       (count ids) (:dry-run? opts)))
      (doseq [eid ids]
        (let [doc (xt/entity eid)
              existing (store/fetch-entity conn {:id eid} {})
              kind (infer-kind doc existing)
              name (or (:entity/name existing)
                       (:entity/name doc)
                       (:entity/label doc)
                       (str eid))
              external-id (or (:entity/external-id existing)
                              (:entity/external-id doc))]
          (if (and kind (not (:entity/kind doc)))
            (if (:dry-run? opts)
              (do
                (swap! fixed inc)
                (println (format "Would set %s kind=%s" name kind)))
              (do
                (store/ensure-entity! conn env
                                      (cond-> {:id eid
                                               :name name
                                               :type kind}
                                        external-id (assoc :external-id external-id)))
                (swap! fixed inc)
                (println (format "Set %s kind=%s" name kind))))
            (swap! skipped inc))))
      (println (format "Backfill summary: fixed=%d skipped=%d" @fixed @skipped))
      (store-manager/shutdown!))))
