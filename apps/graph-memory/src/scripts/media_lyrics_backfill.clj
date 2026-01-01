(ns scripts.media-lyrics-backfill
  "Backfill :media/lyrics relations from existing media entities."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private relation-type :media/lyrics)
(def ^:private lyrics-type :arxana/media-lyrics)
(def ^:private track-type :arxana/media-track)

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-lyrics-backfill [--dry-run]"
    "Environment:"
    "  ALPHA_PROFILE      Profile name (defaults to configured profile)"
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

(defn- lyrics-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id] %) ids)))

(defn- relation-exists? [db src-id dst-id]
  (boolean
   (seq (d/q '[:find ?r
               :in $ ?src-id ?dst-id
               :where
               [?r :relation/type :media/lyrics]
               [?r :relation/src ?src]
               [?src :entity/id ?src-id]
               [?r :relation/dst ?dst]
               [?dst :entity/id ?dst-id]]
             db src-id dst-id))))

(defn- derive-track-external [lyrics-external]
  (when (and (string? lyrics-external) (seq (str/trim lyrics-external)))
    (let [prefix "arxana/media-lyrics/"]
      (when (str/starts-with? lyrics-external prefix)
        (str "arxana/media/" (subs lyrics-external (count prefix)))))))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)]
      (try
        (let [db @conn
              lyrics (lyrics-entities db)
              missing-track (atom [])
              created (atom 0)
              skipped (atom 0)]
          (println (format "Media lyrics backfill: lyrics=%d dry-run=%s"
                           (count lyrics) (:dry-run? opts)))
          (doseq [{:entity/keys [id name external-id]} lyrics]
            (let [track-external (derive-track-external external-id)
                  track (when track-external
                          (store/fetch-entity conn {:external-id track-external
                                                    :type track-type}
                                             {}))]
              (cond
                (nil? track)
                (swap! missing-track conj {:lyrics-id id
                                           :lyrics-name name
                                           :lyrics-external external-id
                                           :track-external track-external})

                (relation-exists? db (:id track) id)
                (swap! skipped inc)

                (:dry-run? opts)
                (do
                  (swap! created inc)
                  (println (format "Would link %s -> %s" (:name track) name)))

                :else
                (do
                  (store/upsert-relation! conn env
                                          {:type relation-type
                                           :src {:id (:id track)
                                                 :name (:name track)
                                                 :type track-type}
                                           :dst {:id id
                                                 :name name
                                                 :type lyrics-type}})
                  (swap! created inc)
                  (println (format "Linked %s -> %s" (:name track) name))))))
          (when (seq @missing-track)
            (println (format "Missing tracks: %d" (count @missing-track)))
            (doseq [{:keys [lyrics-name lyrics-external track-external]} @missing-track]
              (println (format "  - %s (%s -> %s)"
                               (or lyrics-name "?")
                               (or lyrics-external "?")
                               (or track-external "?")))))
          (println (format "Backfill summary: created=%d skipped=%d"
                           @created @skipped)))
        (finally
          (store-manager/shutdown!))))))
