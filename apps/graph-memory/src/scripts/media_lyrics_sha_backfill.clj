(ns scripts.media-lyrics-sha-backfill
  "Backfill :media/sha256 on lyrics entities from external-id."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private lyrics-type :arxana/media-lyrics)
(def ^:private lyrics-prefix "arxana/media-lyrics/")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-lyrics-sha-backfill [--dry-run]"
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

(defn- lyrics-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id :media/sha256] %) ids)))

(defn- valid-sha? [value]
  (boolean (and (string? value)
                (re-matches #"[0-9a-f]{64}" (str/lower-case value)))))

(defn- derive-sha [external-id]
  (when (and (string? external-id)
             (str/starts-with? external-id lyrics-prefix))
    (let [token (last (str/split external-id #"/"))
          token (some-> token str/lower-case)]
      (when (valid-sha? token)
        token))))

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
              updated (atom 0)
              skipped (atom 0)
              missing (atom [])]
          (println (format "Lyrics SHA backfill: lyrics=%d dry-run=%s"
                           (count lyrics) (:dry-run? opts)))
          (doseq [{:entity/keys [id name external-id] :media/keys [sha256]} lyrics]
            (cond
              (and sha256 (not (str/blank? sha256)))
              (swap! skipped inc)

              (str/blank? external-id)
              (swap! missing conj {:lyrics-id id :lyrics-name name :issue :missing-external-id})

              :else
              (if-let [sha (derive-sha external-id)]
                (if (:dry-run? opts)
                  (do
                    (swap! updated inc)
                    (println (format "Would tag %s -> %s" (or name "?") sha)))
                  (do
                    (store/ensure-entity! conn env
                                          {:name (or name (str external-id " (lyrics)"))
                                           :type lyrics-type
                                           :external-id external-id
                                           :media/sha256 sha})
                    (swap! updated inc)
                    (println (format "Tagged %s -> %s" (or name "?") sha))))
                (swap! missing conj {:lyrics-id id
                                     :lyrics-name name
                                     :issue :unparseable-sha
                                     :external-id external-id}))))
          (when (seq @missing)
            (println (format "Missing/unparseable: %d" (count @missing)))
            (doseq [row (take 20 @missing)]
              (println (format "  - %s (%s)"
                               (or (:lyrics-name row) "?")
                               (or (:external-id row) (:issue row))))))
          (println (format "Backfill summary: tagged=%d skipped=%d"
                           @updated @skipped)))
        (finally
          (store-manager/shutdown!))))))
