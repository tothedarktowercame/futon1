;; scripts/media_lyrics_prune_placeholders.clj
(ns scripts.media-lyrics-prune-placeholders
  "Remove placeholder lyrics entities so invariants stop failing."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private default-names
  #{"salt (lyrics)"
    "the-moon-rising (lyrics)"
    "20251130_112854_track1-01 (lyrics)"
    "Dec27B (lyrics)"})

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-lyrics-prune-placeholders [--apply] [--names name1,name2,...]"
    ""
    "Options:"
    "  --apply     Apply deletions (default: dry-run)"
    "  --names     Comma-separated lyric names to prune"
    ""]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false :names default-names}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--apply" arg) (recur (rest args) (assoc opts :apply? true))
          (= "--names" arg) (let [raw (second args)
                                  names (->> (str/split (or raw "") #",")
                                             (map str/trim)
                                             (remove str/blank?)
                                             set)]
                              (recur (nnext args) (assoc opts :names names)))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- placeholder-lyrics [db names]
  (let [ids (->> (d/q '[:find ?e
                        :in $ [?name ...]
                        :where
                        [?e :entity/type :arxana/media-lyrics]
                        [?e :entity/name ?name]]
                      db (vec names))
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/source] %) ids)))

(defn -main [& args]
  (let [{:keys [apply? names]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (try
      (let [db @conn
            targets (vec (placeholder-lyrics db names))]
        (println (format "Placeholder lyrics: %d apply=%s" (count targets) apply?))
        (doseq [{:entity/keys [id name source]} targets]
          (if apply?
            (do
              (store/forget-entity! conn env {:id id :name name})
              (println (format "Deleted %s (%s) source=%s"
                               name id (or source "?"))))
            (println (format "Would delete %s (%s) source=%s"
                             name id (or source "?"))))))
      (finally
        (store-manager/shutdown!)))))
