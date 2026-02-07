;; scripts/media_lyrics_wipe.clj
(ns scripts.media-lyrics-wipe
  "Delete all media lyrics entities and their :media/lyrics relations."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private lyrics-type :arxana/media-lyrics)
(def ^:private relation-type :media/lyrics)

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-lyrics-wipe [--apply]"
    ""
    "Options:"
    "  --apply       Apply deletions (default: dry-run)"
    "  --no-verify   Disable verify-on-write for this run (repair mode)"
    ""]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false :no-verify? false}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--apply" arg) (recur (rest args) (assoc opts :apply? true))
          (= "--no-verify" arg) (recur (rest args) (assoc opts :no-verify? true))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- lyrics-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id :entity/source] %) ids)))

(defn- lyrics-relations [db]
  (let [ids (->> (d/q '[:find ?r
                        :where
                        [?r :relation/type :media/lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db [:relation/id
                      {:relation/src [:entity/id :entity/name]}
                      {:relation/dst [:entity/id :entity/name]}]
                  %) ids)))

(defn -main [& args]
  (let [{:keys [apply? no-verify?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)
        env (if no-verify? (dissoc env :verify-fn) env)]
    (try
      (let [db @conn
            relations (vec (lyrics-relations db))
            entities (vec (lyrics-entities db))]
        (println (format "Lyrics wipe: lyrics=%d relations=%d apply=%s"
                         (count entities) (count relations) apply?))
        (doseq [{:relation/keys [id src dst]} relations]
          (if apply?
            (do
              (store/delete-relation! conn env {:id id})
              (println (format "Deleted relation %s src=%s dst=%s"
                               id
                               (or (:entity/name src) (:entity/id src) "?")
                               (or (:entity/name dst) (:entity/id dst) "?"))))
            (println (format "Would delete relation %s src=%s dst=%s"
                             id
                             (or (:entity/name src) (:entity/id src) "?")
                             (or (:entity/name dst) (:entity/id dst) "?")))))
        (doseq [{:entity/keys [id name external-id source]} entities]
          (if apply?
            (do
              (store/forget-entity! conn env {:id id :name name})
              (println (format "Deleted %s (%s) external=%s source=%s"
                               (or name "?") id (or external-id "?") (or source "?"))))
            (println (format "Would delete %s (%s) external=%s source=%s"
                             (or name "?") id (or external-id "?") (or source "?"))))))
      (finally
        (store-manager/shutdown!)))))
