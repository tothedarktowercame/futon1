(ns scripts.media-missing-fields-repair
  "Backfill missing media fields and optionally prune empty lyrics entities."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private lyrics-type :arxana/media-lyrics)
(def ^:private track-type :arxana/media-track)
(def ^:private track-prefix "arxana/media/")
(def ^:private lyrics-prefix "arxana/media-lyrics/")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-missing-fields-repair [--apply] [--prune-empty]"
    ""
    "Options:"
    "  --apply         Apply changes (default: dry-run)"
    "  --prune-empty   Delete lyrics entities missing source text"
    ""
    "Environment:"
    "  ALPHA_PROFILE       Profile name (defaults to configured profile)"
    "  BASIC_CHAT_DATA_DIR Data root (required if server uses a non-default data dir)"]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false :prune-empty? false}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--apply" arg) (recur (rest args) (assoc opts :apply? true))
          (= "--prune-empty" arg) (recur (rest args) (assoc opts :prune-empty? true))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- track-missing-external [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-track]
                        (not [?e :entity/external-id _])]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id :entity/source] %) ids)))

(defn- track-missing-source [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-track]
                        (not [?e :entity/source _])]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id :entity/source] %) ids)))

(defn- lyrics-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-lyrics]]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id :entity/source :media/sha256] %) ids)))

(defn- derive-track-external [name]
  (when (and (string? name)
             (seq (str/trim name))
             (str/starts-with? name track-prefix))
    name))

(defn- derive-lyrics-external [name]
  (when (and (string? name)
             (seq (str/trim name))
             (str/starts-with? name lyrics-prefix))
    name))

(defn- derive-sha [external-id]
  (when (and (string? external-id)
             (str/includes? external-id "/"))
    (last (str/split external-id #"/"))))

(defn- blank-source? [value]
  (or (nil? value)
      (and (string? value) (str/blank? value))))

(defn- derive-track-source [value]
  (when (and (string? value)
             (str/includes? value "/"))
    (let [parts (str/split value #"/")
          src (nth parts 2 nil)]
      (when (and src (not (str/blank? src)))
        src))))

(defn -main [& args]
  (let [{:keys [apply? prune-empty?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (try
      (let [db @conn
            tracks (vec (track-missing-external db))
            tracks-missing-source (vec (track-missing-source db))
            lyrics (vec (lyrics-entities db))
            track-fixed (atom 0)
            track-source-fixed (atom 0)
            lyrics-fixed (atom 0)
            lyrics-pruned (atom 0)
            lyrics-missing (atom 0)]
        (println (format "Media repair: tracks-missing=%d tracks-missing-source=%d lyrics=%d apply=%s prune-empty=%s"
                         (count tracks) (count tracks-missing-source) (count lyrics) apply? prune-empty?))
        (doseq [{:entity/keys [id name external-id]} tracks]
          (let [new-external (or external-id (derive-track-external name))]
            (when new-external
              (if apply?
                (do
                  (store/ensure-entity! conn env {:id id
                                                  :name (or name (str id))
                                                  :type track-type
                                                  :external-id new-external})
                  (swap! track-fixed inc)
                  (println (format "Set track external-id %s -> %s" (or name id) new-external)))
                (println (format "Would set track external-id %s -> %s" (or name id) new-external))))))
        (doseq [{:entity/keys [id name external-id source]} tracks-missing-source]
          (let [new-source (or (derive-track-source external-id)
                               (derive-track-source name))]
            (when (and (blank-source? source) new-source)
              (if apply?
                (do
                  (store/ensure-entity! conn env {:id id
                                                  :name (or name (str id))
                                                  :type track-type
                                                  :external-id external-id
                                                  :source new-source})
                  (swap! track-source-fixed inc)
                  (println (format "Updated track source for %s -> %s" (or name id) new-source)))
                (println (format "Would update track source for %s -> %s" (or name id) new-source))))))
        (doseq [{:entity/keys [id name external-id source] :media/keys [sha256]} lyrics]
          (let [new-external (or external-id (derive-lyrics-external name))
                new-sha (or sha256 (derive-sha new-external))
                needs-update? (or (and new-external (not= new-external external-id))
                                  (and new-sha (not= new-sha sha256)))]
            (cond
              (and prune-empty? (blank-source? source))
              (if apply?
                (do
                  (store/forget-entity! conn env {:id id :name name})
                  (swap! lyrics-pruned inc)
                  (println (format "Deleted empty lyrics %s (%s)" name id)))
                (println (format "Would delete empty lyrics %s (%s)" name id)))

              (blank-source? source)
              (do
                (swap! lyrics-missing inc)
                (when needs-update?
                  (if apply?
                    (do
                      (store/ensure-entity! conn env
                                            (cond-> {:id id
                                                     :name (or name (str id))
                                                     :type lyrics-type}
                                              new-external (assoc :external-id new-external)
                                              new-sha (assoc :media/sha256 new-sha)))
                      (swap! lyrics-fixed inc)
                      (println (format "Updated lyrics ids for %s" (or name id))))
                    (println (format "Would update lyrics ids for %s" (or name id)))))))

              needs-update?
              (if apply?
                (do
                  (store/ensure-entity! conn env
                                        (cond-> {:id id
                                                 :name (or name (str id))
                                                 :type lyrics-type}
                                          new-external (assoc :external-id new-external)
                                          new-sha (assoc :media/sha256 new-sha)
                                          source (assoc :source source)))
                  (swap! lyrics-fixed inc)
                  (println (format "Updated lyrics ids for %s" (or name id))))
                (println (format "Would update lyrics ids for %s" (or name id))))))
        (println (format "Repair summary: tracks-fixed=%d tracks-source-fixed=%d lyrics-fixed=%d lyrics-pruned=%d lyrics-missing-source=%d"
                         @track-fixed @track-source-fixed @lyrics-fixed @lyrics-pruned @lyrics-missing)))
      (finally
        (store-manager/shutdown!)))))
