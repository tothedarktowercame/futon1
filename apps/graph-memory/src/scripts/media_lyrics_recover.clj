;; scripts/media_lyrics_recover.clj
(ns scripts.media-lyrics-recover
  "Recover media lyrics entities from JSON exports."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private relation-type :media/lyrics)
(def ^:private lyrics-type :arxana/media-lyrics)
(def ^:private track-type :arxana/media-track)

(def ^:private title-overrides
  {"this is my mother" "page-1-this-is-my-mother"
   "generally alone" "page-2-generally-alone"
   "hand pen eyes" "page-3-hand-pen-eyes"
   "writing on a train" "page-4-writing-on-a-train"
   "warehouse light" "page-5-warehouse-light"
   "don't stake too much" "page-6-don-t-stake-too-much"
   "the wicked" "page-7-the-wicked"
   "new1" "reducing the dimension"})

(def ^:private default-recovery-paths
  ["storage/pma-ep1-lyrics-data-recovery.json"
   "storage/pma-ep2-lyrics-data-recovery.json"])

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-lyrics-recover [--dry-run] [paths...]"
    ""
    "If no paths are supplied, defaults to:"
    (str "  " (str/join "\n  " default-recovery-paths))
    ""]))

(defn- parse-args [args]
  (loop [args args opts {:dry-run? false :paths []}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--dry-run" arg) (recur (rest args) (assoc opts :dry-run? true))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (recur (rest args) (update opts :paths conj arg)))))))

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

(defn- resolve-path [path]
  (let [candidate (io/file path)]
    (cond
      (.exists candidate) (.getAbsolutePath candidate)
      :else (when-let [root (repo-root)]
              (let [alt (io/file root path)
                    parent (.getParentFile root)
                    alt-parent (when parent (io/file parent path))]
                (cond
                  (.exists alt) (.getAbsolutePath alt)
                  (and alt-parent (.exists alt-parent)) (.getAbsolutePath alt-parent)
                  :else nil))))))

(defn- slugify [value]
  (when (seq (str/trim (or value "")))
    (-> value
        str/lower-case
        (str/replace #"[^a-z0-9]+" "-")
        (str/replace #"(^-|-$)" ""))))

(defn- trim-right [value]
  (if (string? value)
    (str/replace value #"\s+$" "")
    value))

(defn- track-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :arxana/media-track]]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/external-id :entity/type] %) ids)))

(defn- lyrics-entity-by-name [db name]
  (when (seq name)
    (let [ids (->> (d/q '[:find ?e
                          :in $ ?name
                          :where
                          [?e :entity/name ?name]
                          [?e :entity/type :arxana/media-lyrics]]
                        db name)
                    (map first))]
      (when-let [eid (first ids)]
        (d/pull db [:entity/id :entity/name :entity/type :entity/external-id] eid)))))

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

(defn- find-track [tracks entry]
  (let [title (:title entry)
        title-key (some-> title str/lower-case)
        slug (or (:slug entry) (slugify title))
        override (get title-overrides title-key)
        candidates (remove str/blank? [override slug])
        direct (some (fn [name] (some #(when (= name (:entity/name %)) %) tracks))
                     candidates)
        suffix (when (seq slug)
                 (some #(when (str/ends-with? (:entity/name %) slug) %) tracks))]
    (or direct suffix)))

(defn- lyrics-id-from-track [track-external]
  (when (str/starts-with? track-external "arxana/media/")
    (str "arxana/media-lyrics/" (subs track-external (count "arxana/media/")))))

(defn- sha-from-external [external-id]
  (when (and (string? external-id) (str/includes? external-id "/"))
    (last (str/split external-id #"/"))))

(defn- load-json [path]
  (let [data (json/read-str (slurp path) :key-fn keyword)]
    (when-not (map? data)
      (throw (ex-info "Unexpected JSON shape" {:path path})))
    data))

(defn- recover-file! [conn env tracks {:keys [path dry-run?]}]
  (let [data (load-json path)
        ep (:ep data)
        items (:tracks data)
        missing-track (atom [])
        missing-lyrics (atom 0)
        updated (atom 0)
        linked (atom 0)
        skipped (atom 0)]
    (println (format "Lyrics recovery: %s tracks=%d dry-run=%s" ep (count items) dry-run?))
    (doseq [entry items]
      (let [track (find-track tracks entry)]
        (if (nil? track)
          (swap! missing-track conj (select-keys entry [:title :slug]))
          (let [track-name (:entity/name track)
                track-id (:entity/id track)
                track-external (:entity/external-id track)
                lyrics-id (lyrics-id-from-track track-external)
                sha (sha-from-external track-external)
                title-lyrics-name (format "%s (lyrics)" (:title entry))
                track-lyrics-name (format "%s (lyrics)" track-name)
                existing (or (lyrics-entity-by-name @conn track-lyrics-name)
                             (lyrics-entity-by-name @conn title-lyrics-name))
                lyrics-id (or (:entity/id existing) (lyrics-id-from-track track-external))
                lyrics-name (or (:entity/name existing) title-lyrics-name)
                lyrics (trim-right (or (:lyrics entry) ""))]
            (cond
              (str/blank? lyrics)
              (swap! missing-lyrics inc)

              (or (nil? lyrics-id)
                  (and (string? lyrics-id) (str/blank? lyrics-id))
                  (str/blank? sha))
              (swap! missing-track conj (select-keys entry [:title :slug]))

              dry-run?
              (do
                (swap! updated inc)
                (println (format "Would upsert lyrics+link for %s (%s)" track-name lyrics-id)))

              :else
              (let [existed? (relation-exists? @conn track-id lyrics-id)]
                (store/upsert-relation! conn env
                                        {:type relation-type
                                         :src {:id track-id
                                               :name track-name
                                               :type track-type
                                               :external-id track-external}
                                         :dst {:id lyrics-id
                                               :name lyrics-name
                                               :type lyrics-type
                                               :external-id lyrics-id
                                               :media/sha256 sha
                                               :source lyrics}})
                (swap! updated inc)
                (if existed?
                  (swap! skipped inc)
                  (swap! linked inc))))))))
    (when (seq @missing-track)
      (println (format "Missing tracks: %d" (count @missing-track)))
      (doseq [{:keys [title slug]} @missing-track]
        (println (format "  - %s (%s)"
                         (or title "?")
                         (or slug "?")))))
    (when (pos? @missing-lyrics)
      (println (format "Missing lyrics text: %d" @missing-lyrics)))
    (println (format "Recovery summary: upserted=%d linked=%d skipped-links=%d"
                     @updated @linked @skipped))))

(defn -main [& args]
  (let [{:keys [dry-run? paths]} (parse-args args)
        paths (if (seq paths) paths default-recovery-paths)
        resolved (->> paths
                      (map resolve-path)
                      (remove nil?)
                      vec)
        missing (remove #(some #{(resolve-path %)} resolved) paths)]
    (when (seq missing)
      (println (format "Missing recovery files: %s" (str/join ", " missing))))
    (when (empty? resolved)
      (println "No recovery files found.")
      (System/exit 1))
    (let [profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)
          tracks (vec (track-entities @conn))]
      (try
        (doseq [path resolved]
          (recover-file! conn env tracks {:path path :dry-run? dry-run?}))
        (when-not dry-run?
          (xt/sync-node!))
        (finally
          (store-manager/shutdown!))))))
