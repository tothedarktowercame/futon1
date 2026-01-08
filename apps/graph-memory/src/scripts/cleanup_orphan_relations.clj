(ns scripts.cleanup-orphan-relations
  (:require [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:gen-class))

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.cleanup-orphan-relations [options] <relation-id>..."
    ""
    "Options:"
    "  --data-dir <path>   XTDB data directory (defaults to env BASIC_CHAT_DATA_DIR)"
    "  --config <path>     XTDB config edn (default: apps/graph-memory/resources/xtdb.edn)"
    "  --scan              Scan all relations for orphans"
    "  --delete            Delete orphan relations (default: dry-run)"
    "  --dry-run           Only report orphans (default)"
    "  --limit <n>         Limit detailed orphan output when scanning"
    ""]))

(defn- parse-args
  [args]
  (loop [args args
         opts {:ids []}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--data-dir" arg) (recur (nnext args) (assoc opts :data-dir (second args)))
          (= "--config" arg) (recur (nnext args) (assoc opts :config (second args)))
          (= "--scan" arg) (recur (rest args) (assoc opts :scan? true))
          (= "--delete" arg) (recur (rest args) (assoc opts :delete? true))
          (= "--dry-run" arg) (recur (rest args) (assoc opts :delete? false))
          (= "--limit" arg) (recur (nnext args) (assoc opts :limit (Long/parseLong (second args))))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (recur (rest args) (update opts :ids conj arg)))))))

(defn- orphan?
  [db rel-doc]
  (let [src (:relation/src rel-doc)
        dst (:relation/dst rel-doc)
        src-doc (when src (xtdb/entity db src))
        dst-doc (when dst (xtdb/entity db dst))
        hydratable? (fn [doc]
                      (when doc
                        (or (some? (:entity/name doc))
                            (some? (:entity/type doc))
                            (some? (:entity/last-seen doc))
                            (some? (:entity/seen-count doc))
                            (contains? doc :entity/pinned?)
                            (some? (:entity/external-id doc))
                            (some? (:entity/source doc)))))]
    (or (nil? src)
        (nil? dst)
        (nil? src-doc)
        (nil? dst-doc)
        (not (hydratable? src-doc))
        (not (hydratable? dst-doc)))))

(defn- find-by-relation-id
  [db rid]
  (when-let [eid (ffirst (xtdb/q db '{:find [e]
                                     :in [?rid]
                                     :where [[e :relation/id ?rid]]}
                          rid))]
    (xtdb/entity db eid)))

(defn- summarize
  [db rel-doc]
  {:relation/id (:relation/id rel-doc)
   :relation/type (:relation/type rel-doc)
   :relation/src (:relation/src rel-doc)
   :relation/dst (:relation/dst rel-doc)
   :src-exists? (boolean (when-let [src (:relation/src rel-doc)]
                           (xtdb/entity db src)))
   :dst-exists? (boolean (when-let [dst (:relation/dst rel-doc)]
                           (xtdb/entity db dst)))})

(defn- all-relation-docs
  [db]
  (->> (xtdb/q db '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst])]
                   :where [[?r :relation/id _]]})
       (map first)))

(defn- default-config []
  (let [cwd (System/getProperty "user.dir")
        local (str cwd "/resources/xtdb.edn")
        repo (str cwd "/apps/graph-memory/resources/xtdb.edn")]
    (cond
      (.exists (io/file local)) local
      (.exists (io/file repo)) repo
      :else local)))

(defn -main [& args]
  (let [{:keys [ids data-dir config delete? scan? limit]} (parse-args args)
        data-dir (or data-dir (System/getenv "BASIC_CHAT_DATA_DIR"))
        config (or config (default-config))]
    (when (and (empty? ids) (not scan?))
      (println (usage))
      (System/exit 1))
    (when-not data-dir
      (throw (ex-info "BASIC_CHAT_DATA_DIR or --data-dir required" {})))
    (println "Using XTDB data dir:" data-dir)
    (println "Using XTDB config:" config)
    (xt/start! config {:data-dir data-dir
                       :xt/created-by "scripts.cleanup-orphan-relations"})
    (try
      (let [db (xt/db)
            missing (atom [])
            orphans (atom [])]
        (when scan?
          (doseq [doc (all-relation-docs db)]
            (when (orphan? db doc)
              (swap! orphans conj doc)))
          (println "scan-relations:" (count (all-relation-docs db)))
          (println "orphan-count:" (count @orphans))
          (doseq [doc (take (or limit 50) @orphans)]
            (println "orphan:" (summarize db doc))))
        (when (seq ids)
          (doseq [id ids]
            (if-let [doc (or (xtdb/entity db id)
                             (find-by-relation-id db id))]
              (if (orphan? db doc)
                (do
                  (swap! orphans conj doc)
                  (println "orphan:" (summarize db doc)))
                (println "ok:" (summarize db doc)))
              (do
                (swap! missing conj id)
                (println "missing:" id)))))
        (when (seq @missing)
          (println "missing-count:" (count @missing)))
        (when (and delete? (seq @orphans))
          (doseq [doc @orphans]
            (xt/delete-rel! (:relation/id doc)))
          (println "deleted-orphans:" (count @orphans))))
      (finally
        (xt/stop!)))))
