;; apps/graph-memory/src/scripts/open_world_repair_missing_relations.clj
(ns scripts.open-world-repair-missing-relations
  "Remove open-world relations that reference missing entity endpoints."
  (:require [app.config :as config]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M -m scripts.open-world-repair-missing-relations [--apply]"
             "  --apply    Delete invalid relations (default is dry-run)"]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false}]
    (if (seq args)
      (case (first args)
        "--apply" (recur (rest args) (assoc opts :apply? true))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- xtdb-config-path []
  (some-> (io/resource "xtdb.edn") io/file .getAbsolutePath))

(defn- data-dir []
  (let [root (:app/data-dir (config/config))
        base (if (and root (not (.isAbsolute (io/file root))))
               (let [repo (loop [dir (io/file (System/getProperty "user.dir"))]
                            (when dir
                              (if (.exists (io/file dir "AGENTS.md"))
                                dir
                                (recur (.getParentFile dir)))))]
                 (if repo
                   (.getAbsolutePath (io/file repo root))
                   (.getAbsolutePath (io/file root))))
               root)]
    (when base
      (.getAbsolutePath (io/file base "xtdb")))))

(defn- missing-endpoints [db relation]
  (let [src (:relation/src relation)
        dst (:relation/dst relation)
        src-doc (when src (xtdb/entity db src))
        dst-doc (when dst (xtdb/entity db dst))
        missing (cond-> []
                  (and src (nil? src-doc)) (conj {:issue :missing-src :entity-id src})
                  (and dst (nil? dst-doc)) (conj {:issue :missing-dst :entity-id dst}))]
    (when (seq missing)
      {:relation-id (:relation/id relation)
       :missing missing})))

(defn- open-world-relations [db]
  (->> (xtdb/q db '{:find [(pull ?r [*])]
                   :where [[?r :relation/id _]
                           [?r :relation/label _]]})
       (map first)))

(defn -main [& args]
  (let [{:keys [apply?]} (parse-args args)
        cfg (xtdb-config-path)
        dir (data-dir)]
    (when-not cfg
      (throw (ex-info "XTDB config not found on classpath" {})))
    (xt/start! cfg {:data-dir dir :xt/created-by "scripts.open-world-repair-missing-relations"})
    (try
      (let [db (xtdb/db (xt/node))
            relations (open-world-relations db)
            invalid (->> relations
                         (keep #(missing-endpoints db %))
                         vec)]
        (println (format "Open-world relations checked: %d" (count relations)))
        (println (format "Invalid relations found: %d" (count invalid)))
        (doseq [entry (take 10 invalid)]
          (println "  " entry))
        (when apply?
          (doseq [{:keys [relation-id]} invalid]
            (xt/delete-rel! relation-id))
          (println (format "Deleted %d relations." (count invalid)))))
      (finally
        (xt/stop!)))))
