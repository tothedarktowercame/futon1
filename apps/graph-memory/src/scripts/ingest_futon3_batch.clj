(ns scripts.ingest-futon3-batch
  "Ingest Futon3 patterns with checkpointed progress."
  (:require [app.store-manager :as store-manager]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scripts.futon3-ingest :as ingest]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M -m scripts.ingest-futon3-batch [--root PATH] [--checkpoint PATH] [--resume] [--limit N]"
             "  --root PATH       Override Futon3 root"
             "  --checkpoint PATH Progress checkpoint (EDN)"
             "  --resume          Resume from checkpoint"
             "  --limit N         Ingest at most N patterns"]))

(defn- parse-args [args]
  (loop [args args
         opts {:root nil :checkpoint nil :resume? false :limit nil}]
    (if (seq args)
      (case (first args)
        "--root" (recur (nnext args) (assoc opts :root (second args)))
        "--checkpoint" (recur (nnext args) (assoc opts :checkpoint (second args)))
        "--resume" (recur (rest args) (assoc opts :resume? true))
        "--limit" (recur (nnext args) (assoc opts :limit (some-> (second args) Long/parseLong)))
        "--help" (do (println (usage)) (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- read-checkpoint [path]
  (when (and path (.exists (io/file path)))
    (with-open [r (io/reader path)]
      (edn/read (java.io.PushbackReader. r)))))

(defn- write-checkpoint [path payload]
  (when path
    (io/make-parents path)
    (spit path (pr-str payload))))

(defn -main [& args]
  (let [{:keys [root checkpoint resume? limit]} (parse-args args)
        ckpt-path (or checkpoint
                      (str (io/file "data" "logs" "ingest-futon3.edn")))
        resolved-root (ingest/resolve-root* root)
        patterns (ingest/parse-patterns* resolved-root)
        slugs (mapv :id patterns)
        ckpt (when resume? (read-checkpoint ckpt-path))
        pending (vec (or (:pending ckpt) slugs))
        pending (if (and limit (pos? limit))
                  (vec (take (long limit) pending))
                  pending)
        pattern-by-id (into {} (map (juxt :id identity)) patterns)
        sm-cfg (store-manager/configure! {})
        profile (or (:default-profile sm-cfg)
                    (store-manager/default-profile))
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (try
      (println (format "Ingesting %d patterns (profile %s)" (count pending) profile))
      (loop [remaining pending
             processed (vec (or (:processed ckpt) []))
             skipped (vec (or (:skipped ckpt) []))]
        (if-let [slug (first remaining)]
          (if-let [pattern (get pattern-by-id slug)]
            (do
              (ingest/ingest-patterns* conn env [pattern])
              (let [processed' (conj processed slug)
                    remaining' (vec (rest remaining))]
                (write-checkpoint ckpt-path {:pending remaining'
                                             :processed processed'
                                             :skipped skipped
                                             :updated-at (System/currentTimeMillis)})
                (recur remaining' processed' skipped)))
            (let [skipped' (conj skipped slug)
                  remaining' (vec (rest remaining))]
              (println (format "  %s: missing in Futon3 sources" slug))
              (write-checkpoint ckpt-path {:pending remaining'
                                           :processed processed
                                           :skipped skipped'
                                           :updated-at (System/currentTimeMillis)})
              (recur remaining' processed skipped')))
          (do
            (println "Ingest complete.")
            (write-checkpoint ckpt-path {:pending []
                                         :processed processed
                                         :skipped skipped
                                         :updated-at (System/currentTimeMillis)}))))
      (finally
        (store-manager/shutdown!)))))
