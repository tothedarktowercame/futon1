(ns scripts.ingest-futon3-batch
  "Ingest Futon3 patterns with checkpointed progress."
  (:require [app.store-manager :as store-manager]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scripts.futon3-ingest :as ingest]
            [scripts.rehydrate :as rehydrate]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M -m scripts.ingest-futon3-batch [--root PATH] [--checkpoint PATH] [--resume] [--limit N]"
             "  --root PATH       Override Futon3 root"
             "  --checkpoint PATH Progress checkpoint (EDN)"
             "  --resume          Resume from checkpoint"
             "  --limit N         Ingest at most N patterns"
             "  --rehydrate       POST to the live rehydrate endpoint after ingest"
             "  --rehydrate-url   Override rehydrate URL"]))

(defn- parse-args [args]
  (loop [args args
         opts {:root nil :checkpoint nil :resume? false :limit nil
               :rehydrate? nil :rehydrate-url nil}]
    (if (seq args)
      (case (first args)
        "--root" (recur (nnext args) (assoc opts :root (second args)))
        "--checkpoint" (recur (nnext args) (assoc opts :checkpoint (second args)))
        "--resume" (recur (rest args) (assoc opts :resume? true))
        "--limit" (recur (nnext args) (assoc opts :limit (some-> (second args) Long/parseLong)))
        "--rehydrate" (recur (rest args) (assoc opts :rehydrate? true))
        "--rehydrate-url" (recur (nnext args) (assoc opts :rehydrate? true :rehydrate-url (second args)))
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

(defn- ingest-one [conn env pattern]
  (let [result (ingest/ingest-patterns* conn env [pattern])]
    (if (false? (:ok? result))
      {:ok? false :error result}
      {:ok? true})))

(defn -main [& args]
  (let [{:keys [root checkpoint resume? limit rehydrate? rehydrate-url]} (parse-args args)
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
        env (assoc (store-manager/env profile)
                   :verify? true
                   :penholder "charon")
        remaining* (atom pending)
        processed* (atom (vec (or (:processed ckpt) [])))
        skipped* (atom (vec (or (:skipped ckpt) [])))]
    (try
      (println (format "Ingesting %d patterns (profile %s)" (count pending) profile))
      (while (seq @remaining*)
        (let [slug (first @remaining*)
              rest-remaining (vec (rest @remaining*))]
          (if-let [pattern (get pattern-by-id slug)]
            (let [{:keys [ok? error]} (ingest-one conn env pattern)]
              (if ok?
                (do
                  (swap! processed* conj slug)
                  (reset! remaining* rest-remaining)
                  (write-checkpoint ckpt-path {:pending @remaining*
                                               :processed @processed*
                                               :skipped @skipped*
                                               :updated-at (System/currentTimeMillis)}))
                (do
                  (println (format "Ingest halted on %s. Resume with --resume once fixed." slug))
                  (when error
                    (binding [*out* *err*]
                      (println (pr-str error))))
                  (write-checkpoint ckpt-path {:pending @remaining*
                                               :processed @processed*
                                               :skipped @skipped*
                                               :updated-at (System/currentTimeMillis)})
                  (throw (ex-info "Pattern ingest failed" {:pattern slug
                                                         :error error})))))
            (do
              (println (format "  %s: missing in Futon3 sources" slug))
              (swap! skipped* conj slug)
              (reset! remaining* rest-remaining)
              (write-checkpoint ckpt-path {:pending @remaining*
                                           :processed @processed*
                                           :skipped @skipped*
                                           :updated-at (System/currentTimeMillis)})))))
      (println "Ingest complete.")
      (write-checkpoint ckpt-path {:pending []
                                   :processed @processed*
                                   :skipped @skipped*
                                   :updated-at (System/currentTimeMillis)})
      (rehydrate/maybe-rehydrate! {:rehydrate? rehydrate?
                                   :rehydrate-url rehydrate-url})
      (finally
        (store-manager/shutdown!)))))
