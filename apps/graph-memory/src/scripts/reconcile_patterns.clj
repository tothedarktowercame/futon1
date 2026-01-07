(ns scripts.reconcile-patterns
  "Remove stale pattern/includes relations and extra components based on FUTON3 sources."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [scripts.futon3-ingest :as ingest]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M -m scripts.reconcile-patterns [--root PATH] [--checkpoint PATH] [--resume] [--limit N]"
             "  --root PATH       Override Futon3 root"
             "  --checkpoint PATH Progress checkpoint (EDN)"
             "  --resume          Resume from checkpoint"
             "  --limit N         Process at most N patterns"])) 

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

(defn- component-name [pattern-name {:keys [order kind]}]
  (format "%s/%02d-%s" pattern-name order (or kind "section")))

(defn- pattern-entity [db slug]
  (ffirst
   (d/q '[:find (pull ?e [:db/id :entity/id :entity/name])
          :in $ ?name
          :where
          [?e :entity/name ?name]
          [?e :entity/type :pattern/library]]
        db slug)))

(defn- include-relations [db pattern-db-id]
  (map first
       (d/q '[:find (pull ?rel [:relation/id {:relation/dst [:entity/name]}])
              :in $ ?pattern
              :where
              [?rel :relation/src ?pattern]
              [?rel :relation/type :pattern/includes]
              [?rel :relation/dst ?dst]]
            db pattern-db-id)))

(defn- components-by-prefix [db prefix]
  (map first
       (d/q '[:find (pull ?e [:entity/id :entity/name])
              :in $ ?prefix
              :where
              [?e :entity/type :pattern/component]
              [?e :entity/name ?name]
              [(clojure.string/starts-with? ?name ?prefix)]]
            db prefix)))

(defn -main [& args]
  (let [{:keys [root checkpoint resume? limit]} (parse-args args)
        ckpt-path (or checkpoint
                      (str (io/file "data" "logs" "pattern-reconcile.edn")))
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
      (println (format "Reconciling %d patterns (profile %s)" (count pending) profile))
      (loop [remaining pending
             processed (vec (or (:processed ckpt) []))
             skipped (vec (or (:skipped ckpt) []))]
        (if-let [slug (first remaining)]
          (let [pattern (get pattern-by-id slug)
                expected (when pattern
                           (->> (:components pattern)
                                (map #(component-name slug %))
                                set))
                db @conn
                entity (when pattern (pattern-entity db slug))]
            (if (and pattern entity)
              (let [prefix (str slug "/")
                    rels (include-relations db (:db/id entity))
                    bad-includes (filter (fn [rel]
                                           (let [name (get-in rel [:relation/dst :entity/name])]
                                             (and name (not (contains? expected name)))))
                                         rels)
                    components (components-by-prefix db prefix)
                    extra-components (filter (fn [component]
                                               (let [name (:entity/name component)]
                                                 (and name (not (contains? expected name)))))
                                             components)]
                (doseq [rel bad-includes
                        :let [rel-id (:relation/id rel)]]
                  (when rel-id
                    (store/delete-relation! conn env rel-id)))
                (doseq [component extra-components
                        :let [id (:entity/id component)]]
                  (when id
                    (store/forget-entity! conn env {:id id})))
                (println (format "  %s: removed %d includes, %d components"
                                 slug (count bad-includes) (count extra-components)))
                (let [processed' (conj processed slug)
                      remaining' (vec (rest remaining))]
                  (write-checkpoint ckpt-path {:pending remaining'
                                               :processed processed'
                                               :skipped skipped
                                               :updated-at (System/currentTimeMillis)})
                  (recur remaining' processed' skipped)))
              (let [skipped' (conj skipped slug)
                    remaining' (vec (rest remaining))]
                (when (nil? pattern)
                  (println (format "  %s: missing in Futon3 sources" slug)))
                (when (and pattern (nil? entity))
                  (println (format "  %s: missing in store" slug)))
                (write-checkpoint ckpt-path {:pending remaining'
                                             :processed processed
                                             :skipped skipped'
                                             :updated-at (System/currentTimeMillis)})
                (recur remaining' processed skipped'))))
          (do
            (println "Reconcile complete.")
            (write-checkpoint ckpt-path {:pending []
                                         :processed processed
                                         :skipped skipped
                                         :updated-at (System/currentTimeMillis)}))))
      (finally
        (store-manager/shutdown!)))))
