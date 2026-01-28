;; apps/graph-memory/src/scripts/sigil_allowlist_repair.clj
(ns scripts.sigil-allowlist-repair
  "Remove Futon3 sigil entities that are not in the allowlist."
  (:require [app.sigil-allowlist :as sigil-allowlist]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private futon3-sigil-source "futon3/sigil")

(defn- usage []
  (str "Usage: clojure -M -m scripts.sigil-allowlist-repair [--apply] [--root PATH]\n"
       "  --apply   Delete invalid sigil entities (default is dry-run)\n"
       "  --root    FUTON3 root for allowlist (default: auto-detect)"))

(defn- parse-args [args]
  (loop [args args opts {:apply? false :root nil}]
    (if (seq args)
      (case (first args)
        "--apply" (recur (rest args) (assoc opts :apply? true))
        "--root" (if-let [value (second args)]
                   (recur (nnext args) (assoc opts :root value))
                   (throw (ex-info "Missing value for --root" {:args args})))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- sigil-entities [db]
  (d/q '[:find ?id ?name ?external ?source
         :where
         [?e :entity/type :sigil]
         [?e :entity/id ?id]
         [?e :entity/name ?name]
         [(get-else $ ?e :entity/external-id "") ?external]
         [(get-else $ ?e :entity/source "") ?source]]
       db))

(defn- normalize-part [value]
  (let [raw (some-> value str/trim)]
    (when (and (seq raw) (not= raw "?"))
      raw)))

(defn- parse-sigil [name external]
  (let [from-external (when (seq external) (str/split external #"\|" 2))
        from-name (when (seq name) (str/split name #"/" 2))
        [emoji hanzi] (cond
                        (and from-external (= 2 (count from-external))) from-external
                        (and from-name (= 2 (count from-name))) from-name
                        :else [nil nil])]
    {:emoji (normalize-part emoji)
     :hanzi (normalize-part hanzi)}))

(defn -main [& args]
  (let [{:keys [apply? root]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)
        env* (dissoc env :verify-fn)]
    (try
      (let [allowlist (sigil-allowlist/allowlist-from-root root)
            invalid (->> (sigil-entities @conn)
                         (filter (fn [[_ _ _ source]]
                                   (= source futon3-sigil-source)))
                         (keep (fn [[id name external _]]
                                 (let [sigil (parse-sigil name external)]
                                   (when-not (sigil-allowlist/sigil-allowed? allowlist sigil)
                                     {:id id
                                      :name name
                                      :external external
                                      :emoji (:emoji sigil)
                                      :hanzi (:hanzi sigil)}))))
                         vec)]
        (println (format "Invalid Futon3 sigils found: %d" (count invalid)))
        (doseq [entry (take 20 invalid)]
          (println " " entry))
        (when apply?
          (doseq [{:keys [id]} invalid]
            (store/forget-entity! conn env* {:id id}))
          (println (format "Deleted %d sigil entities." (count invalid)))))
      (finally
        (store-manager/shutdown!)))))
