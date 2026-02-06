;; scripts/media_source_scrub.clj
(ns scripts.media-source-scrub
  "Remove legacy :entity/source = \"external\" from media entities."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private track-type :arxana/media-track)
(def ^:private lyrics-type :arxana/media-lyrics)

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.media-source-scrub [--apply]"
    ""
    "Options:"
    "  --apply         Apply changes (default: dry-run)"
    ""
    "Environment:"
    "  ALPHA_PROFILE       Profile name (defaults to configured profile)"
    "  BASIC_CHAT_DATA_DIR Data root (required if server uses a non-default data dir)"]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--apply" arg) (recur (rest args) (assoc opts :apply? true))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- external-source? [value]
  (and (string? value)
       (= "external" (str/lower-case (str/trim value)))))

(defn- entities-with-external-source [db entity-type]
  (let [rows (d/q '[:find ?e ?id ?name ?source
                    :in $ ?t
                    :where
                    [?e :entity/type ?t]
                    [?e :entity/id ?id]
                    [?e :entity/name ?name]
                    [?e :entity/source ?source]]
                  db entity-type)]
    (->> rows
         (map (fn [[eid id name source]]
                {:db/id eid
                 :entity/id id
                 :entity/name name
                 :entity/source source
                 :entity/type entity-type}))
         (filter #(external-source? (:entity/source %)))
         vec)))

(defn -main [& args]
  (let [{:keys [apply?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        db @conn
        tracks (entities-with-external-source db track-type)
        lyrics (entities-with-external-source db lyrics-type)
        total (+ (count tracks) (count lyrics))
        txs (->> (concat tracks lyrics)
                 (map (fn [{db-id :db/id source :entity/source}]
                        [:db/retract db-id :entity/source source]))
                 vec)]
    (println (format "Media source scrub: tracks=%d lyrics=%d apply=%s"
                     (count tracks) (count lyrics) apply?))
    (doseq [{id :entity/id name :entity/name etype :entity/type} (concat tracks lyrics)]
      (println (format "  - %s (%s) %s" name id (clojure.core/name etype))))
    (when (and apply? (seq txs))
      (d/transact! conn txs)
      (let [sync-fut (future (#'store/sync-to-xtdb! conn))
            result (deref sync-fut 15000 :timeout)]
        (when (= result :timeout)
          (println "Warning: XTDB sync timed out after 15s; changes are still recorded in Datascript."))))
    (when (and apply? (zero? total))
      (println "No entities required changes."))
    (shutdown-agents)
    (System/exit 0)))
