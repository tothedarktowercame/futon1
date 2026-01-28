;; scripts/open_world_repair_entities_xtdb.clj
(ns scripts.open-world-repair-entities-xtdb
  "Scan XTDB for open-world entities missing required fields, backfill, and optionally delete."
  (:require [app.charon-guard :as charon-guard]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb])
  (:import (java.time Instant)))

(def ^:private required-keys
  [:entity/id :entity/label :entity/lower-label :entity/kind :entity/first-seen :entity/updated-at])

(defn- usage []
  (str/join
   \newline
   ["Usage: clojure -M -m scripts.open-world-repair-entities-xtdb [options]"
    ""
    "Options:"
    "  --apply               Write updates (default: dry-run)"
    "  --dry-run             Report updates without applying (default)"
    "  --delete-orphans      Delete unresolved entities with no open-world links"
    "  --delete-with-links   Delete unresolved entities and their open-world mentions/relations"
    "  --limit <n>           Limit detailed output (default: 50)"
    "  --out <path>          Write EDN report"
    "  --help                Show this message"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:apply? false
               :delete-orphans? false
               :delete-with-links? false
               :limit 50
               :out nil}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--apply" arg) (recur (rest args) (assoc opts :apply? true))
          (= "--dry-run" arg) (recur (rest args) (assoc opts :apply? false))
          (= "--delete-orphans" arg) (recur (rest args) (assoc opts :delete-orphans? true))
          (= "--delete-with-links" arg) (recur (rest args) (assoc opts :delete-with-links? true))
          (= "--limit" arg) (recur (nnext args) (assoc opts :limit (Long/parseLong (second args))))
          (= "--out" arg) (recur (nnext args) (assoc opts :out (second args)))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

(defn- resolve-config-path [path]
  (let [candidate (io/file path)]
    (cond
      (.exists candidate) (.getAbsolutePath candidate)
      :else (when-let [root (repo-root)]
              (let [alt (io/file root path)]
                (when (.exists alt)
                  (.getAbsolutePath alt)))))))

(defn- profile-data-dir []
  (let [{:keys [data-root]} (store-manager/config)
        profile (store-manager/default-profile)
        profile-id (if (= :me profile) (store-manager/default-profile) profile)]
    (str (io/file data-root (str profile-id)))))

(defn- cli-penholder [env]
  (or (:penholder env)
      (System/getenv "MODEL_PENHOLDER")
      (System/getenv "BASIC_CHAT_PENHOLDER")
      "cli"))

(defn- start-xt! []
  (let [{:keys [xtdb]} (store-manager/config)
        cfg-path (or (resolve-config-path (:config-path xtdb "apps/graph-memory/resources/xtdb.edn"))
                     (throw (ex-info "XTDB config file not found" {:path (:config-path xtdb)})))
        data-dir (profile-data-dir)]
    (xt/start! cfg-path {:data-dir (str (io/file data-dir "xtdb"))
                         :xt/created-by "scripts.open-world-repair-entities-xtdb"})))

(defn- missing? [value]
  (cond
    (nil? value) true
    (string? value) (str/blank? value)
    (sequential? value) (empty? value)
    :else false))

(defn- missing-fields [doc]
  (->> required-keys
       (filter #(missing? (get doc %)))
       vec))

(defn- normalize-label [value]
  (when-not (missing? value)
    (-> value str str/trim not-empty)))

(defn- canonical-kind [value]
  (cond
    (keyword? value) value
    (string? value) (let [normalized (-> value
                                         str/lower-case
                                         (str/replace #"[^a-z0-9]+" "-")
                                         (str/replace #"^-+" "")
                                         (str/replace #"-+$" ""))]
                      (if (str/blank? normalized) nil (keyword normalized)))
    :else nil))

(defn- ->instant [value]
  (cond
    (nil? value) nil
    (instance? Instant value) value
    (integer? value) (Instant/ofEpochMilli (long value))
    (number? value) (Instant/ofEpochMilli (long value))
    :else nil))

(defn- backfill-doc [doc now]
  (let [label (or (normalize-label (:entity/label doc))
                  (normalize-label (:entity/name doc))
                  (normalize-label (:entity/external-id doc)))
        kind (or (canonical-kind (:entity/kind doc))
                 (canonical-kind (:entity/type doc)))
        first-seen (or (->instant (:entity/first-seen doc))
                       (->instant (:entity/updated-at doc))
                       (->instant (:entity/last-seen doc))
                       now)
        updated-at (or (->instant (:entity/updated-at doc))
                       (->instant (:entity/last-seen doc))
                       now)
        updates (cond-> {}
                  (and (missing? (:entity/label doc)) label)
                  (assoc :entity/label label)

                  (and (missing? (:entity/lower-label doc)) label)
                  (assoc :entity/lower-label (str/lower-case label))

                  (and (missing? (:entity/kind doc)) kind)
                  (assoc :entity/kind kind)

                  (missing? (:entity/first-seen doc))
                  (assoc :entity/first-seen first-seen)

                  (missing? (:entity/updated-at doc))
                  (assoc :entity/updated-at updated-at))
        doc' (merge doc updates)]
    {:doc doc'
     :updates updates}))

(defn- open-world-entity-ids [db]
  (let [mention-ids (->> (xtdb/q db '{:find [?eid]
                                     :where [[?m :mention/text _]
                                             [?m :mention/entity ?eid]]})
                         (map first))
        relation-ids (->> (xtdb/q db '{:find [?eid]
                                      :where [[?r :relation/label _]
                                              [?r :relation/src ?eid]]})
                          (map first))
        relation-ids' (->> (xtdb/q db '{:find [?eid]
                                       :where [[?r :relation/label _]
                                               [?r :relation/dst ?eid]]})
                           (map first))
        labeled-ids (let [queries ['{:find [?eid]
                                    :where [[?e :entity/id ?eid]
                                            [?e :entity/label _]]}
                                   '{:find [?eid]
                                     :where [[?e :entity/id ?eid]
                                             [?e :entity/lower-label _]]}
                                   '{:find [?eid]
                                     :where [[?e :entity/id ?eid]
                                             [?e :entity/kind _]]}
                                   '{:find [?eid]
                                     :where [[?e :entity/id ?eid]
                                             [?e :entity/first-seen _]]}
                                   '{:find [?eid]
                                     :where [[?e :entity/id ?eid]
                                             [?e :entity/updated-at _]]}]]
                      (->> queries
                           (mapcat #(xtdb/q db %))
                           (map first)))]
    (->> (concat mention-ids relation-ids relation-ids' labeled-ids)
         (remove nil?)
         set)))

(defn- mention-ids-for [db eid]
  (->> (xtdb/q db '{:find [?mid]
                   :in [?eid]
                   :where [[?m :mention/entity ?eid]
                           [?m :mention/id ?mid]]}
               eid)
       (map first)
       set))

(defn- relation-ids-for [db eid]
  (let [src (xtdb/q db '{:find [?rid]
                        :in [?eid]
                        :where [[?r :relation/label _]
                                [?r :relation/src ?eid]
                                [?r :relation/id ?rid]]}
                     eid)
        dst (xtdb/q db '{:find [?rid]
                        :in [?eid]
                        :where [[?r :relation/label _]
                                [?r :relation/dst ?eid]
                                [?r :relation/id ?rid]]}
                     eid)]
    (->> (concat src dst)
         (map first)
         set)))

(defn -main [& args]
  (let [{:keys [apply? delete-orphans? delete-with-links? limit out]} (parse-args args)
        _ (start-xt!)
        conn (store-manager/conn (store-manager/default-profile))
        env (let [base (store-manager/env (store-manager/default-profile))]
              (assoc base :penholder (cli-penholder base)))
        _ (charon-guard/guard-models! conn [:open-world-ingest] env :open-world/repair)
        now (Instant/now)]
    (try
      (let [db (xt/db)
            ids (open-world-entity-ids db)
            docs (->> ids (map #(xtdb/entity db %)) (remove nil?) vec)
            missing (filter #(seq (missing-fields %)) docs)
            results (mapv (fn [doc]
                            (let [missing-before (missing-fields doc)
                                  {:keys [doc updates]} (backfill-doc doc now)
                                  missing-after (missing-fields doc)
                                  eid (:entity/id doc)
                                  mention-ids (mention-ids-for db eid)
                                  relation-ids (relation-ids-for db eid)
                                  refs {:mentions (count mention-ids)
                                        :relations (count relation-ids)}]
                              {:entity-id eid
                               :missing-before missing-before
                               :missing-after missing-after
                               :updates updates
                               :refs refs
                               :doc doc
                               :mention-ids mention-ids
                               :relation-ids relation-ids}))
                          missing)
            resolvable (filter #(empty? (:missing-after %)) results)
            unresolved (filter #(seq (:missing-after %)) results)]
        (println "Open-world entities with missing required fields:" (count results))
        (doseq [{:keys [entity-id missing-before missing-after]} (take limit results)]
          (println "missing:" entity-id "before" missing-before "after" missing-after))
        (when apply?
          (doseq [{:keys [doc entity-id updates]} resolvable]
            (when (seq updates)
              (xt/put-entity! (assoc doc :xt/id entity-id)))))
        (when (and apply? (seq unresolved))
          (doseq [{:keys [entity-id missing-after mention-ids relation-ids]} unresolved]
            (cond
              delete-with-links?
              (do
                (doseq [mid mention-ids]
                  (xt/delete-doc! mid))
                (doseq [rid relation-ids]
                  (xt/delete-doc! rid))
                (xt/delete-entity! entity-id)
                (println "deleted with links:" entity-id "missing" missing-after))

              delete-orphans?
              (when (and (empty? mention-ids) (empty? relation-ids))
                (xt/delete-entity! entity-id)
                (println "deleted orphan:" entity-id "missing" missing-after)))))
        (when out
          (spit out (pr-str {:ts now
                             :apply? apply?
                             :delete-orphans? delete-orphans?
                             :delete-with-links? delete-with-links?
                             :count (count results)
                             :resolvable (count resolvable)
                             :unresolved (count unresolved)
                             :results (map #(dissoc % :doc) results)})))
        (println "Done."))
      (finally
        (xt/stop!)))))
