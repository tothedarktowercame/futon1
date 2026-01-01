(ns scripts.open-world-sigil-date-audit
  "Summarize open-world sigil/date entities, mentions, and relations from XTDB."
  (:require [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(def ^:private default-sample-size 10)

(defn- usage []
  (str "Usage: clojure -M -m scripts.open-world-sigil-date-audit [--out PATH] [--sample N]\n"
       "  --out PATH    Write EDN results to PATH\n"
       "  --sample N    Number of samples to include (default 10)\n"))

(defn- parse-args [args]
  (loop [args args opts {:sample default-sample-size}]
    (if (seq args)
      (case (first args)
        "--out" (if-let [path (second args)]
                  (recur (nnext args) (assoc opts :out path))
                  (throw (ex-info "Missing path after --out" {:args args})))
        "--sample" (if-let [raw (second args)]
                     (recur (nnext args) (assoc opts :sample (Integer/parseInt raw)))
                     (throw (ex-info "Missing number after --sample" {:args args})))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- profile-data-dir []
  (let [{:keys [data-root]} (store-manager/config)
        profile (store-manager/default-profile)
        profile-id (if (= :me profile) (store-manager/default-profile) profile)]
    (.getAbsolutePath (io/file data-root (str profile-id) "xtdb"))))

(defn- start-xt! []
  (let [cfg (store-manager/config)
        {:keys [xtdb]} cfg
        cfg-path (get-in cfg [:xtdb :config-path])
        data-dir (profile-data-dir)
        cfg-path (or cfg-path
                     (throw (ex-info "XTDB config path not found" {:config xtdb})))]
    (xt/start! cfg-path {:data-dir data-dir :xt/created-by "open-world-sigil-date-audit"})))

(defn- top-n [coll n]
  (->> coll (sort-by (juxt (comp - second) first)) (take n) vec))

(defn- type-name [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    :else (str t)))

(defn- entity-ids-by-type [db type-str]
  (let [from-type (->> (xtdb/q db '{:find [?eid ?type]
                                   :where [[?e :entity/id ?eid]
                                           [?e :entity/type ?type]]})
                       (filter (fn [[_ t]] (= type-str (type-name t))))
                       (map first))
        from-kind (->> (xtdb/q db '{:find [?eid ?kind]
                                   :where [[?e :entity/id ?eid]
                                           [?e :entity/kind ?kind]]})
                       (filter (fn [[_ k]] (= type-str (type-name k))))
                       (map first))]
    (vec (set (concat from-type from-kind)))))

(defn- entity-labels [db ids]
  (let [label-of (fn [doc]
                   (or (:entity/label doc)
                       (:entity/name doc)
                       (:entity/id doc)))]
    (->> ids
         (keep (fn [eid]
                 (when-let [doc (xtdb/entity db eid)]
                   {:entity-id eid
                    :label (label-of doc)})))
         vec)))

(defn- sample-entities [db ids sample-size]
  (->> ids
       (take sample-size)
       (keep (fn [eid]
               (when-let [doc (xtdb/entity db eid)]
                 (select-keys doc
                              [:entity/id
                               :entity/type
                               :entity/kind
                               :entity/name
                               :entity/label
                               :entity/lower-label
                               :entity/source]))))
       vec))

(defn- mention-sentences [db ids]
  (->> (xtdb/q db '{:find [?mid ?eid ?sentence]
                    :in [$ ?ids]
                    :where [[(contains? ?ids ?eid)]
                            [?m :mention/id ?mid]
                            [?m :mention/entity ?eid]
                            [?m :mention/sentence ?sentence]]}
                  ids)
       (map (fn [[mid eid sentence]]
              {:mention-id mid
               :entity-id eid
               :sentence sentence}))
       vec))

(defn- relation-rows [db ids]
  (->> (xtdb/q db '{:find [?rid ?rtype ?src ?dst]
                    :in [$ ?ids]
                    :where [[?r :relation/id ?rid]
                            [?r :relation/type ?rtype]
                            [?r :relation/src ?src]
                            [?r :relation/dst ?dst]
                            [(or (contains? ?ids ?src)
                                 (contains? ?ids ?dst))]]}
                  ids)
       (map (fn [[rid rtype src dst]]
              {:relation-id rid
               :relation-type rtype
               :src src
               :dst dst}))
       vec))

(defn- entity-type [db eid]
  (when eid
    (or (some-> (xtdb/entity db eid) :entity/type)
        (some-> (xtdb/entity db eid) :entity/kind))))

(defn- summarize-kind [db kind sample-size]
  (let [ids (entity-ids-by-type db (name kind))
        id-set (set ids)
        labels (entity-labels db id-set)
        label-counts (frequencies (map :label labels))
        samples (sample-entities db ids sample-size)
        mentions (mention-sentences db id-set)
        mention-count (count mentions)
        mention-samples (->> mentions (map :sentence) distinct (take sample-size) vec)
        relations (relation-rows db id-set)
        relation-type-counts (->> relations (map :relation-type) frequencies)
        rel-type-top (top-n relation-type-counts sample-size)
        other-type-counts (let [cache (atom {})]
                            (->> relations
                                 (map (fn [{:keys [src dst]}]
                                        (let [other (if (contains? id-set src) dst src)]
                                          (or (get @cache other)
                                              (let [t (entity-type db other)]
                                                (swap! cache assoc other t)
                                                t)))))
                                 (remove nil?)
                                 frequencies))
        other-type-top (top-n other-type-counts sample-size)]
    {:kind kind
     :entity-count (count ids)
     :sample-entities samples
     :label-top (top-n label-counts sample-size)
     :mention-count mention-count
     :mention-samples mention-samples
     :relation-type-top rel-type-top
     :linked-entity-type-top other-type-top}))

(defn -main [& args]
  (let [{:keys [out sample]} (parse-args args)]
    (start-xt!)
    (try
      (let [db (xtdb/db (xt/node))
            payload {:generated-at (System/currentTimeMillis)
                     :sample-size sample
                     :sigil (summarize-kind db :sigil sample)
                     :date (summarize-kind db :date sample)}]
        (prn payload)
        (when out
          (spit out (pr-str payload))
          (println (str "Wrote results to " out))))
      (finally
        (xt/stop!)))))
