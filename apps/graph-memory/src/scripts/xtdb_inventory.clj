(ns scripts.xtdb-inventory
  "Inventory XTDB entity types and docbook docs."
  (:require [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- usage []
  (str "Usage: clojure -M -m scripts.xtdb-inventory [--out PATH]\n"
       "  --out PATH    Write EDN results to PATH\n"))

(defn- parse-args [args]
  (loop [args args opts {}]
    (if (seq args)
      (case (first args)
        "--out" (if-let [path (second args)]
                  (recur (nnext args) (assoc opts :out path))
                  (throw (ex-info "Missing path after --out" {:args args})))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- repo-root []
  (loop [dir (java.io.File. (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (java.io.File. dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

(defn- resolve-config-path [path]
  (let [candidate (java.io.File. path)]
    (cond
      (.exists candidate) (.getAbsolutePath candidate)
      :else (when-let [root (repo-root)]
              (let [alt (java.io.File. root path)]
                (when (.exists alt)
                  (.getAbsolutePath alt)))))))

(defn- profile-data-dir []
  (let [{:keys [data-root]} (store-manager/config)
        profile (store-manager/default-profile)
        profile-id (if (= :me profile) (store-manager/default-profile) profile)]
    (str (java.io.File. data-root (str profile-id)))))

(defn- start-xt! []
  (let [{:keys [xtdb]} (store-manager/config)
        cfg-path (or (resolve-config-path (:config-path xtdb "apps/graph-memory/resources/xtdb.edn"))
                     (throw (ex-info "XTDB config file not found" {:path (:config-path xtdb)})))
        data-dir (profile-data-dir)]
    (xt/start! cfg-path {:data-dir (when data-dir (str (java.io.File. data-dir "xtdb")))
                         :xt/created-by "scripts.xtdb-inventory"})))

(defn- keyword-name [k]
  (when k
    (if (keyword? k)
      (if-let [ns (namespace k)]
        (str ns "/" (name k))
        (name k))
      (str k))))

(defn- pattern-type? [t]
  (str/starts-with? (keyword-name t) "pattern/"))

(defn- model-descriptor? [t]
  (= "model/descriptor" (keyword-name t)))

(defn- entity-type-counts [db]
  (->> (xtdb/q db '{:find [?type (count ?e)]
                    :where [[?e :entity/type ?type]]})
       (map (fn [[t n]] {:type t :count n}))
       (sort-by (juxt #(keyword-name (:type %)) :count))))

(defn- docbook-heading-counts [db]
  (->> (xtdb/q db '{:find [?book (count ?h)]
                    :where [[?h :doc/id _]
                            [?h :doc/book ?book]
                            (not [?h :doc/entry-id _])
                            (not [?h :doc/toc-order _])]})
       (map (fn [[book n]] {:book book :count n}))
       (sort-by (juxt :book :count))))

(defn- docbook-entry-counts [db]
  (->> (xtdb/q db '{:find [?book (count ?e)]
                    :where [[?e :doc/entry-id _]
                            [?e :doc/book ?book]]})
       (map (fn [[book n]] {:book book :count n}))
       (sort-by (juxt :book :count))))

(defn -main [& args]
  (let [{:keys [out]} (parse-args args)]
    (start-xt!)
    (try
      (let [db (xt/db)
            entity-types (entity-type-counts db)
            others (->> entity-types
                        (remove (fn [{:keys [type]}]
                                  (or (pattern-type? type)
                                      (model-descriptor? type))))
                        vec)
            payload {:generated-at (System/currentTimeMillis)
                     :entity-types entity-types
                     :entity-types-others others
                     :docbook {:headings-by-book (docbook-heading-counts db)
                               :entries-by-book (docbook-entry-counts db)}}]
        (prn payload)
        (when out
          (spit out (pr-str payload))
          (println (str "Wrote results to " out))))
      (finally
        (xt/stop!)))))
