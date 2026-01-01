(ns scripts.open-world-type-counts
  "Report top-N open-world relation type counts from the Datascript cache."
  (:require [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private default-top [10 20 50 100])

(defn- usage []
  (str "Usage: clojure -M -m scripts.open-world-type-counts [--out PATH] [--all]\n"
       "  --out PATH   Write EDN results to PATH\n"
       "  --all        Include namespaced relation types (default filters to unnamespaced)\n"))

(defn- parse-args [args]
  (loop [args args opts {:all? false}]
    (if (seq args)
      (case (first args)
        "--out" (if-let [path (second args)]
                  (recur (nnext args) (assoc opts :out path))
                  (throw (ex-info "Missing path after --out" {:args args})))
        "--all" (recur (rest args) (assoc opts :all? true))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- keyword-name [k]
  (when k
    (if-let [ns (namespace k)]
      (str ns "/" (name k))
      (name k))))

(defn- relation-type-counts [db]
  (->> (d/q '[:find ?type (count ?r)
              :where
              [?r :relation/type ?type]]
            db)
       (map (fn [[type count]] {:type type :count count}))
       (remove (fn [{:keys [type]}] (nil? type)))))

(defn- open-world-type? [type]
  (and (keyword? type)
       (nil? (namespace type))))

(defn- prepare-results [counts all?]
  (let [filtered (if all?
                   counts
                   (filter (fn [{:keys [type]}] (open-world-type? type)) counts))
        sorted (->> filtered
                    (sort-by (juxt (comp - :count)
                                   (comp keyword-name :type))))]
    {:total (count sorted)
     :top (into {}
                (map (fn [n] [(keyword (str "top-" n)) (vec (take n sorted))]))
                default-top)}))

(defn -main [& args]
  (let [{:keys [out all?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        db @conn
        counts (relation-type-counts db)
        result (prepare-results counts all?)]
    (try
      (prn result)
      (when out
        (spit out (pr-str result))
        (println (str "Wrote results to " out)))
      (finally
        (store-manager/shutdown!)))))
