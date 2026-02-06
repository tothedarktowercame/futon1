;; apps/graph-memory/src/app/type_counts.clj
(ns app.type-counts
  "Helpers for reporting and comparing entity counts by type."
  (:require [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]))

(def ^:private baseline-filename "type-counts.edn")

(defn- baseline-path [metadata-root]
  (when-let [root (some-> metadata-root str str/trim not-empty)]
    (.getAbsolutePath (io/file root baseline-filename))))

(defn load-baseline
  [metadata-root]
  (when-let [path (baseline-path metadata-root)]
    (let [file (io/file path)]
      (when (.exists file)
        (try
          (with-open [r (io/reader file)]
            (binding [*read-eval* false]
              (edn/read (java.io.PushbackReader. r))))
          (catch Exception _
            nil))))))

(defn save-baseline!
  [metadata-root data-dir counts]
  (when-let [path (baseline-path metadata-root)]
    (let [file (io/file path)]
      (.mkdirs (.getParentFile file))
      (spit file
            (pr-str {:data-dir (some-> data-dir str)
                     :updated-at (System/currentTimeMillis)
                     :counts counts})))
    path))

(defn type-counts
  [conn]
  (let [db (d/db conn)
        total (or (d/q '[:find (count ?e) . :where [?e :entity/id _]] db) 0)
        typed (d/q '[:find ?t (count ?e) :where [?e :entity/type ?t]] db)
        by-type (into {} (map (fn [[t c]] [t c])) typed)
        typed-total (reduce + 0 (vals by-type))
        untyped (- total typed-total)]
    {:total total
     :by-type by-type
     :untyped (when (pos? untyped) untyped)}))

(defn durable-type-counts
  []
  (xt/entity-type-counts))

(defn compare-type-counts
  [baseline counts {:keys [data-dir]}]
  (cond
    (nil? baseline)
    {:ok? true :skipped? true :reason :baseline-missing}

    (and data-dir (:data-dir baseline) (not= (str data-dir) (str (:data-dir baseline))))
    {:ok? true :skipped? true :reason :data-dir-mismatch}

    :else
    (let [base (if (contains? baseline :counts) (:counts baseline) baseline)
          base-by (:by-type base)
          current-by (:by-type counts)
          failures (->> base-by
                        (keep (fn [[t b]]
                                (let [c (get current-by t 0)]
                                  (when (< c b)
                                    {:type t :baseline b :current c}))))
                        vec)
          failures (cond-> failures
                     (and (:total base) (< (:total counts) (:total base)))
                     (conj {:type :total
                            :baseline (:total base)
                            :current (:total counts)}))]
      {:ok? (empty? failures)
       :failures failures})))

(defn compare-counts-eq
  [left right]
  (cond
    (or (nil? left) (nil? right))
    {:ok? true :skipped? true :reason :counts-missing}

    :else
    (let [left-by (:by-type left)
          right-by (:by-type right)
          keys (into #{} (concat (keys left-by) (keys right-by)))
          failures (->> keys
                        (keep (fn [t]
                                (let [l (get left-by t 0)
                                      r (get right-by t 0)]
                                  (when (not= l r)
                                    {:type t :left l :right r}))))
                        vec)
          failures (cond-> failures
                     (not= (:total left) (:total right))
                     (conj {:type :total
                            :left (:total left)
                            :right (:total right)}))]
      {:ok? (empty? failures)
       :failures failures})))

(defn format-type-counts
  [counts]
  (let [by-type (:by-type counts)
        sorted (sort-by (fn [[t c]] [(- c) (str t)]) by-type)
        entries (map (fn [[t c]] (str t "=" c)) sorted)
        untyped (when (pos? (or (:untyped counts) 0)) (str ":untyped=" (:untyped counts)))
        total (str ":total=" (:total counts))
        parts (cond-> (vec entries)
                untyped (conj untyped)
                total (conj total))]
    (str/join " " parts)))

(defn format-type-count-failures
  [failures]
  (->> failures
       (map (fn [{:keys [type baseline current]}]
              (str type "=" current "<" baseline)))
       (str/join ", ")))

(defn format-counts-mismatch
  [failures]
  (->> failures
       (map (fn [{:keys [type left right]}]
              (str type "=" left "!=" right)))
       (str/join ", ")))
