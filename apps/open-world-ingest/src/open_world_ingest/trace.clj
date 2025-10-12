(ns open-world-ingest.trace
  (:refer-clojure :exclude [reset!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private default-trace-file "trace-openie.edn")

(def ^:private env-var "BASIC_CHAT_TRACE_OPENIE")

(defn- normalize-value
  [value]
  (cond
    (nil? value) nil
    (map? value)
    (when-let [path (:path value)]
      {:path (str path)})
    (true? value)
    {:path default-trace-file}
    (false? value)
    nil
    (string? value)
    (let [trimmed (-> value str str/trim)]
      (when (seq trimmed)
        {:path trimmed}))
    :else
    (normalize-value (str value))))

(defn- normalize-env
  [raw]
  (let [value (some-> raw str str/trim)]
    (cond
      (or (nil? value)
          (str/blank? value)
          (= value "0")
          (= (str/lower-case value) "false"))
      nil

      (= value "1")
      {:path default-trace-file}

      :else
      {:path value})))

(defn config
  "Derive the effective trace configuration from an override or the environment."
  ([] (config nil))
  ([override]
   (or (normalize-value override)
       (normalize-env (System/getenv env-var)))))

(defn reset!
  "Remove any existing trace output for a fresh recording session."
  [{:keys [path]}]
  (when path
    (let [file (io/file path)]
      (io/make-parents file)
      (when (.exists file)
        (.delete file)))))

(defn append!
  "Append one or more triple maps to the configured trace file."
  [{:keys [path]} triples]
  (when (and path (seq triples))
    (let [file (io/file path)]
      (io/make-parents file)
      (with-open [w (io/writer file :append true)]
        (doseq [triple triples]
          (.write w (pr-str triple))
          (.write w "\n"))))))

(defn load-triples
  "Load recorded triples (one EDN map per line) from the provided source."
  [source]
  (cond
    (nil? source) []
    (sequential? source) (mapv identity source)
    (map? source) (load-triples (:path source))
    :else
    (let [file (io/file (str source))]
      (if (.exists file)
        (with-open [r (io/reader file)]
          (->> (line-seq r)
               (remove str/blank?)
               (map edn/read-string)
               (mapv identity)))
        []))))

(defn index-by-sentence
  "Index recorded triples by [sentence-idx sentence-text] for quick replay lookups."
  [triples]
  (let [grouped (group-by (fn [triple]
                            [(:sent-idx triple)
                             (:sent triple)])
                          triples)
        base (reduce (fn [acc [[idx sent] group]]
                       (assoc acc [idx sent] (vec group)))
                     {}
                     grouped)
        any-by-idx (reduce (fn [acc [[idx _] group]]
                             (update acc [idx :any] (fnil into []) group))
                           base
                           grouped)
        any-by-sent (reduce (fn [acc [[_ sent] group]]
                              (update acc [:any sent] (fnil into []) group))
                            any-by-idx
                            grouped)]
    (assoc any-by-sent [:any :any] (vec triples))))
