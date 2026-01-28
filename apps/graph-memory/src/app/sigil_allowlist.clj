(ns app.sigil-allowlist
  "Load the Futon3 sigil allowlist (tokipona emoji + truth-table hanzi)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io PushbackReader)))

(def ^:private tokipona-path ["holes" "tokipona.org"])
(def ^:private truth-table-path ["holes" "256ca.el"])

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        (.getAbsolutePath dir)
        (recur (.getParentFile dir))))))

(defn- sibling-futon3 [root]
  (when root
    (when-let [parent (.getParentFile (io/file root))]
      (let [candidate (io/file parent "futon3")]
        (when (.exists candidate)
          (.getAbsolutePath candidate))))))

(defn- sibling-futon3-upwards [start]
  (some (fn [dir]
          (when-let [parent (.getParentFile (io/file dir))]
            (let [candidate (io/file parent "futon3")]
              (when (.exists candidate)
                (.getAbsolutePath candidate)))))
        (take-while identity
                    (iterate (fn [^java.io.File f] (.getParentFile f))
                             (io/file start)))))

(defn- env-root []
  (some-> (System/getenv "FUTON3_ROOT") str/trim not-empty))

(defn- sys-root []
  (some-> (System/getProperty "futon3.root") str/trim not-empty))

(defn- resolve-root [explicit]
  (let [fallback (-> (io/file ".." "futon3") .getAbsolutePath)
        user-dir (System/getProperty "user.dir")
        candidates (remove nil?
                           [explicit
                            (env-root)
                            (sys-root)
                            (sibling-futon3 (repo-root))
                            (when user-dir (sibling-futon3-upwards user-dir))
                            fallback])
        path (some (fn [candidate]
                     (let [dir (io/file candidate)]
                       (when (.exists dir)
                         (.getAbsolutePath dir))))
                   candidates)]
    (when-not path
      (throw (ex-info "Futon3 root not found"
                      {:candidates candidates})))
    path))

(defn resolve-futon3-root
  ([] (resolve-futon3-root nil))
  ([explicit] (resolve-root explicit)))

(defn- tokipona-file [root]
  (apply io/file root tokipona-path))

(defn- truth-table-file [root]
  (apply io/file root truth-table-path))

(defn tokipona-emoji-order [root]
  (let [file (tokipona-file root)]
    (when (.exists file)
      (with-open [r (io/reader file)]
        (->> (line-seq r)
             (filter #(and (str/starts-with? % "|")
                           (not (str/starts-with? % "|---"))))
             (map #(map str/trim (str/split % #"\|")))
             (keep (fn [cols]
                     (when (>= (count cols) 2)
                       (let [emoji (nth cols 1)]
                         (when (and (seq emoji)
                                    (not= (str/lower-case emoji) "emoji"))
                           emoji)))))
             distinct
             vec)))))

(defn truth-table-hanzi-order [root]
  (let [file (truth-table-file root)]
    (when (.exists file)
      (with-open [r (PushbackReader. (io/reader file))]
        (loop []
          (let [form (read r false ::eof)]
            (cond
              (= form ::eof) nil
              (and (seq? form)
                   (= 'defvar (first form))
                   (= 'truth-table-8 (second form)))
              (let [table (nth form 2)
                    entries (if (and (seq? table) (= 'quote (first table)))
                              (second table)
                              table)]
                (->> entries
                     (map second)
                     (remove str/blank?)
                     vec))
              :else (recur))))))))

(defn ensure-allowlist! [emoji-order hanzi-order]
  (when-not (seq emoji-order)
    (throw (ex-info "tokipona emoji allowlist missing or empty"
                    {:file (str/join "/" tokipona-path)})))
  (when-not (seq hanzi-order)
    (throw (ex-info "truth-table hanzi allowlist missing or empty"
                    {:file (str/join "/" truth-table-path)}))))

(defn allowlist-from-root
  ([] (allowlist-from-root nil))
  ([root]
   (let [root* (resolve-root root)
         emoji-order (tokipona-emoji-order root*)
         hanzi-order (truth-table-hanzi-order root*)]
     (ensure-allowlist! emoji-order hanzi-order)
     {:root root*
      :emoji-order emoji-order
      :hanzi-order hanzi-order
      :emoji-set (set emoji-order)
      :hanzi-set (set hanzi-order)})))

(defn sigil-allowed? [allowlist {:keys [emoji hanzi]}]
  (and (contains? (:emoji-set allowlist) emoji)
       (contains? (:hanzi-set allowlist) hanzi)))
