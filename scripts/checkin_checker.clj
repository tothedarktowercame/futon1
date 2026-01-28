(ns checkin-checker
  "Fail if untracked/ignored files exist under classpath roots for main apps."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import (java.nio.file Paths)))

(defn- die! [msg data]
  (binding [*out* *err*]
    (println msg)
    (when data
      (println (pr-str data))))
  (System/exit 2))

(defn- repo-root []
  (let [{:keys [exit out err]} (sh/sh "git" "rev-parse" "--show-toplevel")]
    (when-not (zero? exit)
      (die! "Not inside a git repository." {:err err}))
    (str/trim out)))

(defn- read-edn [file]
  (when (.exists file)
    (edn/read-string (slurp file))))

(defn- deps-paths [root]
  (let [deps-file (io/file root "deps.edn")
        deps (read-edn deps-file)]
    (when-not deps
      (die! "deps.edn not found or unreadable." {:path (.getPath deps-file)}))
    (->> (:paths deps)
         (map #(-> (io/file root %) .getCanonicalPath))
         (remove nil?)
         vec)))

(defn- app-deps-paths [root]
  (let [apps-dir (io/file root "apps")]
    (if-not (.exists apps-dir)
      []
      (->> (.listFiles apps-dir)
           (filter #(.isDirectory %))
           (mapcat (fn [dir]
                     (let [deps-file (io/file dir "deps.edn")
                           deps (read-edn deps-file)]
                       (when deps
                         (map #(-> (io/file dir %) .getCanonicalPath)
                              (:paths deps))))))
           (remove nil?)))))

(defn- scan-roots [root]
  (->> (concat (deps-paths root) (app-deps-paths root))
       (map #(-> (Paths/get ^String % (make-array String 0))
                 .normalize
                 .toAbsolutePath))
       distinct
       vec))

(defn- git-status [root]
  (let [{:keys [exit out err]} (sh/sh "git" "-C" root
                                     "status" "--porcelain=v1"
                                     "-z" "-uall" "--ignored=matching")]
    (when-not (zero? exit)
      (die! "git status failed" {:err err}))
    out))

(defn- untracked-or-ignored [status-out]
  (->> (str/split status-out #"\u0000")
       (remove str/blank?)
       (keep (fn [entry]
               (when (>= (count entry) 3)
                 (let [code (subs entry 0 2)
                       path (subs entry 3)]
                   (when (or (= code "??") (= code "!!"))
                     {:code code :path path})))))))

(defn- resolve-path [root rel]
  (-> (Paths/get ^String root (make-array String 0))
      (.resolve rel)
      .normalize
      .toAbsolutePath))

(defn- in-scan-root? [scan-roots ^java.nio.file.Path path]
  (some #(.startsWith path %) scan-roots))

(defn- offenders [root scan-roots]
  (->> (untracked-or-ignored (git-status root))
       (map (fn [{:keys [code path]}]
              (let [abs (resolve-path root path)]
                (when (in-scan-root? scan-roots abs)
                  {:code code :path path :abs abs}))))
       (remove nil?)))

(defn -main [& args]
  (let [root (repo-root)
        scan-roots (scan-roots root)
        verbose? (some #{"--verbose" "-v"} args)
        offenders (offenders root scan-roots)]
    (when verbose?
      (println "Scan roots:")
      (doseq [root scan-roots]
        (println " " (.toString root))))
    (if (seq offenders)
      (do
        (println "Untracked/ignored files under classpath roots:")
        (doseq [{:keys [code path]} offenders]
          (println (format "  %s %s" code path)))
        (println)
        (println "Fix: git add/commit or move files out of classpath roots.")
        (System/exit 1))
      (do
        (println "Check-in checker: OK")
        (System/exit 0)))))
