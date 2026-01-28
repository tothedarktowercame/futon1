;; apps/graph-memory/src/scripts/docbook_repair_missing_entries.clj
(ns scripts.docbook-repair-missing-entries
  "Repair docbook headings/entries with missing required fields."
  (:require [app.charon-guard :as charon-guard]
            [app.config :as config]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(def ^:private heading-required
  [:doc/id :doc/book :doc/title :doc/outline_path :doc/path_string :doc/level])

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M -m scripts.docbook-repair-missing-entries [--apply]"
             "  --apply    Persist repairs (default is dry-run)"]))

(defn- parse-args [args]
  (loop [args args opts {:apply? false}]
    (if (seq args)
      (case (first args)
        "--apply" (recur (rest args) (assoc opts :apply? true))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- xtdb-config-path []
  (some-> (io/resource "xtdb.edn") io/file .getAbsolutePath))

(defn- cli-penholder [env]
  (or (:penholder env)
      (System/getenv "MODEL_PENHOLDER")
      (System/getenv "BASIC_CHAT_PENHOLDER")
      "cli"))

(defn- data-dir []
  (let [root (:app/data-dir (config/config))
        base (if (and root (not (.isAbsolute (io/file root))))
               (let [repo (loop [dir (io/file (System/getProperty "user.dir"))]
                            (when dir
                              (if (.exists (io/file dir "AGENTS.md"))
                                dir
                                (recur (.getParentFile dir)))))]
                 (if repo
                   (.getAbsolutePath (io/file repo root))
                   (.getAbsolutePath (io/file root))))
               root)]
    (when base
      (.getAbsolutePath (io/file base "xtdb")))))

(defn- normalize-outline [value]
  (cond
    (vector? value) value
    (sequential? value) (vec value)
    (string? value) (->> (str/split value #"\s*/\s*")
                         (remove str/blank?)
                         vec)
    :else nil))

(defn- missing? [value]
  (cond
    (nil? value) true
    (string? value) (str/blank? value)
    (sequential? value) (empty? value)
    :else false))

(defn- missing-fields [m fields]
  (->> fields (filter #(missing? (get m %))) vec))

(defn- heading-repair [heading]
  (let [path-str (:doc/path_string heading)
        outline (or (:doc/outline_path heading)
                    (normalize-outline path-str))
        title (or (:doc/title heading)
                  (when (seq outline) (last outline)))
        level (or (:doc/level heading)
                  (when (seq outline) (count outline)))]
    (cond-> heading
      (and (seq outline) (missing? (:doc/outline_path heading)))
      (assoc :doc/outline_path outline)
      (and (seq path-str) (missing? (:doc/path_string heading)))
      (assoc :doc/path_string path-str)
      (and (seq title) (missing? (:doc/title heading)))
      (assoc :doc/title title)
      (and level (missing? (:doc/level heading)))
      (assoc :doc/level level))))

(defn- heading-invalid? [heading]
  (seq (missing-fields heading heading-required)))

(defn- entry-body-missing? [entry]
  (let [status (:doc/status entry)
        body (:doc/body entry)]
    (and (not= :removed status)
         (missing? body))))

(defn -main [& args]
  (let [{:keys [apply?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (let [base (store-manager/env profile)]
              (assoc base :penholder (cli-penholder base)))
        cfg (xtdb-config-path)
        dir (data-dir)]
    (when-not cfg
      (throw (ex-info "XTDB config not found on classpath" {})))
    (xt/start! cfg {:data-dir dir :xt/created-by "scripts.docbook-repair-missing-entries"})
    (try
      (charon-guard/guard-models! conn [:docbook] env :docbook/repair)
      (let [db (xtdb/db (xt/node))
            headings (->> (xtdb/q db '{:find [(pull ?h [*])]
                                       :where [[?h :doc/id _]
                                               [?h :doc/book _]
                                               (not [?h :doc/entry-id _])]})
                          (map first))
            entries (->> (xtdb/q db '{:find [(pull ?e [*])]
                                      :where [[?e :doc/entry-id _]]})
                         (map first))
            heading-fixes (->> headings
                               (filter heading-invalid?)
                               (map (fn [heading]
                                      (let [repaired (heading-repair heading)]
                                        {:doc-id (:doc/id heading)
                                         :missing (missing-fields heading heading-required)
                                         :repaired repaired
                                         :still-missing (missing-fields repaired heading-required)})))
                               vec)
            entry-fixes (->> entries
                             (filter entry-body-missing?)
                             (map (fn [entry]
                                    {:doc-id (:doc/id entry)
                                     :entry-id (:doc/entry-id entry)
                                     :repaired (assoc entry :doc/status :removed
                                                      :doc/notes "auto-removed: missing body")}))
                             vec)]
        (println (format "Docbook headings with missing fields: %d" (count heading-fixes)))
        (doseq [entry (take 10 heading-fixes)]
          (println "  " (select-keys entry [:doc-id :missing :still-missing])))
        (println (format "Docbook entries with missing body: %d" (count entry-fixes)))
        (doseq [entry (take 10 entry-fixes)]
          (println "  " (select-keys entry [:doc-id :entry-id])))
        (when apply?
          (doseq [{:keys [repaired still-missing]} heading-fixes
                  :when (empty? still-missing)]
            (xt/put-entity! (assoc repaired :xt/id (:doc/id repaired))))
          (doseq [{:keys [repaired]} entry-fixes]
            (xt/put-entity! (assoc repaired :xt/id (:doc/entry-id repaired))))
          (println "Repairs applied.")))
      (finally
        (xt/stop!)))))
