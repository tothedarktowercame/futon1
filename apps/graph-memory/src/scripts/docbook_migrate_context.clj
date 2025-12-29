(ns scripts.docbook-migrate-context
  "Migrate doc/context -> doc/body for docbook entries in XTDB."
  (:require [app.config :as config]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xtdb]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M:scripts/docbook-migrate-context [--book NAME] [--dry-run]"
             "Environment variables:"
             "  ALPHA_DATA_DIR  Override XTDB data dir (if configured)"
             "Notes:"
             "  Migrates :doc/context -> :doc/body for all matching docs."]))

(defn- parse-args [args]
  (loop [opts {:book nil :dry-run? false :help? false :verbose? false}
         remaining args]
    (if-let [arg (first remaining)]
      (cond
        (#{"-h" "--help"} arg)
        (recur (assoc opts :help? true) (rest remaining))

        (#{"-b" "--book"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :book value) (nnext remaining))
          (throw (ex-info "Missing value for --book" {})))

        (#{"-n" "--dry-run"} arg)
        (recur (assoc opts :dry-run? true) (rest remaining))

        (#{"-v" "--verbose"} arg)
        (recur (assoc opts :verbose? true) (rest remaining))

        :else
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- ensure-xt-node! []
  (when-not (xt/started?)
    (let [cfg (config/config)
          cfg-path (:xtdb/config-path cfg "apps/graph-memory/resources/xtdb.edn")
          data-dir (:app/data-dir cfg)]
      (xt/start! cfg-path {:data-dir (when data-dir (str (io/file data-dir "xtdb")))
                           :xt/created-by "scripts.docbook-migrate-context"}))))

(defn- docs-with-context [book]
  (let [db (xt/db)]
    (if book
      (->> (xt/q db '{:find [(pull ?e [*])]
                      :in [?book]
                      :where [[?e :doc/context ?ctx]
                              [?e :doc/book ?book]]}
                 book)
           (map first))
      (->> (xt/q db '{:find [(pull ?e [*])]
                      :where [[?e :doc/context ?ctx]]})
           (map first)))))

(defn- migrate-doc [doc]
  (let [body (or (:doc/body doc) (:doc/context doc))]
    (-> doc
        (assoc :doc/body body)
        (dissoc :doc/context))))

(defn- submit-batches! [docs]
  (doseq [batch (partition-all 100 docs)]
    (xt/submit! (mapv (fn [doc] [::xtdb/put doc]) batch))))

(defn- summarize-docs [docs]
  (let [by-book (group-by :doc/book docs)]
    {:total (count docs)
     :books (->> by-book
                 (map (fn [[book items]]
                        {:book (or book "<unknown>")
                         :count (count items)
                         :sample (->> items
                                      (keep :doc/id)
                                      distinct
                                      (take 5)
                                      vec)
                         :missing-body (count (filter #(nil? (:doc/body %)) items))}))
                 (sort-by :book)
                 vec)}))

(defn -main [& args]
  (let [{:keys [book dry-run? help? verbose?]} (parse-args args)]
    (if help?
      (println (usage))
      (try
        (ensure-xt-node!)
        (let [docs (docs-with-context book)
              migrated (map migrate-doc docs)
              total (count docs)
              summary (when (or dry-run? verbose?)
                        (summarize-docs docs))]
          (if dry-run?
            (do
              (println (format "[docbook-migrate] dry-run=true book=%s matched=%d"
                               (or book "<all>")
                               total))
              (doseq [{:keys [book count sample missing-body]} (:books summary)]
                (println (format "  book=%s count=%d missing-body=%d sample=%s"
                                 book
                                 count
                                 missing-body
                                 (pr-str sample)))))
            (do
              (submit-batches! migrated)
              (println (format "[docbook-migrate] dry-run=false book=%s migrated=%d"
                               (or book "<all>")
                               total))))
          (when (and verbose? (not dry-run?))
            (doseq [{:keys [book count sample missing-body]} (:books summary)]
              (println (format "  book=%s count=%d missing-body=%d sample=%s"
                               book
                               count
                               missing-body
                               (pr-str sample))))))
        (finally
          (xt/stop!)
          (shutdown-agents))))))
