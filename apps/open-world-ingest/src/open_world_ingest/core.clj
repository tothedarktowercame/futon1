(ns open-world-ingest.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [open-world-ingest.nlp :as nlp]
            [open-world-ingest.storage :as storage]
            [open-world-ingest.trace :as trace])
  (:gen-class))

(def cli-options
  [["-d" "--data-dir DIR" "Directory for XTDB storage"
    :default (-> (io/file "data" "open-world") .getAbsolutePath)]
   ["-c" "--config PATH" "Path to XTDB configuration file"]
   ["-t" "--trace-openie [PATH]" "Capture OpenIE triples to an EDN file (optional path)"
    :default false
    :assoc-fn (fn [m k v]
                (assoc m k (if (nil? v)
                             true
                             v)))]] )

(defn usage [options-summary]
  (->> ["Open-world ingest module"
        ""
        "Usage: clojure -M:run-m [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn- format-entity
  [{:keys [label kind new?]}]
  (str "  - " label " (" (name kind) ")" (when new? " [new]")))

(defn- format-relation
  [{:keys [subject relation object polarity confidence]}]
  (let [base (format "  - %s —%s→ %s" subject relation object)
        base (if (= polarity :negated)
               (str base " (negated)")
               base)]
    (if confidence
      (str base (format " (conf %.2f)" (double confidence)))
      base)))

(defn- print-ingest-summary
  [{:keys [entities relations]}]
  (if (seq entities)
    (do
      (println "Entities:")
      (doseq [entity entities]
        (println (format-entity entity))))
    (println "Entities: none"))
  (if (seq relations)
    (do
      (println "Relations:")
      (doseq [relation relations]
        (println (format-relation relation))))
    (println "Relations: none")))

(defn- ingest-line
  [line nlp-opts]
  (let [analysis (nlp/analyze line nlp-opts)
        summary (storage/store-analysis! line analysis)]
    (if (false? (:ok? summary))
      (let [details (get-in summary [:details :errors])]
        (binding [*out* *err*]
          (println "Open-world ingest rejected.")
          (doseq [entry (take 5 details)]
            (println "  " (select-keys entry [:missing :relation]))))
        (throw (ex-info "Open-world ingest rejected" summary)))
      (print-ingest-summary summary))))

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-options)
        usage-text (usage summary)]
    (cond
      (seq errors)
      (do
        (doseq [err errors]
          (println err))
        (println usage-text)
        (System/exit 1))

      :else
      (let [trace-config (trace/config (:trace-openie options))
            nlp-opts (cond-> {}
                       trace-config (assoc :trace-openie trace-config))
            config {:data-dir (:data-dir options)
                    :config-path (:config options)}]
        (when trace-config
          (trace/reset! trace-config)
          (println "Tracing OpenIE triples to" (:path trace-config)))
        (storage/start-node! config)
        (.addShutdownHook (Runtime/getRuntime)
                          (Thread. storage/stop!))
        (println "Open-world ingest ready. Type sentences to add them to XTDB.")
        (loop []
          (print "you> ")
          (flush)
          (if-let [line (try
                          (read-line)
                          (catch Throwable t
                            (println)
                            (println "Error reading input:" (.getMessage t))
                            nil))]
              (do
                (let [trimmed (str/trim line)]
                  (when (seq trimmed)
                    (ingest-line trimmed nlp-opts)))
                (recur))
            (do
              (println)
              (println "Shutting down.")
              (storage/stop!))))))))
