(ns open-world-ingest.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [open-world-ingest.nlp :as nlp]
            [open-world-ingest.storage :as storage])
  (:gen-class))

(def cli-options
  [["-d" "--data-dir DIR" "Directory for XTDB storage"
    :default (-> (io/file "data" "open-world") .getAbsolutePath)]
   ["-c" "--config PATH" "Path to XTDB configuration file"]])

(defn usage [options-summary]
  (->> ["Open-world ingest module"
        ""
        "Usage: clojure -M:run-m [options]"
        ""
        "Options:"
        options-summary
        ""
        "Commands:"
        "  (default)      Ingest the provided line into the catalog"
        "  /tail [n]      Show the last n relations (default 5)"
        "  /ego NAME      Display neighbors connected to the named entity"
        "  /cooccur NAME  Show entities that co-occur with the named entity"
        "  /help          Show this help message"]
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

(defn- describe-recent
  [{:keys [relation src dst polarity]}]
  (let [base (format "  - %s —%s→ %s" (:label src) relation (:label dst))]
    (if (= polarity :negated)
      (str base " (negated)")
      base)))

(defn- handle-tail
  [args]
  (let [limit (when-let [n (first args)]
                (try
                  (Integer/parseInt n)
                  (catch NumberFormatException _
                    (println "Invalid tail limit, using default 5")
                    nil)))
        rows (storage/recent-relations (or limit 5))]
    (if (seq rows)
      (do
        (println "Recent relations:")
        (doseq [row rows]
          (println (describe-recent row))))
      (println "No relations recorded yet."))))

(defn- handle-ego
  [[name & _]]
  (if (str/blank? name)
    (println "Usage: /ego <entity>")
    (if-let [entity (storage/entity-by-name name)]
      (let [{:keys [outgoing incoming]} (storage/ego-neighbors (:id entity))
            label (:label entity)
            kind (:kind entity)]
        (println (format "Entity: %s (%s)" label (name kind)))
        (if (seq outgoing)
          (do
            (println "Outgoing:")
            (doseq [{:keys [relation entity polarity]} outgoing]
              (println (str "  - —" relation "→ " (:label entity)
                            (when (= polarity :negated) " (negated)")))))
          (println "Outgoing: none"))
        (if (seq incoming)
          (do
            (println "Incoming:")
            (doseq [{:keys [relation entity polarity]} incoming]
              (println (str "  - " (:label entity) " —" relation "→ " label
                            (when (= polarity :negated) " (negated)")))))
          (println "Incoming: none")))
      (println "Entity not found."))))

(defn- handle-cooccur
  [[name & _]]
  (if (str/blank? name)
    (println "Usage: /cooccur <entity>")
    (if-let [entity (storage/entity-by-name name)]
      (let [rows (storage/cooccurring-entities (:id entity))]
        (println (format "Co-occurrences with %s:" (:label entity)))
        (if (seq rows)
          (doseq [{:keys [label count]} rows]
            (println (format "  - %s (%d)" label count)))
          (println "  (none)")))
      (println "Entity not found."))))

(defn- handle-command
  [line usage-text]
  (let [[cmd & args] (str/split line #"\s+")]
    (case cmd
      "/tail" (handle-tail args)
      "/ego" (handle-ego args)
      "/cooccur" (handle-cooccur args)
      "/help" (println usage-text)
      (println "Unknown command. Use /help for a list of commands."))))

(defn- ingest-line
  [line]
  (let [analysis (nlp/analyze line)
        summary (storage/store-analysis! line analysis)]
    (print-ingest-summary summary)))

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
      (let [config {:data-dir (:data-dir options)
                    :config-path (:config options)}]
        (storage/start-node! config)
        (.addShutdownHook (Runtime/getRuntime)
                          (Thread. storage/stop!))
        (println "Open-world ingest ready. Type sentences or /help for commands.")
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
                  (if (str/starts-with? trimmed "/")
                    (handle-command trimmed usage-text)
                    (ingest-line trimmed))))
              (recur))
            (do
              (println)
              (println "Shutting down.")
              (storage/stop!))))))))
