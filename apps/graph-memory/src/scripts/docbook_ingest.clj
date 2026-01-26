(ns scripts.docbook-ingest
  "CLI entry to ingest docbook Org sources into XTDB."
  (:require [app.docbook :as docbook]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scripts.rehydrate :as rehydrate]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M:scripts/docbook-ingest [--root PATH] [--book NAME] [--rehydrate]"
             "Environment variables:"
             "  DOCBOOK_ROOT  Override the docbook checkout root (default ../futon4)"
             "  FUTON4_ROOT   Alias for DOCBOOK_ROOT"
             "  FUTON1_REHYDRATE_URL Optional URL to POST for live rehydrate"
             "Notes:"
             "  Ingest reads spine2.org (and included files) under DOCBOOK_ROOT."]))

(defn- parse-args [args]
  (loop [opts {:root nil :book nil :help? false :rehydrate? nil :rehydrate-url nil}
         remaining args]
    (if-let [arg (first remaining)]
      (cond
        (#{"-h" "--help"} arg)
        (recur (assoc opts :help? true) (rest remaining))

        (#{"-r" "--root"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :root value) (nnext remaining))
          (throw (ex-info "Missing value for --root" {})))

        (#{"-b" "--book"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :book value) (nnext remaining))
          (throw (ex-info "Missing value for --book" {})))

        (#{"--rehydrate"} arg)
        (recur (assoc opts :rehydrate? true) (rest remaining))

        (#{"--rehydrate-url"} arg)
        (if-let [value (second remaining)]
          (recur (assoc opts :rehydrate-url value :rehydrate? true) (nnext remaining))
          (throw (ex-info "Missing value for --rehydrate-url" {})))

        :else
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- resolve-root [explicit]
  (let [env-root (or (some-> (System/getenv "DOCBOOK_ROOT") str/trim not-empty)
                     (some-> (System/getenv "FUTON4_ROOT") str/trim not-empty))
        fallback (-> (io/file ".." "futon4") .getAbsolutePath)
        path (or explicit env-root fallback)
        dir (io/file path)]
    (when-not (.exists dir)
      (throw (ex-info (str "Docbook root not found at " (.getAbsolutePath dir))
                      {:path (.getAbsolutePath dir)})))
    (.getAbsolutePath dir)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (if (:help? opts)
      (println (usage))
      (try
        (let [resolved (assoc opts :root (resolve-root (:root opts)))
              result (docbook/ingest! resolved)]
          (println (format "[docbook-ingest] book=%s headings=%s entries=%s"
                           (:book result)
                           (or (:headings result) "?")
                           (:entries result)))
          (when (false? (:ok? result))
            (binding [*out* *err*]
              (println (format "[docbook-ingest] rejected %d invalid entry(ies)"
                               (count (get-in result [:details :errors]))))
              (doseq [entry (take 5 (get-in result [:details :errors]))]
                (println "  " (select-keys entry [:doc-id :entry-id :heading-error :entry-error]))))
            (System/exit 1))
          (rehydrate/maybe-rehydrate! {:rehydrate? (:rehydrate? opts)
                                       :rehydrate-url (:rehydrate-url opts)}))
        (finally
          (xt/stop!)
          (shutdown-agents))))))
