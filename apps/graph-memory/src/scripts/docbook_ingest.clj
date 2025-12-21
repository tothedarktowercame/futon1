(ns scripts.docbook-ingest
  "CLI entry to ingest docbook Org sources into XTDB."
  (:require [app.docbook :as docbook]
            [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- usage []
  (str/join \newline
            ["Usage: clojure -M:scripts/docbook-ingest [--root PATH] [--book NAME]"
             "Environment variables:"
             "  DOCBOOK_ROOT  Override the docbook checkout root (default ../futon4)"
             "  FUTON4_ROOT   Alias for DOCBOOK_ROOT"
             "Notes:"
             "  Ingest reads spine2.org (and included files) under DOCBOOK_ROOT."]))

(defn- parse-args [args]
  (loop [opts {:root nil :book nil :help? false}
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
                           (:entries result))))
        (finally
          (xt/stop!)
          (shutdown-agents))))))
