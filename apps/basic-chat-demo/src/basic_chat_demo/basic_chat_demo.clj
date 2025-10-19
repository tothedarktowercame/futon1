(ns basic-chat-demo.basic-chat-demo
  (:require [app.http-client :as http-client]
            [basic-chat-demo.cli :as legacy]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [protocols.registry :as registry])
  (:gen-class))

(defn- http-mode? [args]
  (some #{"--http"} args))

(defn- load-script [path]
  (let [file (io/file path)]
    (when-not (.exists file)
      (throw (ex-info (str "Script not found: " path) {:path path})))
    (binding [*read-eval* false]
      (edn/read-string (slurp file)))))

(defn- default-pronouns []
  {:me "Me" :you "You" :we "We"})

(defn- run-protocol-script [{:keys [protocol script] :as opts}]
  (let [entry (registry/fetch protocol)]
    (when-not entry
      (throw (ex-info (str "Unknown protocol " protocol) {:protocol protocol})))
    (let [lines (load-script script)
          init-fn (:init entry)
          configure (:configure entry)
          handle (:handle entry)]
      (when-not handle
        (throw (ex-info (str "Protocol missing handle fn " protocol) {:protocol protocol})))
      (let [base-state (if init-fn (init-fn) {})
            configured (if configure
                         (configure base-state opts)
                         base-state)
            state (cond-> configured
                    (map? configured) (assoc :pronouns (default-pronouns)))
            now (System/currentTimeMillis)
            results (map-indexed (fn [idx line]
                                   (let [ts (+ now idx)
                                         result (handle state line ts)]
                                     (if (:in result)
                                       result
                                       (assoc result :in line))))
                                 lines)]
        (prn (vec results))
        (flush)))))

(defn -main
  "Entry point for the basic-chat demo.

  Defaults to the legacy CLI (which supports scripted runs and interactive
  slash commands). Pass `--http` to launch the lightweight HTTP client."
  [& args]
  (cond
    (http-mode? args)
    (do
      (println "futon1 (v6) HTTP-enabled Client (α)")
      (http-client/interactive-loop!))

    :else
    (let [clean-args (remove #{"--http" "--legacy"} args)
          opts (legacy/parse-args clean-args)]
      (if-let [_script (:script opts)]
        (do
          (println "futon1 (v6) Legacy Client — script mode")
          (run-protocol-script opts))
        (do
          (println "futon1 (v6) Legacy Client")
          (apply legacy/old-main clean-args))))))
