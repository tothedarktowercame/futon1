(ns client.script-runner
  "Lightweight helper for executing protocol scripts (mirrors the legacy CLI logic)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [protocols.registry :as registry]))

(defn default-pronouns [] {:me "Me" :you "You" :we "We"})

(defn load-script
  "Read an EDN vector of utterances from `path`."
  [path]
  (let [file (io/file path)]
    (when-not (.exists file)
      (throw (ex-info (str "Script not found: " path) {:path path})))
    (binding [*read-eval* false]
      (edn/read-string (slurp file)))))

(defn- prepare-state [entry opts]
  (let [init-state (if-let [init-fn (:init entry)] (init-fn) {})
        configured (if-let [configure (:configure entry)]
                     (configure init-state opts)
                     init-state)]
    (cond-> configured
      (map? configured) (update :pronouns #(or % (default-pronouns))))))

(defn run-lines
  "Execute the deterministic protocol identified by `protocol` against the
  provided collection of `lines`. Optional `opts` map is forwarded to the
  protocol configure hook (mirroring the legacy CLI). Returns a vector of result
  maps (each guaranteed to include `:in`)."
  [{:keys [protocol lines opts] :as cfg}]
  (let [entry (registry/fetch protocol)]
    (when-not entry
      (throw (ex-info (str "Unknown protocol " protocol) {:protocol protocol :cfg cfg})))
    (let [handle (:handle entry)]
      (when-not handle
        (throw (ex-info (str "Protocol missing handle fn " protocol) {:protocol protocol})))
      (let [state (prepare-state entry opts)
            now   (System/currentTimeMillis)]
        (->> lines
             (map-indexed (fn [idx line]
                            (let [ts (+ now idx)
                                  result (handle state line ts)]
                              (if (:in result)
                                result
                                (assoc result :in line)))))
             vec)))))

(defn run-script
  "Load the EDN script at `path` and run it via `run-lines`."
  [{:keys [script] :as opts}]
  (when-not script
    (throw (ex-info "Missing :script" opts)))
  (let [lines (load-script script)]
    (run-lines (assoc opts :lines lines))))
