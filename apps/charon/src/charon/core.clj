;; apps/charon/src/charon/core.clj
(ns charon.core
  "Shared ingest gatekeeping for Futon1."
  (:require [clojure.string :as str]))

(def ^:private default-error :charon/reject)
(def ^:dynamic *guardian* nil)

(defn set-guardian!
  "Install a guardian function used by `guard`. Guardian receives a payload map."
  [f]
  (alter-var-root #'*guardian* (constantly f)))

(defn clear-guardian!
  "Remove any installed guardian."
  []
  (alter-var-root #'*guardian* (constantly nil)))

(defn ok
  "Wrap a successful result with the Charon envelope."
  ([surface] (ok surface {}))
  ([surface result]
   (assoc (or result {}) :ok? true :surface surface)))

(defn reject
  "Return a rejection envelope with optional hint."
  ([surface reason details]
   (reject surface reason details nil))
  ([surface reason details hint]
   (cond-> {:ok? false
            :error default-error
            :surface surface
            :reason reason
            :details details}
     (and hint (not (str/blank? hint))) (assoc :hint hint))))

(defn ensure-ok
  "Throw when RESULT is a Charon rejection."
  [result]
  (when (and (map? result) (false? (:ok? result)))
    (throw (ex-info "Charon rejected ingest" result)))
  result)

(defn guard
  "Invoke the installed guardian (if any). Returns a Charon-style result map."
  [payload]
  (if-let [guardian *guardian*]
    (guardian payload)
    {:ok? true
     :surface (:surface payload)}))

(defn guard!
  "Invoke guardian and throw on rejection."
  [payload]
  (ensure-ok (guard payload))
  payload)

(defn -main [& _args]
  (println "charon: ingest gatekeeping helpers (no CLI)."))
