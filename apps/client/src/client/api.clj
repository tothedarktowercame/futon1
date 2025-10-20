(ns client.api
  "Public API for the in-process Clojure client."
  (:require [client.session :as session]))

(defn start
  "Start a new client session. Accepts optional opts forwarded to store-manager/start!."
  ([] (start {}))
  ([opts] (session/start opts)))

(defn stop
  "Shutdown the active session."
  ([session] (session/stop session))
  ([] (session/stop)))

(defn run-line
  "Process a single line of input within SESSION.
  Returns the result map from client.session/process."
  [session line]
  (session/process session line))

(defn run-script
  "Run a sequence of input lines, returning a vector of result maps.
  Leaves the session ready for additional interaction."
  [session lines]
  (reduce (fn [acc line]
            (conj acc (run-line session line)))
          []
          lines))
