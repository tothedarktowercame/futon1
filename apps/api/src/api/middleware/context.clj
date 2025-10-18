(ns api.middleware.context
  (:require [datascript.core :as d]
            [app.xt :as x]))   ;; your wrapper; has db with 0 or 1 arg

(defn- deref-if [x]
  (cond
    (instance? clojure.lang.IDeref x) @x
    (fn? x) (x)
    :else x))

(defn- ds-conn-from [m]
  (or (:ds/conn m) (:conn m)))         ;; support :conn

(defn- xt-node-from [m]
  (or (:xt/node m) (:xtdb-node m)))    ;; support :xtdb-node

(defn- try-ds-db [conn]
  (when conn
    (try (d/db conn) (catch Throwable _ nil))))

(defn- call-db-0-or-1
  "Call a db-fn that may be 0-arity or 1-arity (node)."
  [db-fn node]
  (when db-fn
    (cond
      node (try (db-fn node)
                (catch clojure.lang.ArityException _
                  (try (db-fn) (catch Throwable _ nil)))
                (catch Throwable _ nil))
      :else (try (db-fn)
                 (catch clojure.lang.ArityException _
                   (try (db-fn node) (catch Throwable _ nil)))
                 (catch Throwable _ nil)))))

(defn wrap-context
  "Merge base ctx and attach :now, :ds/db, :xt/db if available.
   Looks for :conn or :ds/conn; :xtdb-node or :xt/node."
  ([handler base-ctx] (wrap-context handler base-ctx {}))
  ([handler base-ctx {:keys [clock xtdb-db-fn]
                      :or   {clock #(System/currentTimeMillis)
                             xtdb-db-fn x/db}}]
   (fn [request]
     (let [base   (merge {} (deref-if base-ctx) (:ctx request))
           now    (or (:now base) (clock))
           ds-db  (or (:ds/db base) (try-ds-db (ds-conn-from base)))
           xt-db  (or (:xt/db base) (call-db-0-or-1 xtdb-db-fn (xt-node-from base)))
           ctx*   (cond-> base
                    (some? now) (assoc :now now)
                    ds-db       (assoc :ds/db ds-db)
                    xt-db       (assoc :xt/db xt-db))]
       (handler (assoc request :ctx ctx*))))))
