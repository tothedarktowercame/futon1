(ns graph-memory.io
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [graph-memory.graph-memory :as g]))

(defn dump! [f]
  (spit f (pr-str (d/datoms @(g/conn!) :eavt))))

(defn load! [f]
  (g/reset-conn!)
  (let [datoms (edn/read-string (slurp f))]
    (d/transact! (g/conn!) (map #(zipmap [:e :a :v :tx] %) datoms)))) (ns gms.io)
