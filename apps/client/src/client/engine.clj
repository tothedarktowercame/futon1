(ns client.engine
  "Lightweight helpers for interpreting user input."
  (:require [clojure.string :as str]))

(defn classify
  "Return {:type ...} describing how to handle raw user input."
  [line]
  (let [text (or line "")
        trimmed (str/trim text)]
    (cond
      (str/blank? trimmed) {:type :blank :raw trimmed}
      (str/starts-with? trimmed "/") {:type :slash :raw (subs trimmed 1)}
      (str/starts-with? trimmed "!") {:type :bang  :raw (subs trimmed 1)}
      :else {:type :say :raw trimmed})))
