(ns v3.init
  (:require [v3.types :as T]))

(defn seed!
  "Call once at app start (only if you *use* v3)."
  []
  (doseq [t [:Word? :Noun? :ProperNoun? :Vector? :ListOfWord? :Person?]] (T/register-type! t))
  (T/add-subtype! :ProperNoun? :Noun?)
  (T/add-subtype! :ListOfWord? :Vector?)
  ;; choose your semantics; you might prefer :Person? <: :ProperNoun? or the reverse
  (T/add-subtype! :Person? :ProperNoun?)
  :ok)
