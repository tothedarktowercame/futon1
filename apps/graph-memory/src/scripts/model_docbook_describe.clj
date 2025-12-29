(ns scripts.model-docbook-describe
  "Print the current docbook model descriptor."
  (:require [app.model-docbook :as model]
            [app.store-manager :as store-manager]
            [clojure.pprint :as pprint]))

(defn -main [& _]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)]
    (try
      (if-let [desc (model/describe conn)]
        (pprint/pprint desc)
        (do
          (binding [*out* *err*]
            (println "Docbook model descriptor not found."))
          (System/exit 1)))
      (finally
        (store-manager/shutdown!)))))
