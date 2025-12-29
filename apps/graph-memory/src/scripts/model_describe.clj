(ns scripts.model-describe
  "Print the current model descriptor."
  (:require [app.model :as model]
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
            (println "Model descriptor not found."))
          (System/exit 1)))
      (finally
        (store-manager/shutdown!)))))
