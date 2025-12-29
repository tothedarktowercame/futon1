(ns scripts.model-open-world-describe
  "Print the current open-world ingest model descriptor."
  (:require [app.model-open-world :as model]
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
            (println "Open-world ingest model descriptor not found."))
          (System/exit 1)))
      (finally
        (store-manager/shutdown!)))))
