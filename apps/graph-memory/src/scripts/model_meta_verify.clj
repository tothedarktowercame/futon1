(ns scripts.model-meta-verify
  "Run meta-model invariants and print results."
  (:require [app.model-meta :as model]
            [app.store-manager :as store-manager]))

(defn -main [& _args]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        result (model/verify conn)]
    (try
      (prn (assoc result :profile profile))
      (finally
        (store-manager/shutdown!)))))
