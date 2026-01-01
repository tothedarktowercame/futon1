(ns scripts.model-media-verify
  "Run media model invariants."
  (:require [app.model-media :as model]
            [app.store-manager :as store-manager]))

(defn -main [& _args]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)]
    (try
      (prn (model/verify conn))
      (finally
        (store-manager/shutdown!)))))
