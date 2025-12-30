(ns scripts.model-meta-describe
  "Print the current meta-model descriptor."
  (:require [app.model-meta :as model]
            [app.store-manager :as store-manager]))

(defn -main [& _args]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        desc (model/describe conn)]
    (try
      (prn (assoc (or desc {}) :profile profile))
      (finally
        (store-manager/shutdown!)))))
