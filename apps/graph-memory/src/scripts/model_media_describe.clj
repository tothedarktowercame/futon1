(ns scripts.model-media-describe
  "Print the media model descriptor metadata."
  (:require [app.model-media :as model]
            [app.store-manager :as store-manager]))

(defn -main [& _args]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)]
    (try
      (if-let [desc (model/describe conn)]
        (prn desc)
        (println "Media model descriptor not found."))
      (finally
        (store-manager/shutdown!)))))
