;; apps/graph-memory/src/scripts/charon_penholder_bootstrap.clj
(ns scripts.charon-penholder-bootstrap
  "Ensure standard penholders are registered for ingest descriptors."
  (:require [app.model-penholder :as model-penholder]
            [app.store-manager :as store-manager]))

(defn -main [& _args]
  (let [profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (try
      (model-penholder/ensure-registry! conn env)
      (println "Penholder registry ready.")
      (finally
        (store-manager/shutdown!)))))
