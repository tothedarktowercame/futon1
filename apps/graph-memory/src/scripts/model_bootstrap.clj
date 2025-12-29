(ns scripts.model-bootstrap
  "Insert or update the model descriptor in the graph store."
  (:require [app.model :as model]
            [app.store-manager :as store-manager]))

(defn- usage []
  (str "Usage: clojure -M -m scripts.model-bootstrap [--force]\n"))

(defn- parse-args [args]
  (loop [opts {:force? false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--force" (recur (assoc opts :force? true) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)]
      (try
        (if (:force? opts)
          (model/upsert-descriptor! conn env (model/descriptor-template))
          (model/ensure-descriptor! conn env))
        (println "Model descriptor ready.")
        (finally
          (store-manager/shutdown!))))))
