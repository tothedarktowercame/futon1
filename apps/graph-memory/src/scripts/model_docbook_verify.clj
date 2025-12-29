(ns scripts.model-docbook-verify
  "Run docbook model invariants and print results."
  (:require [app.model-docbook :as model]
            [app.store-manager :as store-manager]
            [clojure.pprint :as pprint]))

(defn- usage []
  (str "Usage: clojure -M -m scripts.model-docbook-verify [--out PATH]\n"
       "  --out PATH    Write EDN results to PATH\n"))

(defn- parse-args [args]
  (loop [args args opts {}]
    (if (seq args)
      (case (first args)
        "--out" (if-let [path (second args)]
                  (recur (nnext args) (assoc opts :out path))
                  (throw (ex-info "Missing path after --out" {:args args})))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn -main [& args]
  (let [{:keys [out]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)]
    (try
      (let [result (model/verify conn)]
        (pprint/pprint result)
        (when out
          (spit out (pr-str result))
          (println (str "Wrote results to " out))))
      (finally
        (store-manager/shutdown!)))))
