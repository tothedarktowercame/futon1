(ns scripts.model-verify
  "Run model invariants and print results."
  (:require [app.model :as model]
            [app.store-manager :as store-manager]
            [clojure.pprint :as pprint]))

(defn- usage []
  (str "Usage: clojure -M -m scripts.model-verify [--out PATH] [--core-only]\n"
       "  --out PATH    Write EDN results to PATH\n"
       "  --core-only   Write only core-component failures to --out\n"))

(defn- parse-args [args]
  (loop [args args opts {:core-only? false}]
    (if (seq args)
      (case (first args)
        "--out" (if-let [path (second args)]
                  (recur (nnext args) (assoc opts :out path))
                  (throw (ex-info "Missing path after --out" {:args args})))
        "--core-only" (recur (rest args) (assoc opts :core-only? true))
        "--help" (do
                   (println (usage))
                   (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first args)) {:args args})))
      opts)))

(defn- core-failures [result]
  (some->> (:results result)
           (filter #(= :patterns/pattern-core-components (:key %)))
           first
           :failures))

(defn -main [& args]
  (let [{:keys [out core-only?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)]
    (try
      (let [result (model/verify conn)]
        (pprint/pprint result)
        (when out
          (let [payload (if core-only?
                          {:generated-at (System/currentTimeMillis)
                           :profile profile
                           :core-failures (or (core-failures result) [])}
                          result)]
            (spit out (pr-str payload))
            (println (str "Wrote results to " out)))))
      (finally
        (store-manager/shutdown!)))))
