;; apps/graph-memory/src/scripts/penholder_bootstrap.clj
(ns scripts.penholder-bootstrap
  "Create or update penholder registry entries."
  (:require [app.model-penholder :as model-penholder]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.edn :as edn]))

(def ^:private default-descriptors
  ["model/descriptor/patterns"
   "model/descriptor/media"
   "model/descriptor/meta-model"
   "model/descriptor/open-world-ingest"
   "model/descriptor/docbook"
   "model/descriptor/penholder"])

(defn- usage []
  (str "Usage: clojure -M -m scripts.penholder-bootstrap "
       "[--penholder NAME]... [--descriptor NAME]... "
       "[--certificate EDN] [--non-strict]\n"))

(defn- parse-args [args]
  (loop [opts {:penholders []
               :descriptors []
               :strict? true
               :certificate nil}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--penholder" (recur (update opts :penholders conj (second remaining))
                             (nnext remaining))
        "--descriptor" (recur (update opts :descriptors conj (second remaining))
                              (nnext remaining))
        "--certificate" (recur (assoc opts :certificate (second remaining))
                               (nnext remaining))
        "--non-strict" (recur (assoc opts :strict? false) (rest remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- parse-certificate [raw]
  (when raw
    (try
      (let [value (edn/read-string raw)]
        (if (and (map? value) (seq value)) value raw))
      (catch Exception _ raw))))

(defn- penholder-entry [descriptor penholders strict? certificate]
  {:name (str "penholder/" descriptor)
   :type :model/penholder
   :source {:descriptor descriptor
            :penholders penholders
            :certificate certificate
            :strict? strict?}})

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [penholders (vec (remove nil? (:penholders opts)))
          penholders (if (seq penholders) penholders ["api"])
          descriptors (vec (remove nil? (:descriptors opts)))
          descriptors (if (seq descriptors) descriptors default-descriptors)
          certificate (parse-certificate (:certificate opts))
          profile (store-manager/default-profile)
          conn (store-manager/conn profile)
          env (store-manager/env profile)]
      (try
        (when-not certificate
          (throw (ex-info "Missing --certificate EDN for penholder entries" {:opts opts})))
        (model-penholder/ensure-descriptor! conn env)
        (doseq [descriptor descriptors]
          (store/ensure-entity! conn env
                                (penholder-entry descriptor penholders (:strict? opts) certificate)))
        (println "Penholder registry ready.")
        (finally
          (store-manager/shutdown!))))))
