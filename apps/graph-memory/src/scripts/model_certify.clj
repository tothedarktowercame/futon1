;; apps/graph-memory/src/scripts/model_certify.clj
(ns scripts.model-certify
  "Re-issue model descriptor certificates with a named penholder."
  (:require [app.model :as model]
            [app.model-docbook :as model-docbook]
            [app.model-media :as model-media]
            [app.model-meta :as model-meta]
            [app.model-open-world :as model-open-world]
            [app.model-penholder :as model-penholder]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.string :as str]))

(def ^:private model-registry
  {:patterns model/descriptor-template
   :media model-media/descriptor-template
   :meta-model model-meta/descriptor-template
   :open-world-ingest model-open-world/descriptor-template
   :docbook model-docbook/descriptor-template
   :penholder model-penholder/descriptor-template})

(defn- usage []
  (str "Usage: clojure -M -m scripts.model-certify "
       "[--penholder NAME] [--models patterns,media,meta-model,...]\n"))

(defn- parse-args [args]
  (loop [opts {:penholder nil
               :models nil}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--penholder" (recur (assoc opts :penholder (second remaining))
                             (nnext remaining))
        "--models" (recur (assoc opts :models (second remaining))
                          (nnext remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- normalize-penholder [value]
  (when-let [raw (some-> value str/trim not-empty)]
    (str/lower-case raw)))

(defn- parse-models [value]
  (when-let [raw (some-> value str/trim not-empty)]
    (->> (str/split raw #",")
         (map str/trim)
         (remove str/blank?)
         (map keyword)
         vec)))

(defn- now-ms []
  (System/currentTimeMillis))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [penholder (or (normalize-penholder (:penholder opts))
                        (normalize-penholder (System/getenv "MODEL_PENHOLDER"))
                        (normalize-penholder (System/getenv "USER"))
                        (normalize-penholder (System/getenv "LOGNAME")))
          _ (when-not penholder
              (throw (ex-info "Missing penholder (use --penholder or set MODEL_PENHOLDER/USER)" {:opts opts})))
          models (or (parse-models (:models opts)) (vec (keys model-registry)))
          certificate {:penholder penholder
                       :issued-at (now-ms)}
          profile (store-manager/default-profile)
          ;; store-manager/conn initializes XTDB via store/restore! -> ensure-xt-node!
          conn (store-manager/conn profile)
          env (store-manager/env profile)]
      (when (and (get-in env [:xtdb :enabled?] true)
                 (not (xt/started?)))
        (println "WARNING: XTDB enabled but not started - updates may not persist"))
      (try
        (doseq [model-key models]
          (if-let [template-fn (get model-registry model-key)]
            (let [descriptor (assoc (template-fn) :schema/certificate certificate)]
              (case model-key
                :patterns (model/upsert-descriptor! conn env descriptor)
                :media (model-media/upsert-descriptor! conn env descriptor)
                :meta-model (model-meta/upsert-descriptor! conn env descriptor)
                :open-world-ingest (model-open-world/upsert-descriptor! conn env descriptor)
                :docbook (model-docbook/upsert-descriptor! conn env descriptor)
                :penholder (model-penholder/upsert-descriptor! conn env descriptor)))
            (throw (ex-info "Unknown model key" {:model model-key}))))
        ;; Sync XTDB to ensure all async writes are committed
        (when (xt/started?)
          (xt/sync-node!)
          (println "XTDB synced."))
        (println "Model certificates updated.")
        (finally
          (store-manager/shutdown!))))))
