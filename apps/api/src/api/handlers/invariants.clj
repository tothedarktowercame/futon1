;; apps/api/src/api/handlers/invariants.clj
(ns api.handlers.invariants
  (:require [app.invariants :as invariants]
            [app.store-manager :as store-manager]))

(defn verify-on-write? []
  (invariants/verify-on-write?))

(defn verify-core
  "Verify core model invariants (patterns, media, meta-model, open-world ingest)."
  [profile]
  (let [conn (store-manager/conn profile)
        env (store-manager/env profile)
        _ (invariants/ensure-descriptors! conn env)
        result (invariants/verify-core conn)]
    (assoc result :profile profile)))

(defn maybe-verify-core
  [profile]
  (when (verify-on-write?)
    (verify-core profile)))

(defn verify-models
  "Verify a subset of models."
  [profile models]
  (let [conn (store-manager/conn profile)
        env (store-manager/env profile)
        _ (invariants/ensure-descriptors! conn env models)
        result (invariants/verify-models conn models)]
    (assoc result :profile profile)))

(defn maybe-verify-models
  [profile models]
  (when (verify-on-write?)
    (verify-models profile models)))

(defn verify-event
  "Verify invariants for a specific event."
  ([profile event]
   (verify-event profile event {}))
  ([profile event opts]
   (let [conn (store-manager/conn profile)
         env (store-manager/env profile)
         _ (invariants/ensure-descriptors! conn env)
         result (invariants/verify-event conn event opts)]
     (assoc result :profile profile))))

(defn maybe-verify-event
  ([profile event]
   (maybe-verify-event profile event {}))
  ([profile event opts]
   (when (verify-on-write?)
     (verify-event profile event opts))))
