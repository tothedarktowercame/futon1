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
