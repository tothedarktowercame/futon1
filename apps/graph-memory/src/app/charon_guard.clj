;; apps/graph-memory/src/app/charon_guard.clj
(ns app.charon-guard
  "Charon-backed invariant guards for internal writes."
  (:require [app.invariants :as invariants]
            [charon.core :as charon]))

(defn- reject
  [surface reason details]
  (charon/reject surface reason details "Model invariants failed."))

(defn guard-event
  "Guard a Datascript event using model invariants."
  ([conn event]
   (guard-event conn event {}))
  ([conn event opts]
   (let [surface (or (:surface opts) (:type event) :store/tx)
         result (invariants/verify-event conn event opts)]
     (if (:ok? result)
       (charon/ok surface {:event-type (:type event)
                           :result result})
       (reject surface :model/invariants-failed
               {:event-type (:type event)
                :result result})))))

(defn guard-models
  "Guard one or more models directly (e.g. open-world ingest writes)."
  [conn models & [surface]]
  (let [surface (or surface :charon/guard)
        result (invariants/verify-models conn models nil)]
    (if (:ok? result)
      (charon/ok surface {:models models
                          :result result})
      (reject surface :model/invariants-failed
              {:models models
               :result result}))))

(defn guardian
  "Guardian entrypoint used by charon.core/guard."
  [{:keys [conn event models surface opts]}]
  (cond
    (and conn event) (guard-event conn event (assoc (or opts {}) :surface surface))
    (and conn models) (guard-models conn models surface)
    :else (charon/ok (or surface :charon/guard) {:note :charon/no-guard})))
