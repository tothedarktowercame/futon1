(ns app.bang
  (:require [app.commands :as svc]))

(defn bang-handler
  "Adapter from (cmd state ctx) -> {:message .. :new-state ..}
   to svc/handle's (conn opts line)."
  [cmd state {:keys [conn env profile]}]
  (let [opts {:profile profile
              :env     env
              :state   state
              :interactive? true}
        {:keys [message result] :as out} (svc/handle conn opts cmd)]
    {:message   (or message result (str "ok: " cmd))
     :new-state (assoc state :last-bang {:cmd cmd :out out})}))
