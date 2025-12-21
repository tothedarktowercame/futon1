(ns tatami.repl
  (:require [app.chat :as chat]    ; <- your real ns
            [app.config :as cfg]))

(defonce !state (atom {:profile (cfg/default-profile)
                       :ctx     (chat/empty-ctx)}))

(defn say [s]
  (let [{:keys [profile ctx]} @!state
        {:keys [message fh ctx']} (chat/run-turn {:profile profile
                                                  :ctx ctx
                                                  :text s})]
    (swap! !state assoc :ctx ctx')
    (println message)
    (when fh (println "[focus]" fh))
    {:message message :fh fh}))
