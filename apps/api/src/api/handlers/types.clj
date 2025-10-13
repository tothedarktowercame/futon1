(ns api.handlers.types
  (:require [app.command-service :as commands]))

(defn list-types [_]
  (commands/list-types))

(defn set-parent! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (commands/set-type-parent! body))

(defn merge-aliases! [_ body]
  (when-not (map? body)
    (throw (ex-info "Payload must be an object" {:status 400})))
  (commands/merge-aliases! body))
