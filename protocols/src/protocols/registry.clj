(ns protocols.registry
  (:require [clojure.string :as str]))

(def ^:private log-skips?
  (boolean (some-> (System/getenv "TRACE_PROTOCOLS") str/trim not-empty)))

(defn- safe-resolve [sym]
  (when sym
    (try
      (requiring-resolve sym)
      (catch Throwable t
        (when log-skips?
          (println (format "[protocols] skipping %s (%s)" sym (.getMessage t))))
        nil))))

(defn- register [{:keys [id init handle configure intro command-handler]}]
  (let [init* (safe-resolve init)
        handle* (safe-resolve handle)
        configure* (safe-resolve configure)
        intro* (safe-resolve intro)
        command-handler* (safe-resolve command-handler)]
    (when (and id init* handle*)
      (cond-> {:id id
               :init init*
               :handle handle*}
        configure* (assoc :configure configure*)
        intro* (assoc :intro intro*)
        command-handler* (assoc :command-handler command-handler*)))))

(def ^:private protocol-specs
  [{:id "basic-chat/v1"
    :init 'protocols.basic-chat.v1/init
    :handle 'protocols.basic-chat.v1/handle
    :intro 'protocols.basic-chat.v1/intro}
   {:id "basic-chat/v2"
    :init 'protocols.basic-chat.v2/init
    :handle 'protocols.basic-chat.v2/handle
    :command-handler 'protocols.basic-chat.v2/command-handler
    :intro 'protocols.basic-chat.v2/intro}
   {:id "basic-chat/v3"
    :init 'protocols.basic-chat.v3/init
    :handle 'protocols.basic-chat.v3/handle
    :intro 'protocols.basic-chat.v3/intro}
   {:id "basic-chat/v4"
    :init 'protocols.basic-chat.v4/init
    :configure 'protocols.basic-chat.v4/configure
    :handle 'protocols.basic-chat.v4/handle
    :intro 'protocols.basic-chat.v4/intro}
   {:id "basic-chat/v5"
    :init 'protocols.basic-chat.v5/init
    :configure 'protocols.basic-chat.v5/configure
    :handle 'protocols.basic-chat.v5/handle
    :intro 'protocols.basic-chat.v5/intro}
   {:id "basic-chat/v6"
    :init 'protocols.basic-chat.v6/init
    :configure 'protocols.basic-chat.v6/configure
    :handle 'protocols.basic-chat.v6/handle
    :intro 'protocols.basic-chat.v6/intro}])

(def registry
  (->> protocol-specs
       (keep (fn [spec]
               (when-let [entry (register spec)]
                 [(:id entry) entry])))
       (into {})))

(defn fetch [protocol-id]
  (get registry protocol-id))

(defn known-ids []
  (keys registry))
