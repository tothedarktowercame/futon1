(ns protocols.registry
  (:require [protocols.basic-chat.v1 :as v1]
            [protocols.basic-chat.v2 :as v2]
            [protocols.basic-chat.v3 :as v3]))

(def registry
  {"basic-chat/v1" {:id "basic-chat/v1"
                     :init v1/init
                     :handle v1/handle
                     :intro v1/intro}
   "basic-chat/v2" {:id "basic-chat/v2"
                     :init v2/init
                     :handle v2/handle
                     :command-handler v2/command-handler
                     :intro v2/intro}
   "basic-chat/v3" {:id "basic-chat/v3"
                     :init v3/init
                     :handle v3/handle
                     :intro v3/intro}})

(defn fetch [protocol-id]
  (get registry protocol-id))

(defn known-ids []
  (keys registry))
