(ns protocols.basic-chat.v5
  (:require [protocols.basic-chat.v4 :as v4]))

(def intro
  ["Protocol basic-chat/v5 — XT-backed focus header with v4 NER pipeline."
   "Entities and relations mirror into XTDB and populate the focus header stream."
   "Use --fh or --fh-only to inspect the focus header payload after each turn."
   "Slash commands: /tail, /ego, /cooccur, and /help surface XTDB highlights."])

(defn init []
  (v4/init))

(defn configure [ctx opts]
  (v4/configure ctx opts))

(defn handle [ctx line ts]
  (v4/handle ctx line ts))

