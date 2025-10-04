(ns nlp-interface.ner-v4
  "Tiered NER pipeline for basic-chat/v4."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def resource-paths
  {:gazetteer {:people "gazetteer/people.edn"
               :places "gazetteer/places.edn"
               :orgs "gazetteer/orgs.edn"
               :projects "gazetteer/projects.edn"
               :tools "gazetteer/tools.edn"}
   :patterns "patterns.edn"
   :stoplists "stoplists.edn"
   :cache {:dir ".graphmem" :file "entities/user-cache.edn"}})

(defn- read-edn [path]
  (some-> path io/resource slurp edn/read-string))

(defn load-resources []
  {:gazetteer (into {}
                    (for [[k p] (:gazetteer resource-paths)]
                      [k (or (read-edn p) [])]))
   :patterns (or (read-edn (:patterns resource-paths)) {})
   :stoplists (or (read-edn (:stoplists resource-paths)) {})})

(defn recognize-entities
  "Placeholder recognizer; returns an empty vector for now."
  [_resources _tokens _pos-tags _text _now]
  [])
*** End PATCH
