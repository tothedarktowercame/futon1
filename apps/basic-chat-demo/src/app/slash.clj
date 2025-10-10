(ns app.slash
  (:require [app.store :as store]
            [clojure.string :as str]
            [graph-memory.main :as gm])
  (:import (java.time Instant)))

(def ^:private help-lines
  ["Slash commands:"
   "  /tail [n]      Show the last n relations (default 5)"
   "  /ego NAME      Display neighbors connected to the named entity"
   "  /cooccur NAME  Show entities that co-occur with the named entity"
   "  /help          Show this help message"]) 

(defn- type-label [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    (nil? t) nil
    :else (str t)))

(defn- name-with-type [name type]
  (if (and name type)
    (str name " (" (type-label type) ")")
    name))

(defn- format-timestamp [ts]
  (when (number? ts)
    (try
      (.toString (Instant/ofEpochMilli (long ts)))
      (catch Exception _ nil))))

(defn- parse-int [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ nil)))

(defn- relation-label [t]
  (or (type-label t) "?"))

(defn- format-tail-line [{:keys [src dst type confidence last-seen]}]
  (let [src-name (or (:name src) "?")
        dst-name (or (:name dst) "?")
        relation (relation-label type)
        extras (->> [(when confidence (format "conf %.2f" (double confidence)))
                     (when-let [ts (format-timestamp last-seen)] (str "seen " ts))]
                    (remove nil?)
                    (str/join ", "))]
    (str "  - " src-name " —" relation "→ " dst-name
         (when (seq extras)
           (str " (" extras ")")))))

(defn- format-ego-line [{:keys [relation direction entity]} focal]
  (let [relation (relation-label relation)
        neighbor-name (name-with-type (:entity/name entity) (:entity/type entity))
        focus-name (name-with-type (:name focal) (:type focal))]
    (if (= direction :out)
      (str "  - " focus-name " —" relation "→ " neighbor-name)
      (str "  - " neighbor-name " —" relation "→ " focus-name))))

(defn- tail-section [conn limit]
  (let [rels (store/recent-relations conn limit)]
    (if (seq rels)
      (into ["Recent relations:"] (map format-tail-line rels))
      ["Recent relations:" "  (none)"])))

(defn- ego-section [conn name]
  (if (str/blank? name)
    ["Usage: /ego <entity>"]
    (if-let [entity (store/resolve-name->eid conn name)]
      (let [neighbors (gm/neighbors conn (:id entity))
            outgoing (filter #(= :out (:direction %)) neighbors)
            incoming (filter #(= :in (:direction %)) neighbors)
            header (str "Entity: " (name-with-type (:name entity) (:type entity)))
            out-lines (if (seq outgoing)
                        (into ["Outgoing:"] (map #(format-ego-line % entity) outgoing))
                        ["Outgoing:" "  (none)"])
            in-lines (if (seq incoming)
                       (into ["Incoming:"] (map #(format-ego-line % entity) incoming))
                       ["Incoming:" "  (none)"])]
        (into [header] (concat out-lines in-lines)))
      [(str "entity not found: " name)])))

(defn- cooccur-section [conn name]
  (if (str/blank? name)
    ["Usage: /cooccur <entity>"]
    (if-let [entity (store/resolve-name->eid conn name)]
      (let [rows (store/cooccurring-entities conn (:id entity))
            header (str "Co-occurrences with " (:name entity) ":")
            lines (if (seq rows)
                    (map (fn [{:keys [name type count]}]
                           (str "  - " (name-with-type name type) " (" count ")"))
                         rows)
                    ["  (none)"])]
        (into [header] lines))
      [(str "entity not found: " name)])))

(defn handler
  "Return a slash-command handler bound to the Datascript connection."
  [conn]
  (fn [raw state]
    (let [trimmed (str/trim raw)]
      (if (str/blank? trimmed)
        {:message help-lines :new-state state}
        (let [[cmd & _] (str/split trimmed #"\s+" 2)
              cmd (str/lower-case cmd)
              space-idx (.indexOf trimmed " ")
              arg (if (neg? space-idx) "" (subs trimmed (inc space-idx)))
              arg (str/trim arg)]
          (case cmd
            "tail" (let [limit (parse-int (first (str/split arg #"\s+")))]
                     {:message (tail-section conn (or limit 5))
                      :new-state state})
            "ego" {:message (ego-section conn arg)
                   :new-state state}
            "cooccur" {:message (cooccur-section conn arg)
                        :new-state state}
            "help" {:message help-lines
                     :new-state state}
            {:message (str "unknown command: /" cmd)
             :new-state state}))))))
