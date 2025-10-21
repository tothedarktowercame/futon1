(ns client.runner
  "Runner for the client session that enriches output with focus header details."
  (:require
   [api.handlers.turns :as turns]
   [app.slash.format :as fmt]
   [app.store-manager :as stores]
   [client.focus-header :as focus-header]
   [clojure.string :as str]))

(defn- block-lines [title rows]
  (when (seq rows)
    (into [title]
          (map #(str "  " %)
               rows))))

(defn- extract-lines [out]
  (let [rels (seq (:relations out))
        ents (seq (:entities out))
        entity-block (when ents
                       (block-lines "Extracted entities:"
                                    (mapcat fmt/entity-lines ents)))
        relation-block (when rels
                         (block-lines "Extracted relations:"
                                      (mapcat fmt/relation-lines rels)))
        sections (->> [entity-block relation-block]
                      (remove nil?))]
    (if (seq sections)
      (vec (mapcat identity (interpose [""] sections)))
      ["No structured data extracted."])))

(defn- label [value]
  (cond
    (keyword? value) (-> value name (str/replace #"[_-]" " "))
    (string? value) value
    (nil? value) nil
    :else (str value)))

(defn- confidence-str [conf]
  (when (number? conf)
    (format "%.2f" (double conf))))

(defn- focus-summary-lines [out debug]
  (let [entities (seq (focus-header/current-entities debug))
        labels   (when entities
                   (->> entities
                        (map #(fmt/name-with-type (:name %) (:type %)))
                        (remove #(or (nil? %) (str/blank? %)))
                        (str/join ", ")))
        intent   (:intent out)
        intent-line (when (seq intent)
                      (let [intent-label (or (label (:type intent)) "unspecified")
                            conf (confidence-str (:conf intent))]
                        (if conf
                          (str "INTENT: " intent-label " (confidence " conf ")")
                          (str "INTENT: " intent-label))))
        focus-line (when (seq labels)
                     (str "FOCUS: " labels))]
    (when (or focus-line intent-line)
      (vec (remove nil? [focus-line intent-line])))))

(defn- focus-lines [out conn]
  (let [context (:context out)
        debug   (or (:focus_header_debug out)
                    (try
                      (focus-header/build conn {:entities (:entities out)
                                                :relations (:relations out)}
                                           context
                                           {:recent-limit 5})
                      (catch Throwable _ nil)))
        text-block (when debug (focus-header/text-lines debug))
        summary-block (when debug (focus-summary-lines out debug))
        sections (->> [text-block summary-block]
                      (remove nil?))]
    (when (seq sections)
      (vec (mapcat identity (interpose [""] sections))))))

(defn runner
  [line ts state {:keys [xtdb-node conn env default-profile] :as ctx}]
  (let [profile (or default-profile (stores/default-profile))
        req     {:headers {"x-profile" profile}
                 :query-params {}
                 :ctx (merge {:default-profile profile
                              :xtdb-node xtdb-node
                              :conn conn
                              :env env}
                             ctx)}
        body    {:text line :ts ts :protocol turns/default-protocol}
        out     (turns/process-turn! req body)]
    {:message            (format "ok — %d entities, %d relations"
                                 (count (:entities out)) (count (:relations out)))
     :bot-lines          (extract-lines out)
     :context            (:context out)
     :focus-header-lines (focus-lines out conn)
     :new-state          (assoc state :last-turn out)}))
