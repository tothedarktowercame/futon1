(ns app.cli-runner
  (:require
   [api.handlers.turns :as turns]
   [app.slash.format :as fmt]
   [app.store-manager :as stores]
   [clojure.string :as str]))

(defn- extract-lines [out]
  (let [rels (seq (:relations out))
        ents (seq (:entities out))]
    (cond
      rels (into ["Extracted relations:"]
                 (map #(str "  " %) (mapcat fmt/relation-lines rels)))
      ents (into ["Extracted entities:"]
                 (map #(str "  " %) (mapcat fmt/entity-lines ents)))
      :else ["No structured data extracted."])))

(defn- focus-lines-from-out [fh]
  (let [ents (:entities fh)]
    (when (seq ents)
      (let [labels (->> ents
                        (map (fn [entity]
                               (let [entity-name (:name entity)
                                     entity-type (:type entity)]
                                 (if entity-type
                                   (str entity-name " (" (clojure.core/name entity-type) ")")
                                   (str entity-name)))))
                        (remove #(or (nil? %) (str/blank? %)))
                        (str/join ", "))]
        [(str "FOCUS: " labels)
         (str "INTENT: " (or (:intent fh) "—"))]))))

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
     :focus-header-lines (focus-lines-from-out (:focus_header out))
     :new-state          (assoc state :last-turn out)}))
