(ns scripts.inspect-pattern
  "Inspect a Futon pattern by slug, printing its summary and component snippets."
  (:require [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d]))

(defn- preview [text]
  (let [normalized (-> (or text "") str/trim (str/replace #"\s+" " "))]
    (if (<= (count normalized) 60)
        normalized
        (str (subs normalized 0 57) "..."))))

(defn- pattern-entity [db slug]
  (ffirst
   (d/q '[:find (pull ?e [:db/id :entity/id :entity/name :entity/source])
          :in $ ?name
          :where
          [?e :entity/name ?name]
          [?e :entity/type :pattern/library]]
        db slug)))

(defn- pattern-components [db pattern-db-id]
  (map first
       (d/q '[:find (pull ?rel [:relation/id :relation/props
                                {:relation/dst [:entity/id :entity/name :entity/source]}])
              :in $ ?pattern
              :where
              [?rel :relation/src ?pattern]
              [?rel :relation/type :pattern/includes]
              [?rel :relation/dst ?dst]]
            db pattern-db-id)))

(defn- normalize-label [value]
  (when value
    (let [raw (-> (str value)
                  str/lower-case
                  str/trim)
          stripped (-> raw
                       (str/replace #"^[^a-z0-9]+" "")
                       (str/replace #"[^a-z0-9]+" "-")
                       (str/replace #"^-+|-+$" "")
                       (str/replace #"^\d+-" ""))]
      stripped)))

(defn- component-label-from-name [component]
  (or (normalize-label (second (re-find #"/\\d{2,}-([^/]+)$" (str (:entity/name component)))))
      (normalize-label (last (str/split (str (:entity/name component)) #"/")))))

(defn -main [& [slug]]
  (if (nil? slug)
      (do
        (println "Usage: clojure -M:scripts/inspect-pattern <pattern-slug>")
        (System/exit 1))
      (let [profile (store-manager/default-profile)
            conn (store-manager/conn profile)
            db   @conn]
        (try
          (if-let [pattern (pattern-entity db slug)]
              (let [components (pattern-components db (:db/id pattern))]
                (println (format "[%s] %s (%s)" profile (:entity/name pattern) (:entity/id pattern)))
                (println (format "  summary: %s" (preview (:entity/source pattern))))
                (if (seq components)
                    (doseq [rel (sort-by (comp :entity/name :relation/dst) components)]
                      (let [component (:relation/dst rel)
                            label-prop (get-in rel [:relation/props :pattern/component-label])
                            label-name (component-label-from-name component)]
                        (println (format "  - %s (%s): %s"
                                         (:entity/name component)
                                         (:entity/id component)
                                         (preview (:entity/source component))))
                        (println (format "      label-prop=%s label-name=%s"
                                         (or label-prop "nil")
                                         (or label-name "nil")))))
                    (println "  (no components)")))
              (println (format "Pattern %s not found in profile %s" slug profile)))
          (finally
            (store-manager/shutdown!))))))
