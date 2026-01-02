(ns baseline-snapshot
  "Generate or refresh the Willie/Jane deterministic snapshot used by FUTON1 tests."
  (:require [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [client.api :as api]
            [datascript.core :as d])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant ZoneId)
           (java.time.format DateTimeFormatter)))

(def scenario-lines
  ["I like Willie Dixon."
   "Jane works at MIT."
   "Remind me about Willie."])

(def instant-formatter
  (.withZone DateTimeFormatter/ISO_OFFSET_DATE_TIME (ZoneId/systemDefault)))

(defn- temp-dir []
  (.toString (Files/createTempDirectory "f1-baseline" (into-array FileAttribute []))))

(defn- delete-tree! [path]
  (when path
    (let [f (io/file path)]
      (when (.exists f)
        (doseq [child (.listFiles f)]
          (if (.isDirectory child)
            (delete-tree! (.getAbsolutePath child))
            (.delete child)))
        (.delete f)))))

(defn- focus-lines [turns]
  (mapv (fn [turn]
          (vec (get-in turn [:data :focus-header-lines])))
        turns))

(defn- summarise-entities [conn]
  (->> (d/q '[:find ?name ?type ?seen
              :where [?e :entity/name ?name]
                     [?e :entity/type ?type]
                     [?e :entity/seen-count ?seen]]
            @conn)
       (map (fn [[name type seen]] {:name name :type type :seen-count seen}))
       (sort-by :name)
       vec))

(defn- summarise-relations [conn]
  (->> (d/q '[:find ?src-name ?rel-type ?dst-name
              :where
              [?src :entity/name ?src-name]
              [?dst :entity/name ?dst-name]
              [?rel :relation/src ?src]
              [?rel :relation/dst ?dst]
              [?rel :relation/type ?rel-type]]
            @conn)
       (map (fn [[src type dst]] {:src src :type type :dst dst}))
       (sort-by (juxt :src :dst :type))
       vec))

(defn- xt->summary [query row->map sort-key]
  (->> (xt/q query)
       (map row->map)
       (sort-by sort-key)
       vec))

(defn- xt-entities []
  (xt->summary '{:find [?name ?type ?seen]
                 :where [[?e :entity/name ?name]
                         [?e :entity/type ?type]
                         [?e :entity/seen-count ?seen]]}
                (fn [[name type seen]] {:name name :type type :seen-count seen})
                :name))

(defn- xt-relations []
  (xt->summary '{:find [?src ?type ?dst]
                 :where [[?rel :relation/src ?src-e]
                         [?rel :relation/dst ?dst-e]
                         [?rel :relation/type ?type]
                         [?src-e :entity/name ?src]
                         [?dst-e :entity/name ?dst]]}
                (fn [[src type dst]] {:src src :type type :dst dst})
                (juxt :src :dst :type)))

(defn- timestamp []
  (.format instant-formatter (Instant/now)))

(defn build-snapshot []
  (let [dir (temp-dir)
        session (api/start {:data-root dir})]
    (try
      (let [turns (mapv #(api/run-line session %) scenario-lines)
            _ (xt/sync-node!)
            ctx ((:ctx-provider session))
            conn (:conn ctx)]
        {:id "baseline/demo-v1"
         :description "Deterministic Willie/Jane scenario verifying focus-header and XTDB mirroring."
         :generated-at (timestamp)
         :script scenario-lines
         :focus-header (focus-lines turns)
         :datascript-entities (summarise-entities conn)
         :datascript-relations (summarise-relations conn)
         :xtdb-entities (xt-entities)
         :xtdb-relations (xt-relations)})
      (finally
        (api/stop session)
        (delete-tree! dir)))))

(defn- parse-args [args]
  (loop [opts {} args args]
    (if-let [arg (first args)]
      (case arg
        "--write" (if-let [path (second args)]
                     (recur (assoc opts :write path) (nnext args))
                     (throw (ex-info "--write requires a path" {})))
        "--stdout" (recur (assoc opts :stdout true) (next args))
        (throw (ex-info (str "Unknown argument: " arg) {:arg arg})))
      opts)))

(defn -main [& args]
  (let [{:keys [write]} (parse-args args)
        snapshot (build-snapshot)]
    (if write
      (do
        (io/make-parents write)
        (with-open [w (io/writer write)]
          (binding [*out* w]
            (pprint/pprint (dissoc snapshot :generated-at))))
        (println "Wrote baseline snapshot to" write))
      (pprint/pprint snapshot))))
