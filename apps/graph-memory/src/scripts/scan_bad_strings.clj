;; apps/graph-memory/src/scripts/scan_bad_strings.clj
(ns scripts.scan-bad-strings
  "Scan the Datascript store for banned string values (blank or \"external\")."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d])
  (:import (java.io File)))

(defn- usage []
  (str "Usage: clojure -M -m scripts.scan-bad-strings [--out PATH]\n"))

(defn- parse-args [args]
  (loop [opts {:out nil}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--out" (recur (assoc opts :out (second remaining))
                       (nnext remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- bad-string? [value]
  (when (string? value)
    (let [trimmed (str/trim value)
          lowered (str/lower-case trimmed)]
      (or (str/blank? trimmed)
          (= lowered "external")))))

(defn- bad-strings
  ([value] (bad-strings [] value))
  ([path value]
   (cond
     (map? value)
     (mapcat (fn [[k v]]
               (bad-strings (conj path k) v))
             value)

     (sequential? value)
     (mapcat (fn [[idx v]]
               (bad-strings (conj path idx) v))
             (map-indexed vector value))

     (bad-string? value)
     [{:path path :value value}]

     :else [])))

(defn- entity-summary [db eid]
  (let [summary (d/pull db '[:entity/id :entity/type :entity/name
                            :relation/id :relation/type
                            :utterance/id :intent/id :trail/id
                            :mention/id :link/id]
                        eid)]
    (cond-> summary
      (empty? summary) (assoc :db/eid eid))))

(defn- scan-db [db]
  (let [datoms (d/datoms db :eavt)
        results (->> datoms
                     (mapcat (fn [datom]
                               (let [eid (:e datom)
                                     attr (:a datom)
                                     value (:v datom)]
                                 (->> (bad-strings value)
                                      (map (fn [bad]
                                             {:db/eid eid
                                              :attr attr
                                              :path (:path bad)
                                              :value (:value bad)}))))))
                     vec)
        by-eid (group-by :db/eid results)]
    (mapv (fn [[eid issues]]
            {:entity (entity-summary db eid)
             :issues (vec issues)})
          by-eid)))

(defn -main [& args]
  (let [opts (parse-args args)]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (let [{:keys [data-root xtdb default-profile]} (store-manager/config)
          profile (or default-profile "default")
          data-root (or data-root (str (io/file (System/getProperty "user.dir") "data")))
          profile-dir (-> (io/file (str data-root) (str profile)) .getAbsolutePath)
          conn (store/restore! {:data-dir profile-dir :xtdb xtdb})
          db (d/db conn)
          report {:generated-at (System/currentTimeMillis)
                  :profile profile
                  :issues (scan-db db)}]
      (try
        (if-let [out (:out opts)]
          (spit (io/file out) (pr-str report))
          (prn report))
        (finally
          (store-manager/shutdown!))))))
