(ns app.scripts.salience-demo
  "Show that XT-backed salience survives process restarts."
  (:require [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint])
  (:import (java.util UUID)))

(def ^:private sample-id
  (UUID/fromString "5a5ca47a-73db-4e44-b375-9381e2a1d2f7"))

(defn- config-path []
  (or (some-> (System/getenv "XT_CONFIG") io/file .getAbsolutePath)
      (.getAbsolutePath (io/file "resources" "xtdb.edn"))))

(defn- data-dir []
  (let [override (System/getenv "SAL_DEMO_DIR")]
    (if override
      (.getAbsolutePath (io/file override))
      (.getAbsolutePath (io/file "target" "salience-demo")))))

(defn- summarize [doc]
  (some-> doc
          (select-keys [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen :entity/pinned?])))

(defn- ensure-sample! []
  (if-let [doc (xt/entity sample-id)]
    doc
    (let [now (System/currentTimeMillis)
          sample {:entity/id sample-id
                  :entity/name "Serena"
                  :entity/type :person
                  :entity/pinned? true
                  :entity/seen-count 1
                  :entity/last-seen now}]
      (xt/put-entity! sample)
      (xt/entity sample-id))))

(defn- bump-salience! [doc]
  (let [doc (or doc (ensure-sample!))
        now (System/currentTimeMillis)
        seen (inc (long (or (:entity/seen-count doc) 0)))
        updated (assoc doc
                       :entity/seen-count seen
                       :entity/last-seen now)]
    (xt/put-entity! updated)
    (xt/entity sample-id)))

(defn- start-node! [label dir]
  (println (format "[%s] starting XT node" label))
  (xt/start! (config-path) {:data-dir dir
                            :xt/created-by (str "salience-demo:" label)}))

(defn- stop-node! [label]
  (println (format "[%s] stopping XT node" label))
  (xt/stop!))

(defn- xt-db []
    (xt/db (xt/node)))

(defn -main [& _args]
  (let [dir (data-dir)]
    (.mkdirs (io/file dir))
    (println "Using data directory" dir)
    (start-node! "boot-1" dir)
    (let [seeded (summarize (ensure-sample!))]
      (println "[boot-1] seeded entity:")
      (pprint/pprint seeded))
    (stop-node! "boot-1")
    (println "Restarting against the same directory...")
    (start-node! "boot-2" dir)
    (let [after-restart (summarize (or (xt/entity sample-id)
                                       (ensure-sample!)))]
      (println "[boot-2] entity after restart (no reconfiguration):")
      (pprint/pprint after-restart)
      (println "[boot-2] bumping salience so the next run observes the increment...")
      (pprint/pprint (summarize (bump-salience! after-restart))))
    ;; added
    (let [db (xt/db (xt/node))]
      (println "total docs:" (count (xt/q db '{:find [e] :where [[e _ _]]})))
      (println "entities:"   (count (xt/q db '{:find [e] :where [[e :entity/id _]]})))
      (println "relations:"  (count (xt/q db '{:find [r] :where [[r :relation/id _]]}))))
    (stop-node! "boot-2")
    (println "Done. Re-run the script to watch :entity/seen-count increment between boots.")))
