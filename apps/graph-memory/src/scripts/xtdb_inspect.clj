(ns scripts.xtdb_inspect
  (:require [app.xt :as xt]
            [clojure.pprint :refer [pprint]]
            [graph-memory.types-registry :as types]
            [xtdb.api :as xtdb])
  (:gen-class))

(defn- default-data-dir []
  (str (System/getProperty "user.dir") "/apps/demo/data/default"))

(defn -main [& [dir & _]]
  (let [data-dir (or (System/getenv "XTDB_INSPECT_DATA") dir (default-data-dir))
        cfg (str (System/getProperty "user.dir") "/apps/graph-memory/resources/xtdb.edn")]
    (println "Inspecting XTDB data at" data-dir)
    (try
      (xt/start! cfg {:data-dir data-dir
                      :xt/created-by "scripts.xtdb-inspect"})
      (let [db (xtdb/db (xt/node))
            entities (xtdb/q db '{:find [(pull ?e [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen :entity/pinned?])]
                                    :where [[?e :entity/id _]]})
            relations (xtdb/q db '{:find [(pull ?r [:relation/id :relation/type :relation/src :relation/dst :relation/last-seen])]
                                     :where [[?r :relation/id _]]})]
        (println "Entities (" (count entities) "):")
        (doseq [doc (take 5 entities)] (pprint doc))
        (println "Relations (" (count relations) "):")
        (doseq [doc (take 5 relations)] (pprint doc))
        (let [cache (types/load-cache!)]
          (println "Type docs:" (count (:docs cache)))
          (doseq [doc (take 5 (:docs cache))] (pprint doc))))
      (finally
        (xt/stop!)))))
