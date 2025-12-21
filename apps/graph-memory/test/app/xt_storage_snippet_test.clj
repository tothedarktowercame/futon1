(ns app.xt-storage-snippet-test
  (:require [app.xt :as xt]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [graph-memory.types-registry :as types]
            [xtdb.api :as xtdb])
  (:import (java.io File)
           (java.nio.file Files Path)))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "xt-snippet" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-recursively [^File f]
  (when (.isDirectory f)
    (run! delete-recursively (.listFiles f)))
  (io/delete-file f true))

(deftest xt-storage-snippet-test
  (let [dir (temp-dir)
        cfg (-> (io/resource "xtdb.edn") io/file .getAbsolutePath)
        entity {:xt/id :demo/id
                :entity/id :demo/id
                :entity/name "Demo"
                :entity/type :demo/type
                :entity/seen-count 1
                :entity/last-seen (System/currentTimeMillis)
                :entity/pinned? true}]
    (alter-var-root #'types/!cache (constantly (atom nil)))
    (try
      (xt/start! cfg {:data-dir (.getAbsolutePath dir)
                      :xt/created-by "xt-storage-snippet-test"})
      (xt/submit! [[::xtdb/put entity]])
      (xt/sync-node!)
      (let [db (xtdb/db (xt/node))
            ids (xtdb/q db '{:find [?e]
                             :where [[?e :entity/id _]]})]
        (is (= 1 (count ids)) "entity doc was stored in XTDB"))
      (types/ensure! :entity [:demo/type])
      (types/load-cache!)
      (is (some #(= (:id %) :demo/type) (types/docs)) "type registry stored doc")
      (finally
        (xt/stop!)
        (delete-recursively dir)
        (alter-var-root #'types/!cache (constantly (atom nil)))))))
