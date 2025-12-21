(ns app.trail-store-test
  (:require [app.store :as store]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import (java.io File)))

(defn- temp-dir []
  (doto (File. (System/getProperty "java.io.tmpdir")
               (str (gensym "trail-store-")))
    (.mkdirs)))

(defn- delete-recursive [^File dir]
  (when (.exists dir)
    (doseq [file (.listFiles dir)]
      (if (.isDirectory file)
        (delete-recursive file)
        (io/delete-file file)))
    (io/delete-file dir true)))

(deftest record-trail-persists-entry
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :xtdb {:enabled? false}}
        conn (store/restore! env)
        trail {:session-id "S-123"
               :turn-id "T-001"
               :intent "Describe bridges"
               :fruits [{:fruit/id :doable}]
               :paramitas [{:paramita/id :truth}]}
        result (store/record-trail! conn env trail)
        trails (store/recent-trails conn 5)]
    (try
      (is (= "S-123" (:trail/session-id result)))
      (is (= "T-001" (:trail/turn-id result)))
      (is (= 1 (count trails)))
      (is (= "S-123" (:session-id (first trails))))
      (finally
        (delete-recursive dir)))))
