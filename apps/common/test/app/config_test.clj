;; Verifies that the shared config loader honours defaults and override order.
(ns app.config-test
  (:require [app.config :as cfg]
            [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-config-file [data]
  (let [path (Files/createTempFile "cfg-test" ".edn" (make-array FileAttribute 0))
        file (.toFile path)]
    (spit file (pr-str data))
    file))

(defn- cleanup-file [^java.io.File f]
  (when (.exists f)
    (io/delete-file f true)))

(deftest load-config-uses-defaults-when-missing
  (testing "missing config file falls back to defaults"
    (with-redefs-fn {#'cfg/config-path (constantly "does-not-exist.edn")
                     #'cfg/env-str (constantly nil)
                     #'cfg/sysprop-str (constantly nil)}
      (fn []
        (let [result (cfg/load-config)]
          (is (= "data/default" (:app/data-dir result)))
          (is (= 4000 (:app/server-port result)))
          (is (= "apps/graph-memory/resources/xtdb.edn"
                 (:xtdb/config-path result))))))))

(deftest load-config-applies-precedence
  (testing "file < env < sysprop precedence"
    (let [cfg-file (temp-config-file {:app/server-port 5555
                                      :custom/value :file})]
      (try
        (with-redefs [cfg/config-path (constantly (.getAbsolutePath cfg-file))
                      cfg/env-str (fn [k]
                                    (case k
                                      "APP_SERVER_PORT" "6000"
                                      "XTDB_CONFIG" "env.edn"
                                      nil))
                      cfg/sysprop-str (fn [k]
                                        (case k
                                          "app.server.port" "7777"
                                          nil))]
          (let [result (cfg/load-config)]
            (is (= 7777 (:app/server-port result)) "sysprop overrides env and file")
            (is (= "env.edn" (:xtdb/config-path result)) "env overrides file")
            (is (= :file (:custom/value result)) "file values still merged")))
        (finally
          (cleanup-file cfg-file))))))
