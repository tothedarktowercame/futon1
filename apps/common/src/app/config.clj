;; apps/common/src/app/config.clj
(ns app.config
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private default
  {:app/data-dir       "data/default"
   :app/snapshot-every 100
   :app/server-port    4000
   :xtdb/enabled?      true
   :xtdb/config-path   "apps/graph-memory/resources/xtdb.edn"
   :warmup/enable?     true
   :warmup/focus-k     5})

(defn- read-edn-file [f]
  (when-let [file (let [candidate (io/file f)]
                    (when (.exists candidate) candidate))]
    (with-open [r (io/reader file)]
      (edn/read (java.io.PushbackReader. r)))))

(defn- env-str [k] (some-> (System/getenv k) str/trim not-empty))
(defn- sysprop-str [k] (some-> (System/getProperty k) str/trim not-empty))

(defn- config-path []
  ;; Allow override of config path itself
  (or (env-str "FUTON_CONFIG")
      (sysprop-str "futon.config")
      "config.edn"))

(defn load-config []
  (let [cfg-file (config-path)
        file-map (or (read-edn-file cfg-file) {})
        ;; ENV overrides for common keys
        env-map  (cond-> {}
                   (env-str "BASIC_CHAT_DATA_DIR")
                   (assoc :app/data-dir (env-str "BASIC_CHAT_DATA_DIR"))

                   (env-str "APP_SERVER_PORT")
                   (assoc :app/server-port (Long/parseLong (env-str "APP_SERVER_PORT")))

                   (env-str "XTDB_CONFIG")
                   (assoc :xtdb/config-path (env-str "XTDB_CONFIG")))

        sys-map  (cond-> {}
                   (sysprop-str "basic.chat.data.dir")
                   (assoc :app/data-dir (sysprop-str "basic.chat.data.dir"))

                   (sysprop-str "app.server.port")
                   (assoc :app/server-port (Long/parseLong (sysprop-str "app.server.port")))

                   (sysprop-str "xtdb.config")
                   (assoc :xtdb/config-path (sysprop-str "xtdb.config")))]
    (merge default file-map env-map sys-map)))

(defonce ^:private !cfg (atom nil))
(defonce ^:private logged? (atom false))

(defn config []
  (or @!cfg (reset! !cfg (load-config))))

(defn data-dir ^java.io.File []
  (let [p (:app/data-dir (config))
        f (io/file p)]
    (.mkdirs f)
    (.getAbsoluteFile f)))

(defn log-once! []
  (when (compare-and-set! logged? false true)
    (println "[config] file:" (config-path))
    (println "[config] data-dir:" (.getPath (data-dir)))
    (println "[config] port:" (:app/server-port (config)))))
