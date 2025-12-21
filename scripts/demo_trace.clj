(ns demo-trace
  "Interactive demo runner that records focus-header + profile taps into trace files."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [client.api :as api])
  (:gen-class))

(def ^:private exit-commands
  #{":quit" ":exit" "quit" "exit" "q"})

(defn- now [] (System/currentTimeMillis))

(defn- default-session []
  (format "session-%d" (now)))

(defn- parse-args [args]
  (loop [opts {:session (default-session)
               :storage (str (System/getProperty "user.home")
                              "/code/storage/futon1-traces")}
         xs args]
    (if-let [arg (first xs)]
      (case arg
        "--session" (recur (assoc opts :session (second xs)) (nnext xs))
        "--storage" (recur (assoc opts :storage (second xs)) (nnext xs))
        "--data-root" (recur (assoc opts :data-root (second xs)) (nnext xs))
        "--profile" (recur (assoc opts :profile (second xs)) (nnext xs))
        (throw (ex-info (str "Unknown argument: " arg) {:arg arg})))
      opts)))

(defn- ensure-dir [path]
  (let [f (io/file path)]
    (.mkdirs f)
    (.getAbsolutePath f)))

(defn- session-dir [storage-root session]
  (let [dir (io/file storage-root session)]
    (.mkdirs dir)
    dir))

(defn- append-event! [file data]
  (spit file (str (pr-str data) "\n") :append true))

(defn- install-tap! [dir]
  (let [tap-file (doto (io/file dir "tap-events.edn")
                   (some-> .getParentFile .mkdirs))
        tap-fn (fn [event]
                 (when (and (map? event) (:event event))
                   (append-event! tap-file (assoc event :logged-at (now))))) ]
    (add-tap tap-fn)
    tap-fn))

(defn- log-turn! [dir line {:keys [type data]}]
  (let [entry {:timestamp (now)
               :input line
               :type type
               :data (select-keys data [:bot-lines :focus-header-lines :message :turn])}
        file (doto (io/file dir "turns" (str (:timestamp entry) ".edn"))
               (-> .getParentFile .mkdirs))]
    (spit file (pr-str entry))))

(defn- render-output [{:keys [type data]}]
  (case type
    :say   (do
             (doseq [line (:bot-lines data)]
               (println "bot>" line))
             (when-let [fh-lines (:focus-header-lines data)]
               (doseq [line fh-lines]
                 (println "fh>" line))))
    :slash (doseq [line (:message data)] (println "bot>" line))
    :bang  (println "bot>" (:message data))
    :blank nil
    (println "bot>" data)))

(defn- start-session [{:keys [data-root profile]}]
  (let [opts (cond-> {}
                data-root (assoc :data-root data-root)
                profile (assoc :profile profile))]
    (api/start opts)))

(defn- run-loop [session trace-dir]
  (println "Tracing demo mode. Type :quit to exit.")
  (loop []
    (print "you> ")
    (flush)
    (if-let [line (read-line)]
      (let [trimmed (str/trim line)]
        (cond
          (str/blank? trimmed) (recur)
          (exit-commands trimmed) (println "Goodbye!")
          :else (let [result (api/run-line session trimmed)]
                  (render-output result)
                  (log-turn! trace-dir trimmed result)
                  (recur))))
      (println "Goodbye!"))))

(defn -main [& args]
  (let [{:keys [session storage] :as opts} (parse-args args)
        trace-dir (session-dir storage session)
        tap-fn (install-tap! trace-dir)
        session (start-session opts)]
    (println "Tracing to" (.getAbsolutePath trace-dir))
    (try
      (run-loop session trace-dir)
      (finally
        (remove-tap tap-fn)
        (api/stop session)))))

(when (= (System/getProperty "babashka.file") *file*)
  (apply -main *command-line-args*))
