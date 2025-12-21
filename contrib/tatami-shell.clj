(ns tatami.shell
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (java.lang ProcessBuilder)))

;;; Docs:

;; (require '[tatami.shell :as ts])

;; (ts/start!)                          ; idempotent
;; (ts/say "I love Clojure")            ; goes to the same CLI you run in EAT
;; (ts/slash :tail 20)                  ; -> "/tail 20"
;; (ts/bang "!entity add e123 :name \"Joe\"")
;; (ts/stop!)

;;; Code:

(defonce ^:private ^java.lang.Process !proc (atom nil))
(defonce ^:private ^OutputStreamWriter !in (atom nil))
(defonce ^:private !out-thread (atom nil))
(defonce ^:private !err-thread (atom nil))

(def ^:dynamic *workdir* (str (System/getProperty "user.home")
                              "/code/futon1/apps/demo"))

(def ^:dynamic *env* {"BASIC_CHAT_DATA_DIR" "data"}) ; customise per dataset/project

(def ^:dynamic *cmd*
  ;; Same command you'd run in EAT/vterm:
  ["clojure" "-M:run-m"])

(defn- ^ProcessBuilder pb []
  (doto (ProcessBuilder. ^java.util.List *cmd*)
    (.directory (java.io.File. *workdir*))
    (.redirectErrorStream false)
    (.environment (doto (.environment (ProcessBuilder.))
                    (as-> env
                      (doseq [[k v] *env*] (.put env k v)))))))

(defn- start-reader-thread! [^BufferedReader rdr label handler]
  (let [t (Thread.
           (fn []
             (try
               (loop []
                 (when-let [line (.readLine rdr)]
                   (handler line)
                   (recur)))
               (catch Throwable _)
               (finally
                 (.close rdr))))
           (str "tatami-shell-" label))]
    (.start t)
    t))

(defn start! 
  "Start the Futon CLI as a subprocess and wire streams for the REPL.
   Idempotent: does nothing if already running."
  ([] (start! {}))
  ([{:keys [workdir env cmd on-out on-err]
     :or {on-out (fn [line]
                   (cond
                     (str/starts-with? line "fh>")
                     (println (str "\u001B[1m" ; bold
                                   "[focus] "
                                   (subs line 3)))
                     :else
                     (println line)))
          on-err (fn [line] (binding [*out* *err*] (println line)))}}]
   (when (nil? @!proc)
     (alter-var-root #'*workdir* (constantly (or workdir *workdir*)))
     (alter-var-root #'*env* (constantly (or env *env*)))
     (alter-var-root #'*cmd* (constantly (or cmd *cmd*)))
     (let [builder (ProcessBuilder. ^java.util.List *cmd*)
           _ (.directory builder (java.io.File. ^String *workdir*))
           envm (.environment builder)]
       (doseq [[k v] *env*] (.put envm k v))
       (let [p (.start builder)
             in-w (OutputStreamWriter. (.getOutputStream p))
             out-r (BufferedReader. (InputStreamReader. (.getInputStream p)))
             err-r (BufferedReader. (InputStreamReader. (.getErrorStream p)))]
         (reset! !proc p)
         (reset! !in in-w)
         (reset! !out-thread (start-reader-thread! out-r "stdout" on-out))
         (reset! !err-thread (start-reader-thread! err-r "stderr" on-err))
         :started)))))

(defn stop!
  "Stop the subprocess + threads."
  []
  (when-let [p @!proc]
    (try (.destroy p) (catch Throwable _)))
  (when-let [t @!out-thread] (try (.interrupt t) (catch Throwable _)))
  (when-let [t @!err-thread] (try (.interrupt t) (catch Throwable _)))
  (reset! !proc nil)
  (reset! !in nil)
  (reset! !out-thread nil)
  (reset! !err-thread nil)
  :stopped)

(defn alive? [] (some? @!proc))

(defn send-line!
  "Low-level: send a raw line to the CLI (slash/bang commands allowed)."
  [s]
  (when-not (alive?) (start!))
  (let [^OutputStreamWriter w @!in]
    (.write w (str s "\n"))
    (.flush w))
  :ok)

;; High-level helpers you can call from the REPL:

(defn say
  "Send a natural-language message."
  [s] (send-line! s))

(defn slash
  "Send a /command (string or keyword/symbol)."
  [cmd & args]
  (send-line! (str "/" (name cmd)
                   (when (seq args)
                     (str " " (str/join " " args))))))

(defn bang
  "Send a !form (already formatted). Example: (bang \"!entity add ...\")."
  [form] (send-line! form))
