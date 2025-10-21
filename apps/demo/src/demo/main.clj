(ns demo.main
  (:require [client.api :as api]
            [clojure.string :as str])
  (:gen-class))

(def exit-commands #{":quit" ":exit" "quit" "exit" "q"})

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

(defn -main
  [& _]
  (println "client demo interactive mode")
  (println "Type your message and press enter. Use :quit to exit.")
  (let [session (api/start {})]
    (try
      (loop []
        (print "you> ")
        (flush)
        (if-let [line (read-line)]
          (let [trimmed (str/trim line)]
            (when-not (str/blank? trimmed)
              (if (exit-commands trimmed)
                (do (println "Goodbye!") (api/stop session))
                (do (render-output (api/run-line session trimmed))
                    (recur)))))
          (do (println "Goodbye!") (api/stop session))))
      (finally
        (api/stop session)))))
