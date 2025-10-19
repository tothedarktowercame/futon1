(ns app.http-client
  (:require [clj-http.lite.client :as client]
            [clojure.data.json :as json]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def exit-commands #{ ":quit" ":exit" "quit" "exit"})
(def default-profile "default")

(defn- print-bot-lines [lines]
  (when (seq lines)
    (let [[first-line & more] lines]
      (println (str "bot> " first-line))
      (doseq [line more] (println (str "     " line))))))

(defn- probe-server [base-url headers]
  (let [diag-url (str base-url "/api/__diag")]
    (try
      (client/get diag-url {:headers headers
                            :socket-timeout 2000
                            :conn-timeout 2000})
      true
      (catch Exception _ false))))

(defn- parse-json-body [response]
  (json/read-str (:body response) :key-fn keyword))

(defn- process-lines-response
  "Parse the response body and print any lines-like payload. Returns the parsed map."
  [response]
  (let [body (parse-json-body response)
        lines (or (:lines body)
                  (:commands body)
                  (when-let [msg (:message body)]
                    (cond
                      (string? msg) [msg]
                      (sequential? msg) (vec msg))))]
    (when (seq lines)
      (print-bot-lines lines))
    body))

(defn- process-turn-response [response]
  (let [body (parse-json-body response)
        entities (:entities body)
        relations (:relations body)
        message (format "ok — %d entities, %d relations"
                        (count entities)
                        (count relations))]
    (print-bot-lines [message])))

(defn- pprint-lines [value]
  (let [rendered (with-out-str (pprint/pprint value))]
    (->> (str/split rendered #"\n")
         (map str/trimr)
         (remove str/blank?))))

(defn- process-me-response [response]
  (let [body (parse-json-body response)]
    (print-bot-lines (into ["Profile:"] (pprint-lines body)))))

(defn- process-summary-response [response]
  (print-bot-lines (str/split-lines (:body response))))

(defn- process-help-response [response]
  (process-lines-response response))

(defn- handle-command [base-url headers line]
  (let [[command-str arg-rest] (str/split line #"\s+" 2)
        command (-> command-str (subs 1) str/lower-case)
        json-headers (assoc headers "Content-Type" "application/json")
        args (if arg-rest
               (->> (str/split arg-rest #"\s+")
                    (remove str/blank?))
               [])
        arg (some-> arg-rest str/trim not-empty)]
    (case command
      "me"
      (if (= "doc" (some-> arg str/lower-case))
        (try
          (-> (client/get (str base-url "/api/α/me") {:headers headers})
              process-me-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e)))))
        (try
          (-> (client/get (str base-url "/api/α/me/summary") {:headers headers})
              process-summary-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e))))))

      "help"
      (try
        (-> (client/get (str base-url "/api/α/help") {:headers headers})
            process-help-response)
        (catch Exception e
          (println (str "Error: " (.getMessage e)))))

      "tail"
      (let [limit (first args)
            opts (cond-> {:headers headers}
                   limit (assoc :query-params {"limit" limit}))]
        (try
          (-> (client/get (str base-url "/api/α/tail") opts)
              process-lines-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e))))))

      "ego"
      (if (str/blank? arg)
        (print-bot-lines ["Usage: /ego <entity>"])
        (try
          (-> (client/get (str base-url "/api/α/ego")
                          {:headers headers
                           :query-params {"name" arg}})
              process-lines-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e))))))

      "cooccur"
      (if (str/blank? arg)
        (print-bot-lines ["Usage: /cooccur <entity>"])
        (try
          (-> (client/get (str base-url "/api/α/cooccur")
                          {:headers headers
                           :query-params {"name" arg}})
              process-lines-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e))))))

      "forget"
      (if (str/blank? arg)
        (print-bot-lines ["Usage: /forget <entity>"])
        (try
          (-> (client/post (str base-url "/api/α/forget")
                           {:headers json-headers
                            :body (json/write-str {:name arg})})
              process-lines-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e))))))

      "expire"
      (if (str/blank? arg)
        (print-bot-lines ["Usage: /expire <entity>"])
        (try
          (-> (client/post (str base-url "/api/α/expire")
                           {:headers json-headers
                            :body (json/write-str {:name arg})})
              process-lines-response)
          (catch Exception e
            (println (str "Error: " (.getMessage e))))))

      "entity"
      (let [[name maybe-type] args]
        (if (str/blank? name)
          (print-bot-lines ["Usage: /entity <name> [type]"])
          (let [payload (cond-> {:name name}
                          (not (str/blank? maybe-type)) (assoc :type (keyword (str/lower-case maybe-type))))]
            (try
              (-> (client/post (str base-url "/api/α/entity")
                               {:headers json-headers
                                :body (json/write-str payload)})
                  process-lines-response)
              (catch Exception e
                (println (str "Error: " (.getMessage e))))))))

      "relation"
      (if (< (count args) 3)
        (print-bot-lines ["Usage: /relation <type> <src> <dst>"])
        (let [[rel-type src dst] args
              payload {:type (keyword (str/lower-case rel-type))
                       :src {:name src}
                       :dst {:name dst}}]
          (try
            (-> (client/post (str base-url "/api/α/relation")
                             {:headers json-headers
                              :body (json/write-str payload)})
                process-lines-response)
            (catch Exception e
              (println (str "Error: " (.getMessage e)))))))

      "types"
      (let [sub (some-> (first args) str/lower-case)
            more (rest args)]
        (case sub
          nil (try
                (-> (client/get (str base-url "/api/α/types") {:headers headers})
                    process-lines-response)
                (catch Exception e
                  (println (str "Error: " (.getMessage e)))))

          "parent" (let [[type parent kind] more]
                      (if (str/blank? type)
                        (print-bot-lines ["Usage: /types parent TYPE [PARENT] [KIND]"])
                        (let [payload (cond-> {:type type}
                                        (not (str/blank? parent)) (assoc :parent parent)
                                        (not (str/blank? kind)) (assoc :kind kind))]
                          (try
                            (-> (client/post (str base-url "/api/α/types/parent")
                                             {:headers json-headers
                                              :body (json/write-str payload)})
                                process-lines-response)
                            (catch Exception e
                              (println (str "Error: " (.getMessage e))))))))

          "merge" (let [[into & aliases] more]
                     (if (or (str/blank? into) (empty? aliases))
                       (print-bot-lines ["Usage: /types merge INTO ALIAS..."])
                       (let [payload {:into into :aliases aliases}]
                         (try
                           (-> (client/post (str base-url "/api/α/types/merge")
                                            {:headers json-headers
                                             :body (json/write-str payload)})
                               process-lines-response)
                           (catch Exception e
                             (println (str "Error: " (.getMessage e))))))))

          (print-bot-lines [(str "Unknown /types subcommand: " sub)])))

      (println (str "Unknown command: " command-str)))))

(defn interactive-loop! []
  (let [base-url (or (System/getenv "ALPHA_URL") "http://localhost:8080")
        headers {"X-Profile" default-profile}
        json-headers (assoc headers "Content-Type" "application/json")]
    (println "http-client interactive mode")
    (println (str "Target server: " base-url))
    (when-not (probe-server base-url headers)
      (println "Warning: unable to reach the API server yet; commands will retry."))
    (println "Type your message and press enter. Use :quit to exit.")
    (loop []
      (print "you> ") (flush)
      (let [line (try (read-line) (catch java.io.IOException _ nil))]
        (if (or (nil? line) (exit-commands (str/trim line)))
          (do
            (println "\nGoodbye!")
            (System/exit 0))
          (let [line (str/trim line)]
            (cond
              (str/blank? line)
              (recur)

              (str/starts-with? line "/")
              (do
                (handle-command base-url headers line)
                (recur))

              :else
              (do
                (try
                  (let [response (client/post (str base-url "/api/α/turns")
                                              {:headers json-headers
                                               :body (json/write-str {:text line})})]
                    (process-turn-response response))
                  (catch Exception e
                    (println (str "Error: " (.getMessage e)))))
                (recur)))))))))
