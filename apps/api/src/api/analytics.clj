(ns api.analytics
  "Live tap consumer that summarizes focus-header and profile taps for Tatami HUD."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defonce ^:private !tap (atom nil))
(def ^:private history-length 12)
(defonce ^:private !state (atom {:focus nil
                                 :profile nil
                                 :history []}))

(defn- now [] (System/currentTimeMillis))

(defn- ensure-parent! [^java.io.File file]
  (when-let [parent (.getParentFile file)]
    (.mkdirs parent))
  file)

(defn- output-files []
  (let [root (-> (io/file ".") .getAbsoluteFile .getCanonicalFile)
        local (io/file root "resources" "vitality" "focus_profile.edn")
        futon3 (io/file root ".." "futon3" "resources" "vitality" "focus_profile.edn")]
    (->> [local futon3]
         (map (fn [f]
                (ensure-parent! f)
                f)))))

(defn- write-state! [state]
  (let [payload (assoc state :generated-at (now))]
    (doseq [file (output-files)]
      (spit file (pr-str payload)))))

(defn- summarize-focus [event]
  (let [cands (or (:raw-candidates-sample event) [])
        anchors (->> cands
                     (keep #(or (get-in % [:entity :entity/name])
                                (get % :name)))
                     (remove str/blank?)
                     (take 5)
                     vec)
        neighbors (->> (:neighbors-sample event)
                       (keep #(get-in % [:neighbor :entity/name]))
                       (remove str/blank?)
                       (take 5)
                       vec)]
    {:timestamp (now)
     :anchors anchors
     :neighbors neighbors}))

(defn- summarize-profile [event]
  (let [topics (->> (:topics event)
                    (keep #(or (:type %)
                               (:label %)))
                    (map (fn [topic]
                           (cond
                             (keyword? topic) (name topic)
                             (string? topic) topic
                             :else (str topic))))
                    (remove str/blank?)
                    (take 3)
                    vec)]
    {:timestamp (now)
     :relation-count (:relations event)
     :topics topics}))

(defn- push-history [state entry]
  (update state :history
          (fn [hist]
            (let [hist' (conj (vec (or hist [])) entry)]
              (if (> (count hist') history-length)
                (subvec hist' (- (count hist') history-length))
                hist')))))

(defn- record-event! [event]
  (case (:event event)
    :focus-header/debug (let [summary (summarize-focus event)]
                          (swap! !state (fn [state]
                                          (-> state
                                              (assoc :focus summary)
                                              (push-history (assoc summary :type :focus)))))
                          (write-state! @!state))
    :me-profile/profile (let [summary (summarize-profile event)]
                          (swap! !state (fn [state]
                                          (-> state
                                              (assoc :profile summary)
                                              (push-history (assoc summary :type :profile)))))
                          (write-state! @!state))
    nil))

(defn start! []
  (when (nil? @!tap)
    (let [listener (fn [event]
                     (when (map? event)
                       (record-event! event)))]
      (add-tap listener)
      (reset! !tap listener))))

(defn stop! []
  (when-let [listener @!tap]
    (remove-tap listener)
    (reset! !tap nil)))
