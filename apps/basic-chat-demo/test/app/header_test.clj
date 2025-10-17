(ns app.header-test
  (:require [app.focus]
            [app.header :as header]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [xtdb.api :as xtdb]))

(def ^:private sample-time 1)

(defn- fake-candidate [id label type score anchor?]
  {:id id
   :entity {:entity/id id
            :entity/name label
            :entity/type type
            :entity/seen-count 3
            :entity/last-seen sample-time}
   :score score
   :anchor? anchor?
   :pinned? false})

(defn- fake-neighbor [focus-id neighbor-label neighbor-type score]
  {:relation/id (str focus-id "->" neighbor-label)
   :relation/type :with
   :focus-id focus-id
   :neighbor {:entity/id (str neighbor-label)
              :entity/name neighbor-label
              :entity/type neighbor-type}
   :direction :out
   :score score
   :confidence 0.9})

(deftest focus-header-display-is-compact
  (let [mock-xt-node (reify xtdb.api/DBProvider
                       (db [_] nil))]
    (with-redefs [app.focus/focus-candidates (fn [_ _ _ _ _]
                                               [(fake-candidate :a "Boston" :place 5.7 true)
                                                (fake-candidate :b "Pat" :person 4.1 false)])
                  app.focus/top-neighbors (fn [_ _ id _]
                                            (if (= id :a)
                                              [(fake-neighbor :a "XTDB" :tool 2.0)]
                                              []))]
      (let [fh (header/focus-header mock-xt-node {:anchors [{:id :a :name "Boston" :type :place}]
                                                  :intent {:type :unknown :conf 0.5}
                                                  :time sample-time
                                                  :turn-id 42
                                                  :focus-limit 5
                                                  :policy {:allow-types nil}})
            json (header/focus-header-json fh)
            lines (header/focus-header-lines fh)]
        (is (= 1 (count (:current fh))))
        (is (= [{:label "Boston" :type "place"}] (:current fh)))
        (is (= [{:label "Pat" :type "person" :score 4.1}] (:history fh)))
        (is (= [{:focus "Boston"
                 :relation "with"
                 :neighbor "XTDB"
                 :direction "out"
                 :neighbor_type "tool"
                 :score 2.0
                 :confidence 0.9}] (:context fh)))
        (is (nil? (:debug fh)))
        (is (<= (count (str/trim (or json ""))) 2000))
        (is (= "Focus header" (first lines)))
        (is (some #(= "History:" %) lines))
        (is (some #(str/includes? % "Pat (person)") lines))))))

(deftest focus-header-debug-includes-raw-details
  (let [mock-xt-node (reify xtdb.api/DBProvider
                       (db [_] nil))]
    (with-redefs [app.focus/focus-candidates (fn [_ _ _ _ _]
                                               [(fake-candidate :a "Boston" :place 5.7 true)])
                  app.focus/top-neighbors (constantly [])]
      (let [fh (header/focus-header mock-xt-node {:anchors [{:id :a :name "Boston" :type :place}]
                                                  :intent {:type :unknown}
                                                  :time sample-time
                                                  :turn-id 7
                                                  :focus-limit 5
                                                  :debug? true})]
        (is (contains? fh :debug))
        (is (= [{:id ":a"
                 :label "Boston"
                 :type "place"
                 :anchor true
                 :score 5.7
                 :last_seen sample-time
                 :seen_count 3}]
               (get-in fh [:debug :candidates])))))))
