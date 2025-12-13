(ns client.session-test
  (:require [app.command-service :as svc]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [client.api :as api]
            [datascript.core :as d])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn temp-dir []
  (-> (Files/createTempDirectory "client-session-test" (into-array FileAttribute []))
      (.toFile)))

(def baseline-demo-path
  (.getPath (io/file ".." ".." "resources" "baseline" "demo_session.edn")))

(def baseline-demo-fixture
  (delay
    (when-not (.exists (io/file baseline-demo-path))
      (throw (ex-info "Baseline demo fixture is missing" {:path baseline-demo-path})))
    (edn/read-string (slurp baseline-demo-path))))

(defn- focus-lines [turns]
  (mapv (fn [turn]
          (vec (get-in turn [:data :focus-header-lines])))
        turns))

(defn- ds-entities [conn]
  (->> (d/q '[:find ?name ?type ?seen
              :where [?e :entity/name ?name]
                     [?e :entity/type ?type]
                     [?e :entity/seen-count ?seen]]
            @conn)
       (map (fn [[name type seen]] {:name name :type type :seen-count seen}))
       (sort-by :name)
       vec))

(defn- ds-relations [conn]
  (->> (d/q '[:find ?src-name ?rel-type ?dst-name
              :where
              [?src :entity/name ?src-name]
              [?dst :entity/name ?dst-name]
              [?rel :relation/src ?src]
              [?rel :relation/dst ?dst]
              [?rel :relation/type ?rel-type]]
            @conn)
       (map (fn [[src type dst]] {:src src :type type :dst dst}))
       (sort-by (juxt :src :dst :type))
       vec))

(defn- xt-entities []
  (->> (xt/q '[:find ?name ?type ?seen
               :where
               [?e :entity/name ?name]
               [?e :entity/type ?type]
               [?e :entity/seen-count ?seen]])
       (map (fn [[name type seen]] {:name name :type type :seen-count seen}))
       (sort-by :name)
       vec))

(defn- xt-relations []
  (->> (xt/q '[:find ?src ?type ?dst
               :where
               [?rel :relation/src ?src-e]
               [?rel :relation/dst ?dst-e]
               [?rel :relation/type ?type]
               [?src-e :entity/name ?src]
               [?dst-e :entity/name ?dst]])
       (map (fn [[src type dst]] {:src src :type type :dst dst}))
       (sort-by (juxt :src :dst :type))
       vec))

(defn- capture-baseline [session lines]
  (let [turns (mapv #(api/run-line session %) lines)
        _ (xt/sync-node!)
        ctx ((:ctx-provider session))
        conn (:conn ctx)]
    {:focus-header (focus-lines turns)
     :datascript-entities (ds-entities conn)
     :datascript-relations (ds-relations conn)
     :xtdb-entities (xt-entities)
     :xtdb-relations (xt-relations)}))

(defn with-session-fixture [f]
  (let [dir (.getAbsolutePath (temp-dir))
        session (api/start {:data-root dir})]
    (try
      (f session)
      (finally
        (api/stop session)))))

(defn summary-text [session]
  (let [ctx ((:ctx-provider session))
        profile (:default-profile ctx)
        conn (:conn ctx)
        summary (svc/profile-summary {:profile profile
                                       :conn conn
                                       :now (System/currentTimeMillis)
                                       :xt-node (xt/node)}
                                      nil)]
    (:text summary)))

(deftest process-lines
  (with-session-fixture
   (fn [session]
     (testing "likes accumulate into profile summary"
       (let [like1 (api/run-line session "I like Willie Dixon.")
             like2 (api/run-line session "I like Red Mitchell.")
             text  (summary-text session)]
         (is (= :say (:type like1)))
         (is (= :say (:type like2)))
         (is (re-find #"Willie Dixon" text))
         (is (re-find #"Red Mitchell" text)))))))

(deftest run-script
  (with-session-fixture
   (fn [session]
     (let [lines ["I like Willie Dixon." "I like Red Mitchell." "I like Jimbo Wallace." "I live in Oxford."]
           results (api/run-script session lines)
           text    (summary-text session)
           conn    (:conn (store-manager/ctx :me))
           rel-types (set (map first (d/q '[:find ?t :where [?r :relation/type ?t]] @conn)))]
       (is (= 4 (count results)))
       (is (= [:say :say :say :say] (map :type results)))
       (doseq [name ["Willie Dixon" "Red Mitchell" "Jimbo Wallace" "Oxford"]]
         (is (re-find (re-pattern name) text)))
       (is (contains? rel-types :oxford/live-in))
       (is (not (rel-types :likes)))))))

(deftest me-summary-succeeds-on-first-call
  (with-session-fixture
   (fn [session]
     (testing "first /me summary renders without restarting XT"
       (let [env @(:env-atom session)
             _ (is (map? env))
             ctx ((:ctx-provider session))
             _ (is (some? (:xtdb-node ctx)))
             result (api/run-line session "/me summary")
             message (get-in result [:data :message])]
         (is (= :slash (:type result)))
         (is (vector? message))
         (is (seq message))
         (is (re-find #"Profile: " (first message))))))))

(deftest focus-header-tracks-history-and-context
  (with-session-fixture
   (fn [session]
     (let [run (fn [text]
                 (api/run-line session text))
           first-turn (run "I play piano")
           second-turn (run "You know Jane")
           third-turn (run "Robbie plays piano")
           fh1 (get-in first-turn [:data :focus-header-lines])
           fh2 (get-in second-turn [:data :focus-header-lines])
           fh3 (get-in third-turn [:data :focus-header-lines])]
       (testing "first turn only reflects current extraction"
         (is (seq fh1))
         (is (some #(re-find #"Current:" %) fh1))
         (is (not-any? #(re-find #"Recent:" %) fh1)))

       (testing "second turn includes recent context from prior relation"
         (is (seq fh2))
         (is (some #(re-find #"Recent:" %) fh2))
         (is (some #(re-find #"You -\[know\]-> Jane" %) fh2)))

       (testing "third turn shows enrichment linking back to piano relation"
         (is (seq fh3))
         (is (some #(re-find #"Enriched:" %) fh3))
         (is (some #(re-find #"Me -\[play\]-> piano" %) fh3)))))))

(defn- relation-key [rel]
  [(some-> (:type rel) name)
   (get-in rel [:src :name])
   (get-in rel [:dst :name])])

(deftest enriched-section-eventually-diverges-from-recent
  (with-session-fixture
   (fn [session]
     (let [sentences ["I am by birth a Genevese, and my family is one of the most distinguished of that republic."
                      "My ancestors had been for many years counsellors and syndics, and my father had filled several public situations with honour and reputation."
                      "He was respected by all who knew him for his integrity and indefatigable attention to public business."
                      "He passed his younger days perpetually occupied by the affairs of his country; a variety of circumstances had prevented his marrying early, nor was it until the decline of life that he became a husband and the father of a family."
                      "As the circumstances of his marriage illustrate his character, I cannot refrain from relating them."
                      "One of his most intimate friends was a merchant who, from a flourishing state, fell, through numerous mischances, into poverty."
                      "This man, whose name was Beaufort, was of a proud and unbending disposition and could not bear to live in poverty and oblivion in the same country where he had formerly been distinguished for his rank and magnificence."
                      "Having paid his debts, therefore, in the most honourable manner, he retreated with his daughter to the town of Lucerne, where he lived unknown and in wretchedness."
                      "My father loved Beaufort with the truest friendship and was deeply grieved by his retreat in these unfortunate circumstances."
                      "He bitterly deplored the false pride which led his friend to a conduct so little worthy of the affection that united them."
                      "He lost no time in endeavouring to seek him out, with the hope of persuading him to begin the world again through his credit and assistance."
                      "Beaufort had taken effectual measures to conceal himself, and it was ten months before my father discovered his abode."
                      "Overjoyed at this discovery, he hastened to the house, which was situated in a mean street near the Reuss."
                      "But when he entered, misery and despair alone welcomed him."
                      "Beaufort had saved but a very small sum of money from the wreck of his fortunes, but it was sufficient to provide him with sustenance for some months, and in the meantime he hoped to procure some respectable employment in a merchant's house."
                      "The interval was, consequently, spent in inaction; his grief only became more deep and rankling when he had leisure for reflection, and at length it took so fast hold of his mind that at the end of three months he lay on a bed of sickness, incapable of any exertion."]
           state (:state session)
           divergence
           (loop [idx 1
                  remaining sentences]
             (if-let [sentence (first remaining)]
               (do
                 (api/run-line session sentence)
                 (let [last-turn (:last-turn @state)
                       debug (:focus_header_debug last-turn)
                       json (:json debug)
                       recent (get-in json [:recent :relations])
                       enriched (get-in json [:enriched :relations])
                       extras (when (seq enriched)
                                (let [recent-keys (set (map relation-key recent))
                                      enriched-keys (set (map relation-key enriched))
                                      diff (set/difference enriched-keys recent-keys)]
                                  (not-empty diff)))]
                   (if extras
                     {:turn idx :extras extras}
                     (recur (inc idx) (rest remaining)))))
               nil))]
       (testing "Enriched section never diverges from recent within the excerpt"
         (is (nil? divergence)))
       (when divergence
       (is (pos? (:turn divergence)))
        (is (seq (:extras divergence))))))))

(deftest baseline-demo-remains-deterministic
  (with-session-fixture
   (fn [session]
     (let [fixture @baseline-demo-fixture
           observed (capture-baseline session (:script fixture))]
       (doseq [k [:focus-header
                  :datascript-entities
                  :datascript-relations
                  :xtdb-entities
                  :xtdb-relations]]
         (is (= (get fixture k) (get observed k))
             (str "Mismatch in baseline segment " k)))))))
