(ns open-world-ingest.storage-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [graph-memory.types_registry :as types]
            [open-world-ingest.storage :as storage]
            [open-world-ingest.util :as util]
            [xtdb.api :as xt])
  (:import (java.time Instant)
           (java.util UUID)))


(defn- entity
  ([id label kind]
   (entity id label kind 0 [0 1]))
  ([id label kind sentence span]
   {:entity/id id
    :entity/label label
    :entity/lower-label (str/lower-case label)
    :entity/kind kind
    :entity/sentence sentence
    :mention/span span}))

(def ^:private alice-id (UUID/fromString "00000000-0000-0000-0000-000000000001"))
(def ^:private acme-id (UUID/fromString "00000000-0000-0000-0000-000000000002"))
(def ^:private bob-id (UUID/fromString "00000000-0000-0000-0000-000000000003"))

(deftest store-analysis-persists-entities-and-relations
  (let [analysis {:entities [(entity alice-id "Alice" :person 0 [0 1])
                              (entity alice-id "A. Smith" :person 0 [1 2])
                              (entity acme-id "Acme Corp" :org 0 [2 3])]
                   :relations [{:relation/subject "Alice"
                                :relation/object "Acme Corp"
                                :relation/src alice-id
                                :relation/dst acme-id
                                :relation/label :works-at
                                :relation/type-aliases [:work-at]
                                :relation/polarity :asserted
                                :relation/confidence 0.88
                                :relation/sentence 0}]}
        submitted (atom nil)
        awaited (atom [])
        ensured (atom [])
        merged (atom [])
        now (Instant/ofEpochMilli 1)]
    (with-redefs [storage/node (constantly ::node)
                  xt/db (fn [node]
                          (is (= ::node node))
                          ::db)
                  xt/entity (fn [db eid]
                              (is (= ::db db))
                              (is (contains? #{[:entity/id alice-id]
                                               [:entity/id acme-id]}
                                             eid))
                              nil)
                  xt/submit-tx (fn [node ops]
                                 (is (= ::node node))
                                 (reset! submitted ops)
                                 ::tx)
                  xt/await-tx (fn [node tx]
                                (is (= ::node node))
                                (swap! awaited conj tx))
                  util/now (constantly now)
                  types/ensure! (fn [kind type opts]
                                  (swap! ensured conj [kind type opts])
                                  {:id type :kind kind})
                  types/merge! (fn [kind canonical aliases]
                                 (swap! merged conj [kind canonical aliases])
                                 nil)]
      (let [summary (storage/store-analysis! "Alice works at Acme Corp." analysis)
            docs (map second @submitted)
            entity-doc (some #(when (= [:entity/id alice-id] (:xt/id %)) %) docs)
            acme-doc (some #(when (= [:entity/id acme-id] (:xt/id %)) %) docs)
            relation-doc (some #(when (= (:relation/src %) alice-id) %) docs)
            mention-docs (filter :mention/id docs)
            utterance-doc (some #(when (:utterance/id %) %) docs)]
        (is (= [::tx] @awaited))
        (is (= #{{:id alice-id :kind :person :new? true}
                 {:id acme-id :kind :org :new? true}}
               (set (map #(dissoc % :label) (:entities summary)))))
        (is (= #{"Alice" "Acme Corp"}
               (set (map :label (:entities summary)))))
        (is (= #{{:subject "Alice"
                  :object "Acme Corp"
                  :relation :works-at
                  :polarity :asserted
                  :confidence 0.88}}
               (set (:relations summary))))
        (is (= #{"Alice" "A. Smith"}
               (:entity/aliases entity-doc)))
        (is (= "Acme Corp" (:entity/label acme-doc)))
        (is (= :works-at (:relation/label relation-doc)))
        (is (= #{alice-id acme-id}
               (set (map :mention/entity mention-docs))))
        (is (= "Alice works at Acme Corp." (:utterance/text utterance-doc)))
        (is (= [[:relation :works-at {:parent :relation/works/*}]] @ensured))
        (is (= [[:relation :works-at [:work-at]]] @merged))))))

(deftest relation-queries-return-recent-and-related-data
  (let [inst1 (Instant/ofEpochMilli 1)
        inst2 (Instant/ofEpochMilli 2)
        inst3 (Instant/ofEpochMilli 3)
        call (atom 0)]
    (with-redefs [storage/node (constantly ::node)
                  xt/db (fn [node]
                          (is (= ::node node))
                          ::db)
                  xt/q (fn [& args]
                         (let [idx (swap! call inc)]
                           (case idx
                             1 (let [[db query] args]
                                 (is (= ::db db))
                                 (is (= 2 (:limit query)))
                                 [[::rel3 :collaborates-with alice-id "Alice" :person bob-id "Bob" :person :asserted inst3]
                                  [::rel2 :mentors bob-id "Bob" :person alice-id "Alice" :person :asserted inst2]])
                             2 (let [[db _query entity] args]
                                 (is (= ::db db))
                                 (is (= alice-id entity))
                                 [[:collaborates-with bob-id "Bob" :person :asserted inst3]
                                  [:works-at acme-id "Acme" :org :asserted inst1]])
                             3 (let [[db _query entity] args]
                                 (is (= ::db db))
                                 (is (= alice-id entity))
                                 [[:mentors bob-id "Bob" :person :asserted inst2]])
                             4 (let [[db _query entity] args]
                                 (is (= ::db db))
                                 (is (= alice-id entity))
                                 [[acme-id "Acme" :org 1]
                                  [bob-id "Bob" :person 1]]))))]
      (testing "recent relations are returned with newest first"
        (let [recent (storage/recent-relations 2)]
          (is (= [:collaborates-with :mentors] (map :relation recent)))
          (is (= ["Alice" "Bob"] (map (comp :label :src) recent)))
          (is (= ["Bob" "Alice"] (map (comp :label :dst) recent)))))
      (testing "ego neighbors include incoming and outgoing relations"
        (let [{:keys [outgoing incoming]} (storage/ego-neighbors alice-id)]
          (is (= #{{:relation :collaborates-with :direction :out :entity {:id bob-id :label "Bob" :kind :person}}
                   {:relation :works-at :direction :out :entity {:id acme-id :label "Acme" :kind :org}}}
                 (set (map #(select-keys % [:relation :direction :entity]) outgoing))))
          (is (= #{{:relation :mentors :direction :in :entity {:id bob-id :label "Bob" :kind :person}}}
                 (set (map #(select-keys % [:relation :direction :entity]) incoming))))))
      (testing "co-occurring entities are tallied"
        (let [coocc (storage/cooccurring-entities alice-id)]
          (is (= #{{:id acme-id :label "Acme" :kind :org :count 1}
                   {:id bob-id :label "Bob" :kind :person :count 1}}
                 (set coocc))))))))
