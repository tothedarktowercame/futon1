(ns open-world-ingest.open-ie-trace-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [open-world-ingest.nlp :as nlp]
            [open-world-ingest.trace :as trace]
            [open-world-ingest.util :as util])
  (:import (java.nio.file Files)))

(def ^:private entity-kinds
  {"Accounts" :proper
   "Agitation" :proper
   "Breeze" :proper
   "British Museum" :org
   "Buda-Pesth" :place
   "Bistritz" :place
   "Clarke" :person
  "Danger" :proper
  "Delight" :proper
  "Dr. Raymond" :person
  "Enterprise" :proper
   "Enticements" :proper
   "Captain Mira" :person
   "Nova" :person
   "Archive" :place
   "Mission plan" :event
   "Reactor" :place
   "Diagnostics" :event
   "Discovery log" :proper
   "Expedition" :event
   "Favourite dream" :proper
   "Fear" :proper
   "Goal" :proper
   "Great wood" :place
   "Horizon" :place
   "Mountains" :place
   "Hotel Royale" :place
   "Klausenburgh" :place
   "Land of beauty" :place
   "Margaret" :person
   "Misgivings" :proper
   "Me" :person
   "Mina" :person
   "Munich" :place
   "Needle" :proper
   "North Pacific Ocean" :place
   "North regions" :place
   "Operation" :event
   "Paprika chicken" :proper
   "Paprika hendl" :proper
   "Path" :proper
   "Petersburgh" :place
   "Pole" :place
   "Pole countries" :place
   "Reflections" :proper
   "Regret" :proper
   "River" :place
   "Sweet breath" :proper
   "Vienna" :place
   "Safety" :proper
   "Sight of world" :proper
   "Sister" :person
   "Study" :proper
   "Sun" :proper
   "Surgeon" :person
   "Terrace" :place
   "Theory" :proper
   "Tonight" :proper
   "Transcendental medicine" :proper
   "Truth" :proper
   "Volumes" :proper
   "We" :group
   "Wild doves" :proper
   "Wondrous power" :proper
   "You" :person
   "Visit" :event
   "Walk" :event
   "Valley" :place})

(def ^:private journal-data
  [{:sentence "3 May. Bistritz.—Left Munich at 8:35 P. M., on 1st May, arriving at Vienna early next morning; should have arrived at 6:46, but train was an hour late."
    :triples [{:subj "Me" :pred "left" :lemma "leave" :obj "Munich" :confidence 0.9}
              {:subj "Me" :pred "arrived at" :lemma "arrive at" :obj "Vienna" :confidence 0.85}]}
   {:sentence "Buda-Pesth seems a wonderful place, from the glimpse which I got of it from the train and the little I could walk through the streets."
    :triples [{:subj "Me" :pred "got of" :lemma "get" :obj "Buda-Pesth" :confidence 0.7}]}
   {:sentence "I feared to go very far from the station, as we had arrived late and would start as near the correct time as possible."
    :triples []}
   {:sentence "The impression I had was that we were leaving the West and entering the East; the most western of splendid bridges over the Danube, which is here of noble width and depth, took us among the traditions of Turkish rule."
    :triples []}
   {:sentence "We left in pretty good time, and came after nightfall to Klausenburgh."
    :triples [{:subj "Me" :pred "came to" :lemma "come to" :obj "Klausenburgh" :confidence 0.8}]}
   {:sentence "Here I stopped for the night at the Hotel Royale."
    :triples [{:subj "Me" :pred "stopped at" :lemma "stop at" :obj "Hotel Royale" :confidence 0.88}]}
   {:sentence "I had for dinner, or rather supper, a chicken done up some way with red pepper, which was very good but thirsty."
    :triples [{:subj "Me" :pred "ate" :lemma "eat" :obj "Paprika chicken" :confidence 0.82}]}
   {:sentence "(Mem., get recipe for Mina.)"
    :triples [{:subj "Me" :pred "get recipe for" :lemma "get recipe for" :obj "Mina" :confidence 0.6}]}
   {:sentence "I asked the waiter, and he said it was called \"paprika hendl,\" and that, as it was a national dish, I should be able to get it anywhere along the Carpathians."
    :triples [{:subj "Paprika hendl" :pred "was" :lemma "be" :obj "national dish" :confidence 0.75}]}
   {:sentence "I found my smattering of German very useful here; indeed, I don’t know how I should be able to get on without it."
    :triples []}
   {:sentence "Having had some time at my disposal when in London, I had visited the British Museum, and made search among the books and maps in the library regarding Transylvania; it had struck me that some foreknowledge of the country could hardly fail to have some importance in dealing with a nobleman of that country."
    :triples [{:subj "Me" :pred "visited" :lemma "visit" :obj "British Museum" :confidence 0.92}]}
   {:sentence "I find that the district he named is in the extreme east of the country, just on the borders of three states, Transylvania, Moldavia and Bukovina, in the midst of the Carpathian mountains; one of the wildest and least known portions of Europe."
    :triples []}
   {:sentence "I was not able to light on any map or work giving the exact locality of the Castle Dracula, as there are no maps of this country as yet to compare with our own Ordnance Survey maps; but I found that Bistritz, the post town named by Count Dracula, is a fairly well-known place."
    :triples [{:subj "Me" :pred "found" :lemma "find" :obj "Bistritz" :confidence 0.78}]}
   {:sentence "I shall enter here some of my notes, as they may refresh my memory when I talk over my travels with Mina."
    :triples [{:subj "Me" :pred "talk with" :lemma "talk with" :obj "Mina" :confidence 0.73}]}])

(def ^:private frankenstein-data
  [{:sentence "You will rejoice to hear that no disaster has accompanied the commencement of an enterprise which you have regarded with such evil forebodings."
    :triples [{:subj "You" :pred "regarded with" :lemma "regard with" :obj "Enterprise" :confidence 0.7}]}
   {:sentence "I arrived here yesterday, and my first task is to assure my dear sister of my welfare and increasing confidence in the success of my undertaking."
    :triples [{:subj "Me" :pred "assure" :lemma "assure" :obj "Sister" :confidence 0.86}]}
   {:sentence "I am already far north of London, and as I walk in the streets of Petersburgh, I feel a cold northern breeze play upon my cheeks, which braces my nerves and fills me with delight."
    :triples [{:subj "Me" :pred "walk in" :lemma "walk in" :obj "Petersburgh" :confidence 0.8}
              {:subj "Breeze" :pred "fills" :lemma "fill" :obj "Delight" :confidence 0.6}]}
   {:sentence "This breeze, which has travelled from the regions towards which I am advancing, gives me a foretaste of those icy climes."
    :triples [{:subj "Breeze" :pred "travelled from" :lemma "travel from" :obj "North regions" :confidence 0.65}]}
   {:sentence "Inspirited by this wind of promise, my daydreams become more fervent and vivid."
    :triples []}
   {:sentence "I try in vain to be persuaded that the pole is the seat of frost and desolation; it ever presents itself to my imagination as the region of beauty and delight."
    :triples [{:subj "Pole" :pred "is" :lemma "be" :obj "Region of beauty" :confidence 0.7}]}
   {:sentence "There, Margaret, the sun is for ever visible, its broad disk just skirting the horizon and diffusing a perpetual splendour."
    :triples [{:subj "Sun" :pred "is visible over" :lemma "be visible over" :obj "Horizon" :confidence 0.75}
              {:subj "Me" :pred "address" :lemma "address" :obj "Margaret" :confidence 0.6}]}
   {:sentence "There—for with your leave, my sister, I will put some trust in preceding navigators—there snow and frost are banished; and, sailing over a calm sea, we may be wafted to a land surpassing in wonders and in beauty every region hitherto discovered on the habitable globe."
    :triples [{:subj "We" :pred "sail to" :lemma "sail to" :obj "Land of beauty" :confidence 0.72}]}
   {:sentence "I may there discover the wondrous power which attracts the needle and may regulate a thousand celestial observations that require only this voyage to render their seeming eccentricities consistent for ever."
    :triples [{:subj "Me" :pred "discover" :lemma "discover" :obj "Wondrous power" :confidence 0.83}
              {:subj "Power" :pred "attracts" :lemma "attract" :obj "Needle" :confidence 0.71}]}
   {:sentence "I shall satiate my ardent curiosity with the sight of a part of the world never before visited, and may tread a land never before imprinted by the foot of man."
    :triples [{:subj "Me" :pred "satiate with" :lemma "satiate with" :obj "Sight of world" :confidence 0.74}]}
   {:sentence "These are my enticements, and they are sufficient to conquer all fear of danger or death and to induce me to commence this laborious voyage with the joy a child feels when he embarks in a little boat, with his holiday mates, on an expedition of discovery up his native river."
    :triples [{:subj "Enticements" :pred "conquer" :lemma "conquer" :obj "Fear" :confidence 0.7}]}
   {:sentence "But supposing all these conjectures to be false, you cannot contest the inestimable benefit which I shall confer on all mankind, to the last generation, by discovering a passage near the pole to those countries, to reach which at present so many months are requisite; or by ascertaining the secret of the magnet, which, if at all possible, can only be effected by an undertaking such as mine."
    :triples [{:subj "Me" :pred "confer on" :lemma "confer on" :obj "Mankind" :confidence 0.77}
              {:subj "Me" :pred "discover passage" :lemma "discover passage" :obj "Pole countries" :confidence 0.69}]}
   {:sentence "These reflections have dispelled the agitation with which I began my letter, and I feel my heart glow with an enthusiasm which elevates me to heaven, for nothing contributes so much to tranquillise the mind as a steady purpose—a point on which the soul may fix its intellectual eye."
    :triples [{:subj "Reflections" :pred "dispelled" :lemma "dispel" :obj "Agitation" :confidence 0.68}]}
   {:sentence "This expedition has been the favourite dream of my early years."
    :triples [{:subj "Expedition" :pred "has been" :lemma "be" :obj "Favourite dream" :confidence 0.73}]}
   {:sentence "I have read with ardour the accounts of the various voyages which have been made in the prospect of arriving at the North Pacific Ocean through the seas which surround the pole."
    :triples [{:subj "Me" :pred "read with" :lemma "read with" :obj "Accounts" :confidence 0.72}
              {:subj "Accounts" :pred "regarding" :lemma "regard" :obj "North Pacific Ocean" :confidence 0.65}]}
   {:sentence "You may remember that a history of all the voyages made for purposes of discovery composed the whole of our good Uncle Thomas’ library."
    :triples [{:subj "You" :pred "remember" :lemma "remember" :obj "Voyage history" :confidence 0.7}]}
   {:sentence "My education was neglected, yet I was passionately fond of reading."
    :triples []}
  {:sentence "These volumes were my study day and night, and my familiarity with them increased that regret which I had felt, as a child, on learning that my father’s dying injunction had forbidden my uncle to allow me to embark in a seafaring life."
    :triples [{:subj "Volumes" :pred "were" :lemma "be" :obj "Study" :confidence 0.66}
              {:subj "Volumes" :pred "increased" :lemma "increase" :obj "Regret" :confidence 0.64}]}])

(def ^:private slash-data
  [{:sentence "Captain Mira mentors/guides Nova through the archive."
    :triples [{:subj "Captain Mira" :pred "mentors/guides" :lemma "mentor/guide" :obj "Nova" :confidence 0.9}]}
   {:sentence "Nova documents/tests the mission plan before lift-off."
    :triples [{:subj "Nova" :pred "documents/tests" :lemma "document/test" :obj "Mission plan" :confidence 0.82}]}
   {:sentence "Together they schedule/calibrate the reactor diagnostics."
    :triples [{:subj "We" :pred "schedule/calibrate" :lemma "schedule/calibrate" :obj "Diagnostics" :confidence 0.78}]}
   {:sentence "The archive cross-references/enriches every discovery they make."
    :triples [{:subj "Archive" :pred "cross-references/enriches" :lemma "cross-reference/enrich" :obj "Discovery log" :confidence 0.7}]}
   {:sentence "Captain Mira mentors/guides Nova again during the night shift."
    :triples [{:subj "Captain Mira" :pred "mentors/guides" :lemma "mentor/guide" :obj "Nova" :confidence 0.88}]}])

(def ^:private raymond-data
  [{:sentence "I am glad you came, Clarke; very glad indeed."
    :triples [{:subj "Dr. Raymond" :pred "glad about" :lemma "glad about" :obj "Clarke" :confidence 0.82}]}
   {:sentence "I was not sure you could spare the time."
    :triples [{:subj "Dr. Raymond" :pred "doubted" :lemma "doubt" :obj "Clarke" :confidence 0.64}]}
   {:sentence "I was able to make arrangements for a few days."
    :triples [{:subj "Clarke" :pred "made arrangements for" :lemma "make arrangements for" :obj "Visit" :confidence 0.7}]}
   {:sentence "Things are not very lively just now."
    :triples []}
   {:sentence "But have you no misgivings, Raymond?"
    :triples [{:subj "Clarke" :pred "asked about" :lemma "ask about" :obj "Misgivings" :confidence 0.68}]}
   {:sentence "Is it absolutely safe?"
    :triples [{:subj "Clarke" :pred "questioned" :lemma "question" :obj "Safety" :confidence 0.72}]}
   {:sentence "The two men were slowly pacing the terrace in front of Dr. Raymond's house."
    :triples [{:subj "Dr. Raymond" :pred "paced with" :lemma "pace with" :obj "Clarke" :confidence 0.74}]}
   {:sentence "The sun still hung above the western mountain-line."
    :triples [{:subj "Sun" :pred "hung over" :lemma "hang over" :obj "Mountains" :confidence 0.73}]}
   {:sentence "A sweet breath came from the great wood on the hillside above."
    :triples [{:subj "Great wood" :pred "sent" :lemma "send" :obj "Sweet breath" :confidence 0.66}]}
   {:sentence "Below, in the long lovely valley, the river wound in and out between the lonely hills."
    :triples [{:subj "River" :pred "wound through" :lemma "wind through" :obj "Valley" :confidence 0.75}]}
   {:sentence "Dr. Raymond turned sharply to his friend."
    :triples [{:subj "Dr. Raymond" :pred "turned toward" :lemma "turn toward" :obj "Clarke" :confidence 0.7}]}
   {:sentence "Safe? Of course it is." :triples [{:subj "Dr. Raymond" :pred "insisted on" :lemma "insist on" :obj "Safety" :confidence 0.78}]}
   {:sentence "In itself the operation is a perfectly simple one; any surgeon could do it."
    :triples [{:subj "Operation" :pred "can be done by" :lemma "can be done by" :obj "Surgeon" :confidence 0.8}]}
   {:sentence "And there is no danger at any other stage?"
    :triples [{:subj "Clarke" :pred "questioned" :lemma "question" :obj "Danger" :confidence 0.74}]}
   {:sentence "None; absolutely no physical danger whatsoever, I give you my word."
    :triples [{:subj "Dr. Raymond" :pred "promised" :lemma "promise" :obj "Safety" :confidence 0.73}]}
   {:sentence "You are always timid, Clarke, always; but you know my history."
    :triples [{:subj "Dr. Raymond" :pred "reminded" :lemma "remind" :obj "Clarke" :confidence 0.65}]}
   {:sentence "I have devoted myself to transcendental medicine for the last twenty years."
    :triples [{:subj "Dr. Raymond" :pred "devoted to" :lemma "devote to" :obj "Transcendental medicine" :confidence 0.86}]}
   {:sentence "I have heard myself called quack and charlatan and impostor, but all the while I knew I was on the right path."
    :triples [{:subj "Dr. Raymond" :pred "followed" :lemma "follow" :obj "Path" :confidence 0.7}]}
   {:sentence "Five years ago I reached the goal, and since then every day has been a preparation for what we shall do tonight."
    :triples [{:subj "Dr. Raymond" :pred "reached" :lemma "reach" :obj "Goal" :confidence 0.8}
              {:subj "Dr. Raymond" :pred "prepared for" :lemma "prepare for" :obj "Tonight" :confidence 0.72}]}
   {:sentence "I should like to believe it is all true."
    :triples [{:subj "Clarke" :pred "desired" :lemma "desire" :obj "Truth" :confidence 0.67}]}
   {:sentence "Clarke knit his brows, and looked doubtfully at Dr. Raymond."
    :triples [{:subj "Clarke" :pred "looked at" :lemma "look at" :obj "Dr. Raymond" :confidence 0.69}]}
   {:sentence "Are you perfectly sure, Raymond, that your theory is not a phantasmagoria?"
    :triples [{:subj "Clarke" :pred "questioned" :lemma "question" :obj "Theory" :confidence 0.76}]}
   {:sentence "Dr. Raymond stopped in his walk and turned sharply."
    :triples [{:subj "Dr. Raymond" :pred "stopped" :lemma "stop" :obj "Walk" :confidence 0.7}]}
   {:sentence "He answered Clarke with a flush on his cheek."
    :triples [{:subj "Dr. Raymond" :pred "answered" :lemma "answer" :obj "Clarke" :confidence 0.68}]}])

(defn- temp-dir []
  (-> (Files/createTempDirectory "open-world-ingest-trace" (make-array java.nio.file.attribute.FileAttribute 0))
      (.toFile)))

(defn- ensure-base-entity! [registry label]
  (or (get @registry label)
      (let [kind (get entity-kinds label :proper)
            base {:entity/id (util/sha1 (str (str/lower-case label) ":" (name kind)))
                  :entity/label label
                  :entity/lower-label (str/lower-case label)
                  :entity/kind kind}]
        (swap! registry assoc label base)
        base)))

(defn- triple->trace-record [sentence-idx sentence {:keys [subj pred obj lemma confidence]}]
  {:sent sentence
   :sent-idx sentence-idx
   :subj subj
   :obj obj
   :pred pred
   :lemma lemma
   :confidence confidence
   :negated? false
   :spans {:subj nil :obj nil :pred nil}})

(defn- build-relation [sentence-idx subj-base obj-base {:keys [pred lemma confidence]}]
  (let [obj-label (:entity/label obj-base)
        {:keys [label aliases]} (#'open-world-ingest.nlp/derive-relation-type lemma pred obj-label)
        rel-label (or label :links-to)]
    {:subject (:entity/label subj-base)
     :object (:entity/label obj-base)
     :relation rel-label
     :polarity :asserted
     :confidence confidence
     :sentence sentence-idx
     :aliases (when (seq aliases) (vec aliases))}))

(defn- run-scenario!
  [scenario]
  (let [data-root (temp-dir)
        trace-file (io/file data-root "trace" "openie.edn")
        trace-config (trace/config {:path (.getAbsolutePath trace-file)})
        entity-registry (atom {})
        world-state (atom {:entities {} :relations []})]
    (trace/reset! trace-config)
    (try
      (let [ingestion-log (doall
                           (map-indexed
                            (fn [idx {:keys [sentence triples]}]
                              (let [trace-records (map #(triple->trace-record idx sentence %) triples)
                                    _ (trace/append! trace-config trace-records)
                                    base-entities (->> triples
                                                       (mapcat (fn [{:keys [subj obj]}]
                                                                 (remove nil? [subj obj])))
                                                       distinct
                                                       (map #(ensure-base-entity! entity-registry %)))
                                    entity-occurrences (map (fn [base]
                                                              (assoc base
                                                                     :entity/sentence idx
                                                                     :mention/span nil))
                                                            base-entities)
                                    relations (map (fn [{:keys [subj obj] :as triple}]
                                                     (let [subj-base (ensure-base-entity! entity-registry subj)
                                                           obj-base (ensure-base-entity! entity-registry obj)]
                                                       (build-relation idx subj-base obj-base triple)))
                                                   triples)]
                                (doseq [occ entity-occurrences]
                                  (swap! world-state update :entities assoc (:entity/label occ)
                                         (select-keys occ [:entity/id :entity/label :entity/kind])))
                                (swap! world-state update :relations into relations)
                                {:sentence sentence
                                 :triples trace-records
                                 :relations relations}))
                            scenario))
            world-model (let [{:keys [entities relations]} @world-state]
                           {:me (get entities "Me")
                            :you (get entities "You")
                            :entities (->> entities vals vec)
                            :relations relations})]
        {:log ingestion-log
         :world world-model})
      (finally
        (.deleteOnExit data-root)))))

(defn- assert-derived-relations [sentence triples relations]
  (let [derived-labels (->> triples
                            (keep (fn [{:keys [pred lemma obj]}]
                                    (some-> (#'open-world-ingest.nlp/derive-relation-type lemma pred obj)
                                            :label)))
                            set)]
    (doseq [{:keys [relation]} relations]
      (if (seq derived-labels)
        (is (or (contains? derived-labels relation)
                (= :links-to relation))
            (str "Relation " relation " must be derived from trace triple"))
        (is (= :links-to relation)
            (str "Expected fallback relation for sentence without triples: " sentence))))))

(deftest dracula-journal-openie-trace
  (let [{:keys [log world]} (run-scenario! journal-data)]
    (doseq [{:keys [sentence triples relations]} log]
      (println "Sentence:" sentence)
      (println "  OpenIE triples:" (map #(select-keys % [:subj :pred :obj :lemma]) triples))
      (println "  Ingested relations:" relations)
      (assert-derived-relations sentence triples relations))
    (testing "world model summarizes ego and context"
      (println "World model:" world)
      (is (some? (:me world)) "Expected :me entity to exist")
      (is (seq (remove #(= (:entity/label %) "Me") (:entities world)))
          "Expected additional world entities")
      (is (seq (:relations world)) "Expected at least one relation in world model"))))

(deftest frankenstein-letter-openie-trace
  (let [{:keys [log world]} (run-scenario! frankenstein-data)]
    (doseq [{:keys [sentence triples relations]} log]
      (println "Sentence:" sentence)
      (println "  OpenIE triples:" (map #(select-keys % [:subj :pred :obj :lemma]) triples))
      (println "  Ingested relations:" relations)
      (assert-derived-relations sentence triples relations))
    (testing "world model summarizes narrator, interlocutor, and ambitions"
      (println "World model:" world)
      (is (some? (:me world)) "Expected :me entity to exist")
      (is (some? (:you world)) "Expected :you entity to be recognised")
      (is (>= (count (:relations world)) 8) "Expected multiple traced relations in world model")
      (is (seq (remove #(= (:entity/label %) "Me") (:entities world)))
          "Expected additional world entities"))))

(deftest raymond-experiment-openie-trace
  (let [{:keys [log world]} (run-scenario! raymond-data)]
    (doseq [{:keys [sentence triples relations]} log]
      (println "Sentence:" sentence)
      (println "  OpenIE triples:" (map #(select-keys % [:subj :pred :obj :lemma]) triples))
      (println "  Ingested relations:" relations)
      (assert-derived-relations sentence triples relations))
  (testing "world model highlights the experiment partnership"
    (println "World model:" world)
    (let [labels (set (map :entity/label (:entities world)))]
      (is (contains? labels "Dr. Raymond") "Expected Dr. Raymond in world model")
      (is (contains? labels "Clarke") "Expected Clarke in world model"))
    (is (>= (count (:relations world)) 8) "Expected numerous traced relations for the experiment narrative"))))

(deftest slash-predicate-openie-trace
  (let [{:keys [log world]} (run-scenario! slash-data)
        relation-labels (map :relation (:relations world))
        namespaced-labels (filter namespace relation-labels)]
    (doseq [{:keys [sentence triples relations]} log]
      (println "Sentence:" sentence)
      (println "  OpenIE triples:" (map #(select-keys % [:subj :pred :obj :lemma]) triples))
      (println "  Ingested relations:" relations)
      (assert-derived-relations sentence triples relations))
    (testing "slash predicates produce namespaced relation labels"
      (is (seq namespaced-labels) "Expected at least one namespaced relation label")
      (doseq [label namespaced-labels]
        (is (some? (namespace label))
            (str "Namespaced label should retain a predicate namespace: " label))))))
