(ns nlp-interface.nlp-interface
  (:require [clojure.string :as str]
            [graph-memory.main :as gm]
            [nlp-interface.ner-er :as ner-er]
            [nlp-interface.ner-v4 :as ner-v4])
  (:import (java.time Instant ZoneId ZonedDateTime))
  (:gen-class))

(def gazetteer (delay (ner-er/load-gazetteer)))

(defn tokenize [text]
  (->> (re-seq #"[\p{L}\p{Nd}]+(?:'[\p{L}\p{Nd}]+)?" text)
        (map #(apply str %))
        vec))

(def ^:private entity-type->catalog-key
  {:person :people
   :place :places
   :org :orgs
   :project :projects
   :tool :tools})

(defn- entity-catalog
  [db]
  (let [entities (gm/entities-by-name db nil)]
    (->> entities
         (reduce (fn [{:keys [seen data] :as acc} ent]
                   (let [name (:entity/name ent)
                         type (:entity/type ent)
                         key (get entity-type->catalog-key type)
                         trimmed (some-> name str/trim)
                         normalized (some-> trimmed str/lower-case)]
                     (if (and key (seq trimmed) (not (str/blank? trimmed))
                              (not (contains? (get seen key #{}) normalized)))
                       {:seen (update seen key (fnil conj #{}) normalized)
                        :data (update data key (fnil conj [])
                                      {:label trimmed :layer :catalog :source :catalog})}
                       acc)))
                 {:seen {} :data {}})
         :data)))

(def ^:private default-confidence
  {:start 0.55
   :step 0.07
   :max 0.9})

(def ^:private intent-dictionary
  {:greet {:keywords #{"hello" "hi" "hey" "greetings" "hola" "howdy" "morning"
                       "afternoon" "evening" "salutations" "sup"}
           :phrases #{"good morning" "good afternoon" "good evening" "hey there"
                      "hello there" "hi there" "how are you"}
           :confidence {:start 0.9 :step 0.09 :max 0.99}
           :priority 0.5}
   :farewell {:keywords #{"bye" "goodbye" "farewell" "ciao" "adios" "later"
                          "cheerio" "goodnight" "ta-ta" "peace"}
              :phrases #{"see you" "see ya" "see-ya" "take care" "talk soon"
                         "catch you later" "later on" "bye bye"}
              :confidence {:start 0.9 :step 0.09 :max 0.99}
              :priority 0.45}
   :gratitude {:keywords #{"thanks" "thank" "appreciate" "grateful" "gratitude"
                           "cheers" "obliged" "merci" "thx"}
               :phrases #{"thank you" "thanks a lot" "thanks so much"
                          "much obliged"}
               :confidence {:start 0.7 :step 0.06 :max 0.95}}
   :apology {:keywords #{"sorry" "apolog" "pardon" "regret" "forgive" "excuse"
                         "oops"}
             :phrases #{"my apologies" "i'm sorry" "i am sorry" "pardon me"
                        "please forgive"}
             :confidence {:start 0.68 :step 0.06 :max 0.94}}
   :affirmation {:keywords #{"yes" "yeah" "yep" "sure" "certainly" "absolutely"
                             "indeed" "affirmative" "right" "ok" "okay" "roger"
                             "yup"}
                 :phrases #{"that works" "sounds good" "makes sense"}
                 :confidence {:start 0.65 :step 0.05 :max 0.92}}
   :negation {:keywords #{"no" "nope" "nah" "never" "cannot" "can't" "wont"
                          "won't" "refuse" "decline" "negative" "not"}
              :phrases #{"i don't" "do not" "wouldn't" "shouldn't" "couldn't"}
              :confidence {:start 0.62 :step 0.05 :max 0.9}}
   :help-request {:keywords #{"help" "assist" "support" "guide" "explain"
                               "clarify" "teach" "show" "demonstrate"
                               "instruct" "walkthrough"}
                  :phrases #{"can you" "could you" "would you" "i need"
                             "please help" "help me" "show me" "tell me how"}
                  :confidence {:start 0.68 :step 0.05 :max 0.93}}
   :primary-need-orality {:keywords #{"absinth" "ale" "appetite" "banana" "bean"
                                      "beef" "beer" "belly" "berry" "beverage"
                                      "biscuit" "bite" "brandy" "bread" "breakfast"
                                      "butter" "cake" "candy" "caviar" "cheese"
                                      "cherry" "chocolate" "cider" "coffee" "consume"
                                      "cook" "corn" "cream" "dessert" "devour"
                                      "dine" "dinner" "dish" "drink" "eat" "feast"
                                      "fruit" "garlic" "gin" "glutton" "grape"
                                      "gruel" "gulp" "honey" "hunger" "juice"
                                      "lemon" "lick" "lunch" "meal" "meat" "milk"
                                      "nectar" "nourish" "nut" "olive" "pastry"
                                      "pepper" "pork" "potato" "pumpkin" "quench"
                                      "raspberry" "restaurant" "rice" "roast"
                                      "rum" "salad" "saliva" "salt" "sauce"
                                      "sherbet" "soup" "spoon" "starve" "stomach"
                                      "strawberry" "sugar" "supper" "swallow"
                                      "tea" "thirst" "tongue" "tomato" "veal"
                                      "vegetable" "vodka" "wine" "yeast"}
                          :confidence {:start 0.66 :step 0.04 :max 0.9}}
   :primary-need-anality {:keywords #{"anal" "anus" "arse" "bowel" "buttock"
                                      "constipat" "defecat" "dirt" "dung" "fecal"
                                      "feces" "filth" "impure" "latrine" "manure"
                                      "mud" "offal" "ooze" "piss" "pollute" "rectum"
                                      "reek" "sewer" "shit" "slimy" "stench"
                                      "stink" "toilet" "unclean" "urine"}
                          :confidence {:start 0.65 :step 0.05 :max 0.9}}
   :primary-need-sex {:keywords #{"adultery" "allure" "caress" "carnal"
                                   "clitori" "cohabit" "coitus" "copulat"
                                   "courtesan" "erotic" "fondl" "fornicat"
                                   "genital" "harem" "harlot" "incest"
                                   "kiss" "lust" "masturbat" "naked" "orgasm"
                                   "peni" "pregnan" "procreat" "prostitut"
                                   "seduc" "sensual" "sexual" "sexy" "slut"
                                   "vagina" "volupt" "whor"}
                       :confidence {:start 0.66 :step 0.05 :max 0.91}}
   :sensation-touch {:keywords #{"brush" "contact" "cuddle" "gentle" "handle"
                                 "itch" "massage" "prickl" "rough" "rub"
                                 "scratch" "smooth" "snuggle" "sting" "stroke"
                                 "texture" "tickl" "tingl" "touch"}}
   :sensation-taste {:keywords #{"aftertaste" "bitter" "delici" "flavor"
                                 "piquant" "savory" "savour" "sour" "spice"
                                 "sweet" "tang" "tasty" "toothsome" "vinegar"}}
   :sensation-odor {:keywords #{"aroma" "fragrant" "fume" "incense" "musk"
                                "odor" "perfume" "pungent" "scent" "smell"
                                "sniff" "scented"}}
   :sensation-sound {:keywords #{"audible" "bang" "bell" "boom" "buzz" "chord"
                                 "clang" "echo" "hiss" "hum" "melody" "music"
                                 "noise" "ring" "shout" "sing" "sound" "thud"
                                 "tone" "whistle"}}
   :sensation-vision {:keywords #{"blink" "bright" "color" "flash" "gaze"
                                  "glance" "glimmer" "glitter" "glow" "green"
                                  "light" "look" "observe" "scan" "see" "shine"
                                  "sight" "vision" "watch"}}
   :sensation-cold {:keywords #{"arctic" "chill" "cold" "cool" "freeze"
                                "frost" "glacier" "hoar" "ice" "numb"
                                "polar" "shiver" "snow" "winter"}}
   :sensation-hard {:keywords #{"brass" "brittle" "copper" "granite" "iron"
                                "marble" "metal" "rigid" "rock" "solid"
                                "steel" "stone"}}
   :sensation-soft {:keywords #{"delicate" "downy" "feather" "fluffy" "gentle"
                                "gossamer" "mellow" "silk" "soft" "tender"
                                "velvet"}}
   :passivity {:keywords #{"calm" "content" "dead" "ease" "idle" "inactive"
                            "languid" "lazy" "peaceful" "relax" "rest"
                            "silent" "still" "tranquil" "yield"}}
   :voyage {:keywords #{"caravan" "cruise" "embark" "explor" "journey"
                         "migrat" "navigate" "pilgrim" "ride" "roam" "sail"
                         "tour" "travel" "trek" "trip" "voyage" "wander"}}
   :random-movement {:keywords #{"agitat" "churn" "commotion" "fidget"
                                 "flurry" "jerk" "lurch" "quiver" "reel"
                                 "shake" "spin" "stagger" "sway" "throb"
                                 "twitch" "vibrate"}}
   :diffusion {:keywords #{"blur" "cloud" "darken" "dim" "fade" "fog" "haze"
                           "mist" "murky" "shadow" "twilight" "uncertain"
                           "vague" "veil"}}
   :chaos {:keywords #{"aimless" "anarchy" "catastrophe" "chaos" "confus"
                       "crowd" "discord" "disorder" "entangle" "haphazard"
                       "jumble" "labyrinth" "lawless" "mess" "mob" "random"
                       "riot" "ruin" "wild"}}
   :unknown {:keywords #{"bizarre" "cryptic" "enigma" "fantastic" "formless"
                         "mystic" "mystery" "occult" "odd" "secret"
                         "strange" "unknown" "unseen" "void"}}
   :timelessness {:keywords #{"always" "ceaseless" "endless" "eternal"
                              "everlasting" "forever" "permanent" "perpetual"
                              "timeless" "unceasing"}}
   :consciousness {:keywords #{"asleep" "awake" "dream" "ecstasy" "frenzy"
                               "hallucinat" "hypno" "insane" "madness"
                               "sleep" "slumber" "trance" "visionary"}}
   :brink-passage {:keywords #{"bridge" "border" "boundary" "door" "edge"
                               "entrance" "gateway" "limit" "margin"
                               "passage" "path" "threshold" "window"}}
   :narcissism {:keywords #{"arm" "body" "bone" "brain" "chest" "eye" "face"
                            "flesh" "hand" "head" "heart" "leg" "neck"
                            "skin" "skull"}}
   :concreteness {:keywords #{"ahead" "among" "apart" "away" "behind"
                               "between" "center" "corner" "distance"
                               "inside" "near" "outside" "place" "space"
                               "surface" "toward" "within"}}
   :icarian-ascend {:keywords #{"ascend" "climb" "elevate" "flight" "float"
                                "fly" "hover" "jump" "leap" "lift" "rise"
                                "soar" "spring" "swing" "upward"}}
   :icarian-descent {:keywords #{"descend" "descent" "dip" "down" "fall"
                                 "plunge" "sink" "slide" "slip" "slope"
                                 "swoop" "tumble"}}
   :icarian-fire {:keywords #{"blaze" "burn" "ember" "fire" "flame" "heat"
                               "ignite" "inferno" "scorch" "smoke" "spark"
                               "sizzle"}}
   :icarian-water {:keywords #{"bath" "brook" "creek" "damp" "dew" "drench"
                                "flood" "lake" "moist" "ocean" "pond" "rain"
                                "river" "sea" "shower" "spray" "stream"
                                "swim" "water" "wave" "wet"}}
   :abstract-thought {:keywords #{"abstract" "analy" "concept" "consider"
                                  "decide" "evaluate" "idea" "logic" "plan"
                                  "reason" "study" "theor" "think"
                                  "understand"}}
   :social-behavior {:keywords #{"accept" "agree" "ask" "assist"
                                  "chat" "communicat" "cooperate" "discuss"
                                  "greet" "help" "invite" "meet" "offer"
                                  "share" "speak" "talk" "welcome"}}
   :instrumental-behavior {:keywords #{"achieve" "build" "construct" "create"
                                       "develop" "earn" "gain" "labor"
                                       "make" "manufactur" "plan" "produce"
                                       "repair" "sell" "work"}}
   :restraint {:keywords #{"arrest" "block" "bound" "confine" "control"
                            "forbid" "guard" "halt" "limit" "obey"
                            "prevent" "restrict" "restrain" "stop"}}
   :order {:keywords #{"arrange" "balance" "class" "consistent" "index"
                        "list" "method" "neat" "order" "organize"
                        "pattern" "regular" "stable" "system"}}
   :temporal-representation {:keywords #{"again" "already" "ancient" "daily"
                                         "earlier" "hour" "moment" "now"
                                         "often" "once" "season" "soon"
                                         "today" "week" "year" "yesterday"}}
   :moral-imperative {:keywords #{"duty" "ethic" "honest" "honor" "law"
                                  "moral" "ought" "principle" "proper"
                                  "responsibility" "right" "upright"
                                  "virtue"}}
   :positive-affect {:keywords #{"amuse" "celebrate" "cheer" "delight"
                                  "enjoy" "excite" "funny" "glad" "happy"
                                  "joy" "laugh" "merry" "pleasure" "smile"
                                  "thrill" "upbeat"}}
   :anxiety {:keywords #{"afraid" "alarm" "anguish" "anxious" "dread"
                         "fear" "nervous" "panic" "scared" "terrify"
                         "uneasy" "worry"}}
   :sadness {:keywords #{"alas" "depress" "despair" "disappoint" "dismal"
                         "grief" "hopeless" "lament" "lonely" "miserable"
                         "mourn" "regret" "sad" "sorrow" "weep"}
             :confidence {:start 0.6 :step 0.07 :max 0.92}}
   :affection {:keywords #{"adore" "affection" "beloved" "cherish" "dear"
                           "embrace" "fond" "friend" "kind" "love"
                           "romance" "sweetheart" "sympathy" "welcome"}}
   :aggression {:keywords #{"abuse" "anger" "argu" "attack" "battle" "beat"
                            "blame" "destroy" "fight" "hate" "kill" "rage"
                            "revenge" "stab" "violence" "war"}}
   :expressive-behavior {:keywords #{"art" "dance" "laugh" "music" "paint"
                                     "poem" "sing" "song" "theater" "yell"
                                     "perform"}}
   :glory {:keywords #{"admire" "boast" "brilliant" "conquer" "crown"
                       "fame" "glory" "grand" "hero" "honor" "king"
                       "majestic" "noble" "proud" "triumph" "victory"
                       "wealth"}}})

(defn- matches-stem? [stem token]
  (let [stem (str/lower-case stem)
        token (str/lower-case token)]
    (cond
      (= stem token) true
      (<= (count stem) 3) false
      :else (str/starts-with? token stem))))

(defn- phrase-matches [phrases text]
  (reduce (fn [acc phrase]
            (let [phrase* (str/lower-case phrase)]
              (if (str/includes? text phrase*)
                (inc acc)
                acc)))
          0
          phrases))

(defn- evaluate-category [tokens lower-text [intent {:keys [keywords phrases confidence priority]
                                                     :or {priority 0}}]]
  (let [keywords (or keywords #{})
        matches (reduce (fn [acc token]
                          (if (some #(matches-stem? % token) keywords)
                            (inc acc)
                            acc))
                        0
                        tokens)
        phrase-count (when (seq phrases)
                       (phrase-matches phrases lower-text))
        total (+ matches (or phrase-count 0))]
    (when (pos? total)
      (let [{:keys [start step max]}
            (merge default-confidence (or confidence {}))
            confidence (double (min max (+ start (* step total))))]
        {:type intent
         :matches total
         :score (+ total priority)
         :confidence confidence}))))

(defn analyze [text]
  (let [tokens (tokenize text)
        lower-text (str/lower-case text)
        results (keep #(evaluate-category tokens lower-text %)
                      intent-dictionary)]
    (if (seq results)
      (let [best (reduce (fn [current candidate]
                           (if (pos? (compare [(:score candidate) (:confidence candidate)]
                                              [(:score current) (:confidence current)]))
                             candidate
                             current))
                         results)]
        {:type (:type best)
         :conf (:confidence best)})
      {:type :unknown :conf 0.5})))

(defn pos-tag [tokens]
  (->> tokens
       (map (fn [t]
              (cond
                (re-matches #"[A-Z][a-z]+" t) [t "NNP"]
                (re-matches #"[0-9]+" t)      [t "CD"]
                (re-matches #".+ing" t)       [t "VBG"]
                :else                          [t "NN"])))
       vec))

(defn parse-tree [tagged]
  (into [:utterance]
        (map (fn [[token tag]] [tag token]) tagged)))

(defn handle-input [db text ts]
  (let [utt-node (gm/add-utterance! db text ts)
        intent (analyze text)
        intent-node (gm/add-intent! db intent)
        link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
        tokens (tokenize text)
        tagged (pos-tag tokens)
        tags (mapv second tagged)
        entities-raw (ner-er/ner tokens tags (force gazetteer))
        entities (mapv (fn [ent]
                         (let [entity (gm/ensure-entity! db ent)]
                           (gm/add-mention! db (:id utt-node) (:id entity) (:span ent))
                           (assoc ent :id (:id entity))))
                       entities-raw)
        entity-index (into {} (map (fn [{:keys [name id]}] [name id]) entities))
        relations-raw (ner-er/relations {:tokens tokens
                                         :entities entities})]
    (doseq [{:keys [type src dst prov]} relations-raw]
      (when (and src dst)
        (when-let [src-id (get entity-index src)]
          (when-let [dst-id (get entity-index dst)]
            (gm/add-relation! db {:type type
                                  :src-id src-id
                                  :dst-id dst-id
                                  :prov prov})))))
    {:utterance utt-node
     :intent intent
     :tokens tokens
     :pos tagged
     :parse-tree (parse-tree tagged)
     :entities (mapv (fn [{:keys [name type span]}]
                       {:name name :type type :span span})
                     entities)
     :relations (mapv #(select-keys % [:type :src :dst]) relations-raw)
     :links [link]}))

(defn- now-from-ts [ts]
  (ZonedDateTime/ofInstant (Instant/ofEpochMilli ts) (ZoneId/systemDefault)))

(defn handle-input-v4
  "Return enriched NLP data using the tiered v4 NER pipeline."
  ([db text ts]
   (handle-input-v4 db text ts {}))
  ([db text ts opts]
   (let [utt-node (gm/add-utterance! db text ts)
         intent (analyze text)
         intent-node (gm/add-intent! db intent)
         link (gm/link! db (:db/eid utt-node) (:db/eid intent-node) :derives)
         tokens (tokenize text)
         tagged (pos-tag tokens)
         now (now-from-ts ts)
         catalog (entity-catalog db)
         ner-opts (assoc opts :catalog catalog)
         entities (ner-v4/recognize-entities tokens tagged text now ner-opts)
         stored (mapv (fn [ent]
                        (let [entity (gm/ensure-entity! db {:name (:label ent)
                                                           :type (:type ent)})]
                          (gm/add-mention! db (:id utt-node) (:id entity) (:span ent))
                          (assoc ent
                                 :name (:label ent)
                                 :entity-id (:id entity)
                                 :entity-db (:db/eid entity))))
                      entities)
         ordered (sort-by (comp :start :span) stored)
         relations (->> (partition 2 1 ordered)
                        (map (fn [[a b]]
                               (let [between (subs text (:end (:span a)) (:start (:span b)))
                                     lower (str/lower-case between)
                                     src (:name a)
                                     dst (:name b)]
                                 (cond
                                   (and (= (:type a) :person) (= (:type b) :place)
                                        (re-find #"\b(in|at|from|to)\b" lower))
                                   {:type :located-in :src src :dst dst}

                                   (and (= (:type b) :date)
                                        (re-find #"\b(on|by|before|after)\b" lower))
                                   {:type :scheduled :src src :dst dst}

                                   (re-find #"\bwith\b" lower)
                                   {:type :with :src src :dst dst}

                                   :else
                                   {:type :links-to :src src :dst dst}))))
                        (remove nil?)
                        vec)]
     {:utterance utt-node
      :intent intent
      :tokens tokens
      :pos tagged
      :entities stored
      :relations relations
      :links [link]})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "nlp-interface ready"))
