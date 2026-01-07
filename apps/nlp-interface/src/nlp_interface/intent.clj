(ns nlp-interface.intent
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [nlp-interface.features :as feat]))

(def ^:private thresholds-path "intent_thresholds.edn")

(def ^:private thresholds
  (delay
    (let [defaults {:default 0.6}]
      (if-let [res (io/resource thresholds-path)]
        (merge defaults (edn/read-string (slurp res)))
        defaults))))

(defn- threshold-for [intent-type]
  (let [kw (when intent-type (if (keyword? intent-type)
                               intent-type
                               (keyword (name intent-type))))]
    (get @thresholds kw (get @thresholds :default 0.6))))

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
   :activation {:keywords #{"agitat" "amp" "anger" "angry" "annoy" "driv"
                            "determ" "fire" "furious" "irritat" "livid"
                            "mad" "motivat" "pressur" "rile" "resolv"
                            "resolut" "urgent" "urgency"}
                :phrases #{"amped up" "fed up" "fired up" "pissed off"
                           "riled up" "under pressure" "worked up"}
                :priority 0.2}
   :attraction {:keywords #{"appeal" "attract" "captivat" "compel" "curios"
                            "draw" "engag" "fascinat" "interest" "intrigu"
                            "magnet" "pull" "tempt"}
                :priority 0.2}
   :joy {:keywords #{"alive" "cheer" "content" "delight" "ease" "energise"
                     "energiz" "glad" "happy" "joy" "light" "play"
                     "pleas" "relax" "satisf" "uplift"}
         :phrases #{"open hearted" "open-hearted"}
         :priority 0.2}
   :fatigue {:keywords #{"drain" "dull" "exhaust" "fatigu" "flat" "heavy"
                         "overload" "overwhelm" "sleepy" "sluggish" "spent"
                         "tired" "weary" "worn"}
             :phrases #{"burned out" "burnt out" "worn out"}
             :priority 0.2}
   :anxiety {:keywords #{"afraid" "anxiety" "anxious" "apprehens" "concern"
                         "fear" "nervous" "panic" "pressur" "scared"
                         "stress" "tense" "uneasy" "worr"}
             :phrases #{"on edge"}
             :priority 0.2}
   :withdrawal {:keywords #{"avoid" "block" "closed" "frozen" "guard"
                            "hesitat" "reluct" "reserved" "stuck" "withdraw"}
                :phrases #{"holding back" "shut down"}
                :priority 0.2}
   :frustration {:keywords #{"annoy" "conflict" "exasperat" "frustrat"
                             "impatient" "irritat" "resent" "stymi"
                             "thwart" "torn"}
                 :priority 0.2}
   :sadness {:keywords #{"blue" "depress" "deflat" "disappoint" "discourag"
                         "down" "empty" "heavy" "hollow" "low" "sad"}
             :phrases #{"heavy-hearted"}
             :priority 0.2}
   :numbness {:keywords #{"apathe" "blank" "detach" "disconnect" "indifferent"
                          "numb" "unmoved"}
              :phrases #{"blanked out" "checked out" "dead inside" "shut off"}
              :priority 0.2}
   :orientation {:keywords #{"alert" "attentive" "aware" "certainty" "clarity"
                             "clear" "confus" "distract" "doubt" "focused"
                             "lost" "skeptic" "surpris" "uncertain" "unclear"}
                 :priority 0.2}
   :social {:keywords #{"accept" "appreciat" "belong" "connect" "dismiss"
                        "exclud" "ignored" "insecure" "lonely" "reject"
                        "seen" "secure" "support" "unseen" "valued"}
            :priority 0.2}
   :regulation {:keywords #{"agitat" "balance" "calm" "equanim" "ground"
                            "overstimul" "restless" "settled" "unbalance"}
                :priority 0.2}
   :expressive-behavior {:keywords #{"art" "dance" "laugh" "music" "paint"
                                     "poem" "sing" "song" "theater" "yell"
                                     "perform"}}
   :glory {:keywords #{"admire" "boast" "brilliant" "conquer" "crown"
                       "fame" "glory" "grand" "hero" "honor" "king"
                       "majestic" "noble" "proud" "triumph" "victory"
                       "wealth"}}})

(defn- tokenize [text]
  (->> (re-seq #"[\p{L}\p{Nd}]+(?:'[\p{L}\p{Nd}]+)?" (or text ""))
        (map #(apply str %))
        vec))

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

(defn- dictionary-analyze [text tokens]
  (let [tokens (vec (or tokens (tokenize text)))
        lower-text (str/lower-case (or text ""))
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
         :conf (:confidence best)
         :source :dictionary})
      {:type :unknown :conf 0.5 :source :dictionary})))

(def ^:private fallback-specs
  [{:type :inquiry
    :base 0.35
    :weights {:wh-question 0.35
              :question-mark 0.25
              :modal 0.1
              :politeness 0.05
              :imperative -0.2
              :negation -0.05}
    :lexicon {:status 0.06 :planning 0.05 :followup 0.04}}
   {:type :directive
    :base 0.3
    :weights {:imperative 0.4
              :modal 0.1
              :politeness 0.05
              :exclamation 0.05
              :wh-question -0.15}
    :lexicon {:support 0.05 :followup 0.04}}
   {:type :scheduling
    :base 0.32
    :weights {:temporal 0.35
              :first-person-future 0.1
              :modal 0.08
              :politeness 0.04}
    :lexicon {:scheduling 0.22}}
   {:type :status-update
    :base 0.3
    :weights {:first-person-future 0.12
              :temporal 0.08
              :question-mark 0.05}
    :lexicon {:status 0.32 :planning 0.08}}
   {:type :support-request
    :base 0.28
    :weights {:modal 0.12
              :negation 0.12
              :question-mark 0.07
              :politeness 0.05}
    :lexicon {:support 0.35 :risk 0.08}}
   {:type :planning
    :base 0.28
    :weights {:first-person-future 0.2
              :temporal 0.1
              :modal 0.1
              :question-mark 0.05}
    :lexicon {:planning 0.32 :status 0.05}}
   {:type :follow-up
    :base 0.27
    :weights {:modal 0.1
              :question-mark 0.08
              :politeness 0.05}
    :lexicon {:followup 0.38 :status 0.05}}
   {:type :risk-escalation
    :base 0.26
    :weights {:negation 0.18
              :exclamation 0.08
              :modal 0.05}
    :lexicon {:risk 0.36 :support 0.07}}
   {:type :delivery-commit
    :base 0.3
    :weights {:first-person-future 0.18
              :temporal 0.12
              :modal 0.08
              :politeness 0.04}
    :lexicon {:delivery 0.32 :status 0.05}}])

(defn- feature-val [features k]
  (double (get features k 0.0)))

(defn- lexicon-val [features k]
  (let [count (double (get-in features [:lexicon k] 0.0))]
    (min 1.0 count)))

(defn- score-candidate [features {:keys [type base weights lexicon]}]
  (let [base (double (or base 0.0))
        feature-score (reduce (fn [acc [k weight]]
                                (+ acc (* weight (feature-val features k))))
                              0.0
                              (or weights {}))
        lex-score (reduce (fn [acc [k weight]]
                            (+ acc (* weight (lexicon-val features k))))
                          0.0
                          (or lexicon {}))
        raw (+ base feature-score lex-score)
        clamped (-> raw (max 0.0) (min 0.99))]
    {:type type
     :score clamped
     :conf clamped}))

(defn- fallback-candidates [features]
  (->> fallback-specs
       (map #(score-candidate features %))
       (filter #(> (:conf %) 0.0))
       (sort-by (comp - :conf))
       vec))

(defn analyze
  "Return a deterministic intent map. Optional opts may include :tokens." 
  ([text]
   (analyze text {}))
  ([text {:keys [tokens]}]
   (let [tokens (vec (or tokens (tokenize text)))
         primary (dictionary-analyze text tokens)
         features (when (= :unknown (:type primary))
                    (feat/extract text tokens))
         candidates (if features
                      (fallback-candidates features)
                      [])
         top-candidate (first candidates)
         promoted? (when top-candidate
                     (>= (:conf top-candidate)
                         (threshold-for (:type top-candidate))))
         base-intent (cond
                       promoted? (assoc top-candidate
                                        :source :fallback)
                       features (assoc primary :source :fallback)
                       :else primary)
         final-intent (cond-> base-intent
                         promoted? identity
                         (not promoted?) (assoc :type (:type base-intent)
                                                :conf (:conf base-intent)))]
     (assoc final-intent :intent-candidates candidates))))
