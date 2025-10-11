(ns nlp-interface.features
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private lexicon-path "intent_lexicons.edn")

(def ^:private lexicons
  (delay
    (if-let [res (io/resource lexicon-path)]
      (->> (edn/read-string (slurp res))
           (reduce (fn [acc [k entries]]
                     (assoc acc k (set (map #(-> % str/lower-case str/trim) entries))))
                   {}))
      {})))

(defn lexicon-names []
  (keys @lexicons))

(defn- tokenize [text]
  (->> (re-seq #"[\p{L}\p{Nd}]+(?:'[\p{L}\p{Nd}]+)?" (or text ""))
        (map #(apply str %))
        vec))

(defn- lower-tokens [tokens]
  (mapv #(-> % (or "") str/lower-case) tokens))

(def ^:private wh-words
  #{"what" "why" "who" "where" "when" "which" "whose" "whom" "how"})

(def ^:private modal-words
  #{"should" "could" "would" "might" "may" "can" "cannot" "can't"
    "won't" "will" "shall" "must" "ought"})

(def ^:private politeness-markers
  #{"please" "kindly" "thank" "thanks" "appreciate" "grateful" "sorry"})

(def ^:private imperative-seeds
  #{"show" "send" "give" "provide" "share" "review" "check" "confirm"
    "schedule" "summarize" "summarise" "explain" "update" "create" "draft"
    "prepare" "remind" "ping" "book" "arrange" "call" "email" "post"
    "note" "list" "outline" "look" "tell" "complete" "finish" "resolve"
    "file" "deploy"})

(def ^:private negation-markers
  #{"no" "not" "never" "none" "nobody" "nothing" "neither" "nor"
    "can't" "cannot" "won't" "wouldn't" "shouldn't" "isn't" "aren't"
    "don't" "doesn't" "didn't" "haven't" "hasn't" "hadn't" "n't"})

(def ^:private temporal-lexemes
  #{"today" "tomorrow" "tonight" "yesterday" "week" "month" "year"
    "morning" "afternoon" "evening" "midnight" "noon" "later" "soon"
    "next" "upcoming" "deadline" "schedule" "reschedule" "calendar"
    "friday" "monday" "tuesday" "wednesday" "thursday" "saturday"
    "sunday" "weekend" "quarter" "annual" "daily" "hour" "hourly"})

(defn- question-mark? [text]
  (boolean (re-find #"\?" (or text ""))))

(defn- exclamation? [text]
  (boolean (re-find #"!" (or text ""))))

(defn- wh-question? [tokens]
  (let [tokens (lower-tokens tokens)]
    (or (some wh-words tokens)
        (some #(str/starts-with? % "wh-") tokens))))

(defn- imperative?
  [tokens]
  (let [tokens (lower-tokens tokens)
        first-token (first tokens)
        second-token (second tokens)]
    (or (imperative-seeds first-token)
        (and (= first-token "please")
             (imperative-seeds second-token))
        (and (= first-token "let") (= second-token "us"))
        (and (= first-token "let's") (some? second-token))
        (and (= first-token "please") (some? second-token)))))

(defn- modal? [tokens]
  (let [tokens (lower-tokens tokens)]
    (boolean (some modal-words tokens))))

(defn- polite? [tokens text]
  (let [tokens (lower-tokens tokens)
        lc-text (str/lower-case (or text ""))]
    (or (some politeness-markers tokens)
        (str/includes? lc-text "thank you")
        (str/includes? lc-text "thanks in advance")
        (str/includes? lc-text "would you mind"))))

(defn- first-person-future? [text]
  (boolean (re-find #"(?i)\b(i|we)\s+(will|shall|won't|can't|can|could|should|might|would|aim|plan)\b" (or text ""))))

(defn- temporal? [tokens text]
  (let [tokens (lower-tokens tokens)
        text (str/lower-case (or text ""))]
    (or (some temporal-lexemes tokens)
        (boolean (re-find #"(?i)\bnext\s+(week|month|quarter|year|monday|tuesday|wednesday|thursday|friday|saturday|sunday)\b" text))
        (boolean (re-find #"(?i)\b\d{1,2}(:\d{2})?\s*(am|pm)\b" text))
        (boolean (re-find #"(?i)\b\d{1,2}\s+(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)" text))
        (str/includes? text "calendar")
        (str/includes? text "schedule"))))

(defn- negation? [tokens]
  (let [tokens (lower-tokens tokens)]
    (or (some negation-markers tokens)
        (some #(str/ends-with? % "n't") tokens))))

(defn- lexicon-counts [text tokens]
  (let [token-set (set (lower-tokens tokens))
        lc-text (str/lower-case (or text ""))]
    (reduce (fn [acc [lex entries]]
              (let [count (reduce (fn [c entry]
                                     (let [entry (str/lower-case entry)]
                                       (if (or (token-set entry)
                                               (str/includes? lc-text entry))
                                         (inc c)
                                         c)))
                                   0
                                   entries)]
                (if (pos? count)
                  (assoc acc lex count)
                  acc)))
            {}
            @lexicons)))

(defn extract
  "Return a feature map for the provided text, optionally reusing pre-tokenized input."
  ([text]
   (extract text nil))
  ([text tokens]
   (let [tokens (if (seq tokens) (vec tokens) (tokenize text))]
     {:token-count (count tokens)
      :question-mark (if (question-mark? text) 1.0 0.0)
      :exclamation (if (exclamation? text) 1.0 0.0)
      :wh-question (if (wh-question? tokens) 1.0 0.0)
      :imperative (if (imperative? tokens) 1.0 0.0)
      :modal (if (modal? tokens) 1.0 0.0)
      :politeness (if (polite? tokens text) 1.0 0.0)
      :first-person-future (if (first-person-future? text) 1.0 0.0)
      :temporal (if (temporal? tokens text) 1.0 0.0)
      :negation (if (negation? tokens) 1.0 0.0)
      :lexicon (lexicon-counts text tokens)})))
