(ns v2.nlp-interface
  "v2 — minimal NLP interface that builds a tiny graph memory from text.
   Drop-in hooks let you replace tokenize/pos-tag/wordnet with real ones later."
  (:require [clojure.string :as str]
            [v2.graph-memory :as gm]))

;; -----------------------------------------------------------------------------
;; Hook points (swap these out in v3)

(def ^:dynamic *tokenize
  "Return a vector of tokens for a string."
  (fn [s]
    ;; naive split; replace with your real tokenizer
    (->> (str/split s #"\s+")
         (remove str/blank?)
         vec)))

(def ^:dynamic *pos-tag
  "Return a vector of [token tag] pairs. Very naive heuristic for demo."
  (fn [tokens]
    (->> tokens
         (map (fn [t]
                (cond
                  (re-matches #"[A-Z][a-z]+" t) [t "NNP"]   ;; Simple Proper Noun guess
                  (re-matches #"[0-9]+" t)        [t "CD"]   ;; Number
                  (re-matches #".+ing" t)         [t "VBG"]  ;; Verb-ING
                  :else                            [t "NN"])))
         vec)))

(def ^:dynamic *wordnet
  "Return a seq of sense maps or []: (*wordnet \"dog\" :noun) => [{:gloss \"...\"} ...].
   Default: no-op empty result; swap in clj-wordnet in v3."
  (fn [word pos] []))

;; -----------------------------------------------------------------------------
;; Emergent type derivation (evidence thresholding lives in gm/bump-type!)

(defn- entity-candidates
  "Collapse runs of NNP into a surface entity string."
  [tagged]
  (->> tagged
       (partition-by (fn [[_ pos]] (str/starts-with? pos "NNP")))
       (filter #(= "NNP" (second (first %))))
       (map (fn [chunk] (str/join " " (map first chunk))))
       (remove str/blank?)))

(defn- derive-token-types!
  "Add soft types for a single token."
  [id token]
  (when (re-matches #"[0-9]+" token) (gm/bump-type! id :Number?))
  (when (Character/isUpperCase ^char (first token)) (gm/bump-type! id :Capitalized?))
  (when (re-matches #"[A-Za-z]+" token) (gm/bump-type! id :Word?))
  ;; WordNet backoff (replace with real *wordnet* later)
  (when (seq (*wordnet token :noun)) (gm/bump-type! id :Noun?))
  (when (seq (*wordnet token :verb)) (gm/bump-type! id :Verb?))
  id)

;; -----------------------------------------------------------------------------
;; Ingestion pipeline

(defn ingest-utterance!
  "Create an :Utterance node, attach :mentions edges to token and entity nodes,
   and update emergent types. Returns utterance id."
  [text]
  (let [u (gm/add-node! {:label text :types #{:Utterance}})]
    (let [tokens (*tokenize text)
          tagged  (*pos-tag tokens)
          ents    (entity-candidates tagged)]
      ;; link tokens
      (doseq [t tokens]
        (let [tid (gm/upsert-node-by-label! t)]
          (gm/add-edge! u :mentions tid)
          (derive-token-types! tid t)))
      ;; link “entity spans”
      (doseq [e ents]
        (let [eid (gm/upsert-node-by-label! e)]
          (gm/bump-type! eid :Person?) ;; very crude heuristic
          (gm/add-edge! u :mentions eid))))
    u))

;; -----------------------------------------------------------------------------
;; Demo “answer” that uses recent context

(defn recent-entities
  "Return up to N recently mentioned nodes with label & types."
  ([n]
   (->> (gm/last-utterances n)
        (mapcat #(gm/edges-from % :mentions))
        (map :dst)
        (distinct)
        (map gm/node)
        (map #(select-keys % [:label :types]))
        (take 20)))
  ([] (recent-entities 5)))

(defn answer
  "Ingest text, then return a simple acknowledgement that lists recognized entities/tokens."
  [text]
  (let [_u (ingest-utterance! text)
        ents (recent-entities 3)]
    (str "noted. you mentioned: "
         (->> ents
              (map (fn [{:keys [label types]}]
                     (str label
                          (when (seq types)
                            (str "{" (->> types sort (str/join ",") ) "}")))))
              (str/join ", "))
         ".")))

;; -----------------------------------------------------------------------------
;; Example REPL session (commented)
(comment
  (require '[v2.graph-memory :as gm] :reload)
  (require '[v2.nlp-interface :as nlp] :reload)

  (nlp/answer "Met Serena at PatCon 30 and talked about Graph Memory.")
  ;; => "noted. you mentioned: Met{Word?,Capitalized?}, Serena{Person?,Word?,Capitalized?}, PatCon{Person?,Word?,Capitalized?}."

  (gm/save! "graph.edn")
  (gm/load! "graph.edn")

  ;; Swap in your own tokenizer/POS:
  (alter-var-root #'*tokenize (constantly (fn [s] (vec (re-seq #"\w+" s)))))
  (alter-var-root #'*pos-tag  (constantly (fn [toks] (mapv (fn [t] [t (if (re-matches #"[A-Z].*" t) "NNP" "NN")]) toks))))
)
