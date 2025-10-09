(ns nlp-interface.ner-v4
  "Tiered deterministic NER pipeline for basic-chat/v4."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def resource-paths
  {:gazetteer {:people "gazetteer/people.edn"
               :places "gazetteer/places.edn"
               :orgs "gazetteer/orgs.edn"
               :projects "gazetteer/projects.edn"
               :tools "gazetteer/tools.edn"}
   :patterns "patterns.edn"
   :stoplists "stoplists.edn"})

(def base-confidence
  {:gazetteer 0.70
   :catalog 0.68
   :pattern 0.65
   :pos 0.50
   :domain 0.45
   :fallback 0.35})

(def context-keywords
  {:person #{"meet" "met" "call" "email" "talk" "chat" "with"}
   :place #{"in" "to" "from" "at" "fly" "travel" "visit"}
   :project #{"project" "launch" "ship" "build"}
   :tool #{"run" "use" "install" "upgrade"}
   :org #{"at" "for" "with"}
   :date #{"on" "by" "before" "after"}
   :time #{"at" "around" "before" "after"}})

(def place-prepositions
  #{"in" "at" "to" "into" "from" "onto" "toward" "towards" "near" "around" "across" "over"})

(def direction-words
  #{"north" "south" "east" "west" "northeast" "northwest" "southeast" "southwest"})

(def sentence-initial-penalty 0.05)
(def alias-bonus 0.10)
(def context-bonus 0.10)
(def multi-token-bonus 0.25)

(defn- read-edn [path]
  (when-let [res (some-> path io/resource)]
    (binding [*read-eval* false]
      (some-> res slurp clojure.core/read-string))))

(defn- normalise-entry [entry]
  (cond
    (map? entry) (let [label (:label entry)
                       aliases (or (:aliases entry) [])]
                   (when label
                     (cond-> {:label label}
                       (seq aliases) (assoc :aliases (vec aliases)))))
    (string? entry) {:label entry}
    :else nil))

(defn- load-gazetteer []
  (into {}
        (for [[k path] (:gazetteer resource-paths)
              :let [entries (some-> path read-edn)]
              :when entries]
          [k (->> entries (keep normalise-entry) vec)])))

(defn- load-patterns []
  (let [raw (or (read-edn (:patterns resource-paths)) {})]
    (reduce-kv (fn [acc k regexes]
                 (assoc acc k (mapv (fn [pattern]
                                      (if (instance? java.util.regex.Pattern pattern)
                                        pattern
                                        (re-pattern pattern)))
                                    regexes)))
               {}
               raw)))

(defn- load-stoplists []
  (or (read-edn (:stoplists resource-paths)) {}))

(defonce ^:private resources*
  (delay {:gazetteer (load-gazetteer)
          :patterns (load-patterns)
          :stoplists (load-stoplists)}))

(defn resources []
  @resources*)

;; ----------------------------------------------------------------------------
;; Helpers

(defn- slugify [s]
  (-> (or s "")
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"-+" "-")
      (str/replace #"(^-|-$)" "")))

(defn- token-spans [text tokens]
  (let [lower (str/lower-case text)]
    (loop [idx 0 offset 0 acc []]
      (if (= idx (count tokens))
        acc
        (let [token (nth tokens idx)
              needle (str/lower-case token)
              pos (or (str/index-of lower needle offset)
                      (str/index-of lower needle))
              start (or pos offset)
              end (+ start (count token))]
          (recur (inc idx)
                 (if pos (+ end 1) offset)
                 (conj acc {:token token
                            :index idx
                            :start start
                            :end end})))))))

(defn- levenshtein [s t limit]
  (let [n (count s)
        m (count t)
        cols (inc m)
        prev (int-array cols)
        curr (int-array cols)]
    (cond
      (= s t) 0
      (zero? n) m
      (zero? m) n
      (> (Math/abs (- n m)) limit) (inc limit)
      :else
      (do
        (dotimes [j cols]
          (aset-int prev j j))
        (dotimes [i n]
          (aset-int curr 0 (inc i))
          (let [s-ch (nth s i)]
            (dotimes [j m]
              (let [t-ch (nth t j)
                    cost (if (= s-ch t-ch) 0 1)
                    deletion (inc (aget prev (inc j)))
                    insertion (inc (aget curr j))
                    substitution (+ cost (aget prev j))]
                (aset-int curr (inc j) (min deletion insertion substitution)))))
          (System/arraycopy curr 0 prev 0 cols))
        (aget prev m)))))

(defn- find-occurrences [text needle]
  (let [lower-text (str/lower-case text)
        lower-needle (str/lower-case needle)
        n (count lower-needle)]
    (loop [idx 0 acc []]
      (if-let [pos (str/index-of lower-text lower-needle idx)]
        (recur (inc pos) (conj acc {:start pos :end (+ pos n)}))
        acc))))

(defn- overlap? [{s1 :start e1 :end} {s2 :start e2 :end}]
  (< (max s1 s2) (min e1 e2)))

(defn- span-length [span]
  (max 0 (- (:end span 0) (:start span 0))))

(defn- context-window [token-data span]
  (let [indices (->> token-data
                     (map-indexed (fn [idx {:keys [start end]}]
                                    (when (and (< start (:end span))
                                               (> end (:start span)))
                                      idx)))
                     (remove nil?)
                     vec)]
    (if (seq indices)
      (let [tokens-vec (vec token-data)
            i0 (max 0 (- (first indices) 2))
            i1 (min (count tokens-vec) (+ (last indices) 3))]
        (map (comp str/lower-case :token) (subvec tokens-vec i0 i1)))
      [])))

;; ----------------------------------------------------------------------------
;; Gazetteer layer

(defn- gazetteer-type [k]
  (case k
    :people :person
    :places :place
    :orgs :org
    :projects :project
    :tools :tool
    k))

(defn- dedupe-by-label [entries]
  (letfn [(normalize [s]
            (some-> s str/lower-case))]
    (->> entries
         (reduce (fn [{:keys [seen entries]} entry]
                   (let [label (normalize (:label entry))]
                     (if (or (nil? label) (contains? seen label))
                       {:seen seen :entries entries}
                       {:seen (conj seen label)
                        :entries (conj entries entry)})))
                 {:seen #{} :entries []})
         :entries)))

(defn- normalise-catalog [catalog]
  (into {}
        (for [[k entries] catalog
              :let [normalized (->> entries
                                    (keep normalise-entry)
                                    (map #(assoc % :layer :catalog
                                                 :source :catalog))
                                    vec)]
              :when (seq normalized)]
          [k normalized])))

(defn- merge-gazetteers [base extra]
  (if (seq extra)
    (reduce (fn [acc [k entries]]
              (update acc k
                      (fn [current]
                        (->> (concat (or current []) entries)
                             dedupe-by-label
                             vec))))
            base
            extra)
    base))

(defn- expand-gazetteer [gz]
  (into {}
        (for [[type entries] gz]
          [type (mapv (fn [{:keys [label aliases layer source] :as entry}]
                        (let [variants (->> (cons label aliases)
                                            (remove #(or (nil? %) (str/blank? %)))
                                            vec)
                              layer' (or layer :gazetteer)
                              source' (or source (if (= layer' :gazetteer)
                                                   :gazetteer
                                                   layer'))]
                          (-> entry
                              (assoc :type type
                                     :variants variants
                                     :layer layer'
                                     :source source'))))
                      entries)])))

(defn- gazetteer-candidates [text {:keys [gazetteer]}]
  (let [entries (expand-gazetteer gazetteer)]
    (for [[etype ents] entries
          entry ents
          variant (:variants entry)
          span (find-occurrences text variant)
          :let [layer (:layer entry :gazetteer)
                source (:source entry :gazetteer)]]
      {:type (gazetteer-type etype)
       :label (:label entry)
       :matched variant
       :aliases (:aliases entry)
       :span span
       :layer layer
       :source source})))

(defn- fuzzy-gazetteer-candidates [token-data {:keys [gazetteer]}]
  (let [entries (expand-gazetteer gazetteer)]
    (for [[etype ents] entries
          entry ents
          variant (:variants entry)
          :when (<= (count variant) 5)
          token token-data
          :let [dist (levenshtein (str/lower-case variant)
                                   (str/lower-case (:token token)) 1)
                layer (:layer entry :gazetteer)
                source (:source entry :gazetteer)]
          :when (<= dist 1)]
      {:type (gazetteer-type etype)
       :label (:label entry)
       :matched (:token token)
      :aliases (:aliases entry)
      :span (select-keys token [:start :end])
      :layer layer
       :source source})))

(defn- merge-gazetteer [text token-data res]
  (concat (gazetteer-candidates text res)
          (fuzzy-gazetteer-candidates token-data res)))

;; ----------------------------------------------------------------------------
;; Pattern layer

(defn- safe-parse-date [s now]
  (cond
    (re-find #"(?i)tomorrow" s)
    (str (.plusDays (.toLocalDate now) 1))

    (re-matches #"\d{4}-\d{2}-\d{2}" s)
    s

    :else
    (try
      (let [year (.getYear (.toLocalDate now))
            fmt (.. (java.time.format.DateTimeFormatterBuilder.)
                    (parseCaseInsensitive)
                    (appendPattern "d MMM")
                    (parseDefaulting java.time.temporal.ChronoField/YEAR year)
                    (toFormatter java.util.Locale/UK))]
        (str (java.time.LocalDate/parse (str/trim s) fmt)))
      (catch Exception _ nil))))

(defn- safe-parse-time [s]
  (try
    (let [fmt (java.time.format.DateTimeFormatter/ofPattern "h[:mm] a" java.util.Locale/UK)]
      (.toString (java.time.LocalTime/parse (str/upper-case s) fmt)))
    (catch Exception _
      (when (re-find #"(?i)\b(noon|midnight)\b" s)
        (if (re-find #"(?i)noon" s) "12:00" "00:00")))))

(defn- pattern-candidates [text now {:keys [patterns]}]
  (mapcat
   (fn [[ptype regexes]]
     (mapcat
      (fn [regex]
        (let [matcher (re-matcher regex text)]
          (loop [acc []]
            (if (.find matcher)
              (let [match (.group matcher)
                    span {:start (.start matcher)
                          :end (.end matcher)}
                    value (case ptype
                            :date (safe-parse-date match now)
                            :time (safe-parse-time match)
                            :version (str/trim match)
                            match)
                    ent {:type (case ptype
                                 :date :date
                                 :time :time
                                 :version :version
                                 :file :file
                                 :url :url
                                 :email :email
                                 :issue :tag
                                 ptype)
                         :label match
                         :value value
                         :span span
                         :layer :pattern
                         :source :pattern}]
                (recur (conj acc ent)))
              acc))))
      regexes))
   patterns))

;; ----------------------------------------------------------------------------
;; POS / titlecase layer

(defn- titlecase? [token]
  (and (pos? (count token))
       (Character/isUpperCase ^char (first token))))

(defn- pos-titlecase-candidates [token-data pos-tags {:keys [stoplists gazetteer]}]
  (let [stopset (set (:titlecase-stop stoplists))
        label->type (into {}
                           (for [[k entries] gazetteer
                                 entry entries
                                  variant (->> (cons (:label entry) (:aliases entry))
                                               (remove #(or (nil? %) (str/blank? %))))]
                             [(str/lower-case variant) (gazetteer-type k)]))
        size (count token-data)]
    (loop [idx 0 acc []]
      (if (>= idx size)
        acc
        (let [{:keys [token start end]} (nth token-data idx)
              tag (second (nth pos-tags idx [nil ""]))]
          (if (and (= tag "NNP")
                   (titlecase? token)
                   (not (contains? stopset token)))
            (let [result (loop [j idx labels [token] span {:start start :end end}]
                           (let [next (inc j)
                                 next-token (nth token-data next nil)
                                 next-tag (second (nth pos-tags next [nil ""]))]
                             (if (and next-token
                                      (#{"NNP" "NNPS"} next-tag)
                                      (titlecase? (:token next-token)))
                               (recur next (conj labels (:token next-token))
                                      {:start (:start span) :end (:end next-token)})
                               {:next (inc j)
                                :labels labels
                                :span span})))]
              (let [{:keys [next labels span]} result
                    label (str/join " " labels)
                    multi? (> (count labels) 1)
                    normalized (str/lower-case label)
                    gaz-type (get label->type normalized)
                    prev-token (nth token-data (dec idx) nil)
                    prev-word (some-> prev-token :token str/lower-case)
                    context (context-window token-data span)
                    person-context? (some #(contains? (get context-keywords :person #{}) %) context)
                    direction? (some #(contains? direction-words %) context)
                    place-context? (or (contains? place-prepositions prev-word)
                                       direction?
                                       (and (some #(contains? (get context-keywords :place #{}) %) context)
                                            (not person-context?)))
                    org? (re-find #"(?i)(lab|ltd|inc|corp|university)" label)
                    etype (cond
                            gaz-type gaz-type
                            place-context? :place
                            org? :org
                            multi? :person
                            person-context? :person
                            (and (= 0 (:start span)) (not place-context?)) :person
                            :else (when gaz-type gaz-type))]
                (if etype
                  (recur next (conj acc {:type etype
                                         :label label
                                         :span span
                                         :layer :pos
                                         :source :pos}))
                  (recur next acc))))
            (recur (inc idx) acc)))))))

;; ----------------------------------------------------------------------------
;; Domain and fallback layers

(def domain-base-types
  {:projects :project
   :tools :tool
   :orgs :org})

(defn- domain-candidates [text {:keys [gazetteer]}]
  (for [[k entries] gazetteer
        :when (contains? domain-base-types k)
        entry entries
        variant (->> (cons (:label entry) (:aliases entry))
                     (remove #(or (nil? %) (str/blank? %))))
        span (find-occurrences text variant)]
    {:type (domain-base-types k)
     :label (:label entry)
     :matched variant
     :span span
     :layer :domain
     :source :domain}))

(defn- fallback-candidates [token-data pos-tags {:keys [stoplists]}]
  (let [stopset (set (:titlecase-stop stoplists))
        size (count token-data)]
    (loop [idx 0 acc []]
      (if (>= idx size)
        acc
        (let [{:keys [token start end index]} (nth token-data idx)
              tag (second (nth pos-tags idx [nil ""]))
              next-token (nth token-data (inc idx) nil)
              next-tag (second (nth pos-tags (inc idx) [nil ""]))
              next-title? (and next-token
                               (= next-tag "NNP")
                               (titlecase? (:token next-token))
                               (not (stopset (:token next-token))))]
          (if (and (= tag "NNP")
                   (titlecase? token)
                   (not (stopset token))
                   (or (> index 0) next-title?))
            (let [{:keys [next labels span]}
                  (loop [j idx
                         labels [token]
                         span {:start start :end end}]
                    (let [nxt (inc j)
                          next-token (nth token-data nxt nil)
                          next-tag (second (nth pos-tags nxt [nil ""]))]
                      (if (and next-token
                               (= next-tag "NNP")
                               (titlecase? (:token next-token))
                               (not (stopset (:token next-token))))
                        (recur nxt (conj labels (:token next-token))
                               {:start (:start span) :end (:end next-token)})
                        {:next (inc j)
                         :labels labels
                         :span span})))]
              (recur next
                     (conj acc {:type :unknown
                                :label (str/join " " labels)
                                :span span
                                :layer :fallback
                                :source :fallback})))
            (recur (inc idx) acc)))))))

;; ----------------------------------------------------------------------------
;; Scoring and resolution

(defn- score-entity [entity token-data]
  (let [base (get base-confidence (:layer entity) 0.0)
        label (:label entity)
        matched (:matched entity label)
        same-label? (= (str/lower-case matched) (str/lower-case label))
        context (context-window token-data (:span entity))
        type (:type entity)
        layer (:layer entity)
        keywords (get context-keywords type #{})
        bonus (+ (if same-label? alias-bonus 0)
                 (if (and (str/includes? label " ")
                          (contains? #{:pos :fallback} layer))
                   multi-token-bonus
                   0)
                 (if (and (seq keywords)
                          (some #(contains? keywords %) context))
                   context-bonus
                   0))
        penalty (if (and (= 0 (:start (:span entity)))
                         (not (str/includes? label " ")))
                  sentence-initial-penalty
                  0)]
    (-> (+ base bonus) (- penalty) (max 0.0) (min 1.0))))

(defn- resolve-overlaps [candidates]
  (let [ordered (sort-by (fn [c]
                           [(- (get base-confidence (:layer c) 0.0))
                            (- (span-length (:span c)))
                            (if (= (:layer c) :fallback) 1 0)])
                         candidates)]
    (reduce (fn [acc cand]
              (if (some #(overlap? (:span %) (:span cand)) acc)
                acc
                (conj acc cand)))
            []
            ordered)))

(defn- finalise-entities [cands token-data]
  (->> cands
       (map (fn [c]
              (let [confidence (score-entity c token-data)
                    adjusted (if (and (< confidence 0.45) (>= confidence 0.35))
                               (assoc c :type :unknown)
                               c)]
                (-> adjusted
                    (assoc :confidence confidence)
                    (dissoc :matched)))))
       (filter #(>= (:confidence %) 0.35))
       (map (fn [c]
              (let [label (:label c)
                    type (:type c)
                    id (if type
                         (format "E:%s:%s" (name type) (slugify label))
                         (format "E:unknown:%s" (slugify label)))]
                (-> c
                    (assoc :id id)
                    (update :aliases #(vec (distinct (remove str/blank? %))))))))
       (filter (fn [c]
                 (if (= (:layer c) :fallback)
                   (>= (:confidence c) 0.35)
                   (>= (:confidence c) 0.45))))
       (map #(dissoc % :layer))
       (sort-by (comp :start :span))
       vec))

;; ----------------------------------------------------------------------------
;; Public API

(defn recognize-entities
  "Recognise entities using deterministic tiered NER.
   opts may include {:enable-fallback? boolean :catalog {...}}."
  [tokens pos-tags text now & [{:keys [enable-fallback? catalog]
                                :or {enable-fallback? false}}]]
  (let [res-base (resources)
        catalog' (normalise-catalog catalog)
        res (if (seq catalog')
              (update res-base :gazetteer merge-gazetteers catalog')
              res-base)
        token-data (token-spans text tokens)
        candidates (concat (merge-gazetteer text token-data res)
                           (pattern-candidates text now res)
                           (pos-titlecase-candidates token-data pos-tags res)
                           (domain-candidates text res)
                           (when enable-fallback?
                             (fallback-candidates token-data pos-tags res)))
        resolved (resolve-overlaps candidates)]
    (finalise-entities resolved token-data)))
