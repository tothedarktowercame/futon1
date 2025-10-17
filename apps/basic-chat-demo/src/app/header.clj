(ns app.header
  (:require [app.focus :as focus]
            [cheshire.core :as json]
            [clojure.string :as str]
            [graph-memory.types-registry :as types]
            [xtdb.api :as xt]))

(def default-policy
  {:allow-types focus/default-allowed-types
   :k-per-anchor 2
   :context-cap-total 6
   :allow-works? false
   :focus-days 30
   :per-edge-caps {:member-of 2
                   :advisor-of 2
                   :affiliated-with 2
                   :performed-in 1
                   :created 0}})

(def ^:private millis-per-day (* 24 60 60 1000))
(def ^:private display-limits {:current 5 :history 5 :context 8})
(def ^:private fh-max-chars 2000)

(defn- round2 [x]
  (when (number? x)
    (/ (Math/round (* x 100.0)) 100.0)))

(defn- type->label [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    (nil? t) nil
    :else (str t)))

(defn- entity->display
  [entity]
  (when-let [label (or (:entity/name entity) (:name entity))]
    (let [etype (or (:entity/type entity) (:type entity))]
      (cond-> {:label label}
        etype (assoc :type (type->label etype))))))

(defn- current-entry [entity]
  (entity->display entity))

(defn- history-entry [{:keys [entity score pinned?]}]
  (when-let [display (entity->display entity)]
    (cond-> (assoc display :score (round2 score))
      pinned? (assoc :pinned true))))

(defn- raw-candidate-entry [{:keys [entity score anchor?]}]
  (let [{:entity/keys [id name type last-seen seen-count pinned?]} entity]
    (cond-> {:anchor (boolean anchor?)
             :score (round2 score)}
      id (assoc :id (str id))
      name (assoc :label name)
      type (assoc :type (type->label type))
      last-seen (assoc :last_seen last-seen)
      seen-count (assoc :seen_count seen-count)
      (some? pinned?) (assoc :pinned (boolean pinned?)))))

(defn- neighbor-display [focus-map entry]
  (let [focus-id (:focus-id entry)
        neighbor (:neighbor entry)
        focus-entity (get focus-map focus-id)
        neighbor-name (:entity/name neighbor)
        focus-name (:entity/name focus-entity)]
    (when (and focus-name neighbor-name)
      (cond-> {:focus focus-name
               :relation (some-> (:relation/type entry) type->label)
               :neighbor neighbor-name
               :direction (name (:direction entry))}
        (:entity/type neighbor) (assoc :neighbor_type (type->label (:entity/type neighbor)))
        (:score entry) (assoc :score (round2 (:score entry)))
        (:confidence entry) (assoc :confidence (round2 (:confidence entry)))))))

(defn- neighbor-debug [focus-map entry]
  (let [focus-id (:focus-id entry)
        neighbor (:neighbor entry)
        focus-entity (get focus-map focus-id)
        relation-type (:relation/type entry)
        confidence (:confidence entry)
        last-seen (:last-seen entry)
        provenance (:provenance entry)
        score (:score entry)
        direction (:direction entry)]
    (cond-> {:focus_id (some-> focus-id str)
             :focus_label (:entity/name focus-entity)
             :focus_type (some-> (:entity/type focus-entity) type->label)
             :relation (some-> relation-type type->label)
             :direction (name direction)
             :neighbor_label (:entity/name neighbor)
             :neighbor_type (some-> (:entity/type neighbor) type->label)
             :score (round2 score)}
      (:entity/id neighbor) (assoc :neighbor_id (str (:entity/id neighbor)))
      confidence (assoc :confidence (round2 confidence))
      last-seen (assoc :last_seen last-seen)
      provenance (assoc :provenance provenance))))

(defn- pop-last [v]
  (if (and (vector? v) (pos? (count v)))
    (subvec v 0 (dec (count v)))
    v))

(defn- shrink-step [header]
  (cond
    (seq (:context header)) [(update header :context pop-last) "trimmed-context"]
    (seq (:history header)) [(update header :history pop-last) "trimmed-history"]
    (> (count (:current header)) 1) [(update header :current pop-last) "trimmed-current"]
    :else [(assoc header :truncated true) nil]))

(defn- shrink-header [header]
  (loop [hdr header]
    (let [encoded (json/generate-string hdr)]
      (if (<= (count encoded) fh-max-chars)
        hdr
        (let [[next note] (shrink-step hdr)]
          (if (= next hdr)
            (assoc hdr :truncated true)
            (recur (cond-> next note (update :notes (fnil conj []) note)))))))))

(defn focus-header
  "Build a focus header map based on anchors, intent, and policy overrides."
  [xt-node {:keys [anchors intent time policy turn-id dimensions focus-limit debug?]}]
  (let [policy' (merge default-policy (or policy {}))
        now (or time (System/currentTimeMillis))
        xt-db (xtdb.api/db xt-node)
        focus-days (:focus-days policy')
        cutoff (- now (* focus-days millis-per-day))
        anchor-ids (->> anchors (map :id) (remove nil?) set)
        focus-count (or focus-limit (:context-cap-total policy'))
        allowed-config (:allow-types policy')
        allowed-pred (cond
                       (nil? allowed-config) nil
                       (ifn? allowed-config) allowed-config
                       :else (types/effective-pred :entity allowed-config))
        candidates (focus/focus-candidates xt-node anchor-ids cutoff focus-count {:allowed-types allowed-config})
        focus-map (into {} (map (fn [{:keys [id entity]}] [id entity]) candidates))
        per-edge (or (:per-edge-caps policy')
                     (:per-type-caps policy'))
        neighbor-entries (->> candidates
                              (mapcat (fn [{:keys [id]}]
                                        (focus/top-neighbors nil xt-db id {:k-per-anchor (:k-per-anchor policy')
                                                                           :per-edge-caps per-edge
                                                                           :allowed-types allowed-config
                                                                           :allow-works? (:allow-works? policy')
                                                                           :time-hint cutoff})))
                              (filter (fn [{:keys [neighbor]}]
                                        (let [etype (:entity/type neighbor)]
                                          (or (nil? allowed-pred)
                                              (nil? etype)
                                              (allowed-pred etype)))))
                              (sort-by (comp - :score))
                              (take (:context display-limits))
                              vec)
        current (->> anchors
                     (map current-entry)
                     (remove nil?)
                     (distinct)
                     (take (:current display-limits))
                     vec)
        history (->> candidates
                     (remove :anchor?)
                     (map history-entry)
                     (remove nil?)
                     (take (:history display-limits))
                     vec)
        header {:fh_v 2
                :turn_id turn-id
                :time now
                :intent (some-> intent (select-keys [:type :conf]))
                :current current
                :history history
                :context (->> neighbor-entries
                              (map #(neighbor-display focus-map %))
                              (remove nil?)
                              vec)
                :dimensions (not-empty dimensions)}
        header (cond-> header
                 debug? (assoc :debug {:policy policy'
                                       :candidates (vec (map raw-candidate-entry candidates))
                                       :neighbors (vec (keep #(neighbor-debug focus-map %) neighbor-entries))}))]
    header))

(defn focus-header-json
  "Serialize the provided focus header to JSON, trimming oversized sections."
  [fh]
  (when fh
    (json/generate-string (shrink-header fh))))

(defn- label-with-type [label type]
  (if (and type (not (str/blank? type)))
    (str label " (" type ")")
    label))

(defn- history-line [{:keys [label type score pinned]}]
  (let [base (label-with-type label type)
        extras (->> [(when score (format "score %.2f" (double score)))
                     (when pinned "pinned")]
                    (remove nil?)
                    (str/join ", "))]
    (str "  - " base (when (seq extras)
                        (str " — " extras)))))

(defn- current-line [{:keys [label type]}]
  (str "  - " (label-with-type label type)))

(defn- context-line [{:keys [focus relation neighbor neighbor_type direction score confidence]}]
  (let [relation (or relation "?")
        neighbor-label (label-with-type neighbor neighbor_type)
        [subject object] (if (= direction "in")
                           [neighbor-label (or focus "?")]
                           [(or focus "?") neighbor-label])
        base (str "  - " subject " —" relation "→ " object)
        extras (->> [(when score (format "score %.2f" (double score)))
                     (when confidence (format "conf %.2f" (double confidence)))]
                    (remove nil?)
                    (str/join ", "))]
    (str base (when (seq extras)
                (str " (" extras ")")))))

(defn- section-lines [title entries formatter]
  (let [entries (vec (remove nil? entries))]
    (if (seq entries)
      (into [(str title ":")] (map formatter entries))
      [(str title ":") "  (none)"])))

(defn focus-header-lines
  "Render the focus header as annotated plain-text sections."
  [fh]
  (when fh
    (let [hdr (shrink-header fh)
          current (:current hdr)
          history (:history hdr)
          context (:context hdr)
          lines (concat ["Focus header"]
                        (section-lines "Current focus" current current-line)
                        (section-lines "History" history history-line)
                        (section-lines "Enriched context" context context-line))]
      (vec lines))))
