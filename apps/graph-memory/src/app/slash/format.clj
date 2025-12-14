(ns app.slash.format
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import (java.time Instant)))

(def help-lines
  ["Slash commands:"
   "  /tail [n]      Show the last n relations (default 5)"
   "  /trails [n]    Show the last n Tatami trail entries (default 10)"
   "  /ego NAME      Display neighbors connected to the named entity"
   "  /cooccur NAME  Show entities that co-occur with the named entity"
   "  /forget NAME   Remove the named entity and attached relations"
   "  /expire NAME   Reset salience counters for the named entity"
   "  /entity NAME [TYPE]   Ensure an entity exists (optionally set type)"
   "  /relation TYPE SRC DST   Upsert a relation between SRC and DST"
   "  /me [summary] [limit]  Render the profile summary (default 2000 chars)"
   "  /me doc          Show the raw profile document"
   "  /types         List registered entity/relation/intent types"
   "  /types parent TYPE [PARENT] [KIND]  Override a type's parent"
   "  /types merge INTO ALIAS...  Merge aliases into a canonical type"
   "  /help          Show this help message"])

(defn type-label [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    (nil? t) nil
    :else (str t)))

(defn name-with-type [name type]
  (if (and name type)
    (str name " (" (type-label type) ")")
    name))

(defn pprint-lines [value]
  (let [rendered (with-out-str (pprint/pprint value))]
    (->> (str/split rendered #"\n")
         (map str/trimr)
         (remove str/blank?))))

(defn profile-lines [{:keys [profile data]}]
  (into [(str "Profile: " profile)] (pprint-lines data)))

(defn profile-summary-lines [{:keys [profile text generated-at]}]
  [(str "Profile: " profile)
   (str "Generated at: " generated-at)
   ""
   text])

(defn entity-lines [{:keys [name type seen-count last-seen pinned?]}]
  (let [header (str "Entity ensured: " (name-with-type name type))
        seen-line (when (some? seen-count)
                    (str "  - seen-count → " seen-count))
        last-line (when (some? last-seen)
                    (str "  - last-seen → " last-seen))
        pin-line (when (some? pinned?)
                   (str "  - pinned? → " pinned?))]
    (->> [header seen-line last-line pin-line]
         (remove nil?))))

(defn relation-lines [{:keys [type src dst confidence last-seen]}]
  (let [src-name (name-with-type (get-in src [:name]) (get-in src [:type]))
        dst-name (name-with-type (get-in dst [:name]) (get-in dst [:type]))
        base (str "Relation ensured: " src-name " —" (type-label type) "→ " dst-name)
        conf-line (when (some? confidence)
                    (format "  - confidence → %.2f" (double confidence)))
        last-line (when (some? last-seen)
                    (str "  - last-seen → " last-seen))]
    (->> [base conf-line last-line]
         (remove nil?))))

(defn types-lines [{:keys [types]}]
  (if (seq types)
    (into ["Registered types:"]
          (for [{:keys [id kind parent aliases alias_of inferred_parent]} types]
            (str "  - " id
                 " [" kind "]"
                 (when parent (str " parent=" parent))
                 (when alias_of (str " alias-of=" alias_of))
                 (when (seq aliases) (str " aliases=" (pr-str aliases)))
                 (when (some? inferred_parent) (str " inferred-parent=" inferred_parent)))))
    ["Registered types:" "  (none)"]))

(defn format-timestamp [ts]
  (when (number? ts)
    (try
      (.toString (Instant/ofEpochMilli (long ts)))
      (catch Exception _ nil))))

(defn relation-label [t]
  (or (type-label t) "?"))

(defn- describe-hx-end [{:keys [role ident type] :as end}]
  (let [label (or (:name end) ident "?")
        role-label (name (or role :participant))
        type-label (when type (str ":" (name type)))]
    (str role-label "=" label type-label)))

(defn- hx-other-ends-extras [ends]
  (let [others (->> ends (remove :primary?) (map describe-hx-end) (remove str/blank?))]
    (when (seq others)
      (str "others " (str/join ", " others)))))

(defn- hx-labels-extras [labels]
  (when (seq labels)
    (str "labels " (str/join ", " (map name labels)))))

(defn- hx-content-extras [content]
  (when (and content (not (and (string? content) (str/blank? content))))
    (let [snippet (if (string? content)
                    content
                    (pr-str content))]
      (str "content " snippet))))

(defn- hx-extra-details [row]
  (let [ends (:hx/ends row)
        labels (:hx/labels row)
        content (:hx/content row)]
    (->> [(hx-other-ends-extras ends)
          (hx-labels-extras labels)
          (hx-content-extras content)]
       (remove str/blank?)
       (str/join "; "))))

(defn format-tail-line [{:keys [src dst type confidence last-seen] :as row}]
  (let [src-name (or (:name src) "?")
        dst-name (or (:name dst) "?")
        relation (relation-label type)
        hx-extras (hx-extra-details row)
        extras (->> [(when confidence (format "conf %.2f" (double confidence)))
                     (when-let [ts (format-timestamp last-seen)] (str "seen " ts))
                     hx-extras]
                    (remove nil?)
                    (str/join ", "))]
    (str "  - " src-name " —" relation "→ " dst-name
         (when (seq extras)
           (str " (" extras ")")))))

(defn tail-lines [relations]
  (if (seq relations)
    (into ["Recent relations:"] (map format-tail-line relations))
    ["Recent relations:" "  (none)"]))

(defn format-trail-line [{:keys [session-id turn-id timestamp intent fruits paramitas futons prototypes]}]
  (let [ts (some-> timestamp format-timestamp)
        fruit-labels (->> fruits (map #(or (:fruit/id %) (:id %))) (remove nil?) (map name) (str/join ", "))
        orb-labels (->> paramitas (map #(or (:paramita/id %) (:id %))) (remove nil?) (map name) (str/join ", "))
        futon-label (when (seq futons) (str "futons " (str/join ", " (map name futons))))
        proto-label (when (seq prototypes) (str "prototypes " (str/join ", " (map name prototypes))))
        extras (->> [(when ts (str "at " ts))
                     (when (seq fruit-labels) (str "fruits " fruit-labels))
                     (when (seq orb-labels) (str "paramitas " orb-labels))
                     futon-label
                     proto-label]
                    (remove str/blank?)
                    (str/join "; "))]
    (str "  - " (or session-id "?") "/" (or turn-id "?")
         " — " (or intent "<no intent>")
         (when (seq extras)
           (str " (" extras ")")))))

(defn trails-lines [trails]
  (if (seq trails)
    (into ["Recent trail entries:"] (map format-trail-line trails))
    ["Recent trail entries:" "  (none)"]))

(defn- entity-label [entity]
  (or (:entity/name entity)
      (:name entity)
      (:entity/external-id entity)
      (:external-id entity)
      (:entity/id entity)
      (:id entity)
      "?"))

(defn- entity-type [entity]
  (or (:entity/type entity)
      (:type entity)))

(defn format-ego-line [{:keys [relation direction entity]} focal]
  (let [relation (relation-label relation)
        neighbor-name (name-with-type (entity-label entity) (entity-type entity))
        focus-name (name-with-type (:name focal) (:type focal))]
    (if (= direction :out)
      (str "  - " focus-name " —" relation "→ " neighbor-name)
      (str "  - " neighbor-name " —" relation "→ " focus-name))))

(defn ego-lines [{:keys [entity outgoing incoming]} requested-name]
  (if (not entity)
    [(str "entity not found: " requested-name)]
    (let [header (str "Entity: " (name-with-type (:name entity) (:type entity)))
          out-lines (if (seq outgoing)
                      (into ["Outgoing:"] (map #(format-ego-line % entity) outgoing))
                      ["Outgoing:" "  (none)"])
          in-lines (if (seq incoming)
                     (into ["Incoming:"] (map #(format-ego-line % entity) incoming))
                     ["Incoming:" "  (none)"])]
      (into [header] (concat out-lines in-lines)))))

(defn cooccur-lines [{:keys [entity rows]} requested-name]
  (if (not entity)
    [(str "entity not found: " requested-name)]
    (let [header (str "Co-occurrences with " (:name entity) ":")
          lines (if (seq rows)
                  (map (fn [{:keys [name type count]}]
                         (str "  - " (name-with-type name type) " (" count ")"))
                       rows)
                  ["  (none)"])]
      (into [header] lines))))

(defn format-removed-relation [{:keys [type src dst confidence]}]
  (let [relation (relation-label type)
        src-name (name-with-type (:name src) (:type src))
        dst-name (name-with-type (:name dst) (:type dst))]
    (str "  - " src-name " —" relation "→ " dst-name
         (when (number? confidence)
           (str " (conf " (format "%.2f" (double confidence)) ")")))))

(defn removed-relations-block [relations]
  (if (seq relations)
    (into ["Detached relations:"] (map format-removed-relation relations))
    ["Detached relations:" "  (none)"]))

(defn forget-lines [{:keys [entity relations]} requested-name]
  (if (not entity)
    [(str "entity not found: " requested-name)]
    (let [label (name-with-type (:name entity) (:type entity))]
      (into [(str "Removed " (or label requested-name))]
            (removed-relations-block relations)))))

(defn expire-lines [entity requested-name]
  (if (not entity)
    [(str "entity not found: " requested-name)]
    (let [label (name-with-type (:name entity) (:type entity))
          seen (or (:seen-count entity) 0)
          last (long (or (:last-seen entity) 0))
          last-line (if (pos? last)
                      (if-let [ts (format-timestamp last)]
                        (str "  - last-seen → " ts)
                        (str "  - last-seen → " last))
                      "  - last-seen reset")
          pin (:pinned? entity)
          pin-line (when (some? pin)
                     (str "  - pinned? → " pin))]
      (cond-> [(str "Expired salience for " (or label requested-name))
               (str "  - seen-count → " seen)
               last-line]
        pin-line (conj pin-line)))))
