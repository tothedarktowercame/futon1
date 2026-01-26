(ns futon1.store.compat.alpha
  "Incremental ArxanaStore adapter that currently supports article get/put/delete via
   the legacy alpha Datascript store, with placeholders for remaining ops."
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [futon1.store.core :as core]
            [futon1.store.datascript :as hx-ds])
  (:import (java.util UUID)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- normalize-ident [ident]
  (cond
    (nil? ident) nil
    (string? ident) (let [trimmed (str/trim ident)]
                      (when (seq trimmed) trimmed))
    (keyword? ident) (name ident)
    (map? ident) (or (:article/ident ident)
                    (:ident ident)
                    (:name ident)
                    (:article/name ident))
    :else (str ident)))

(defn- entity->article [{:keys [id name type seen-count last-seen pinned?]}]
  (when (or id name)
    {:article/ident (or name (some-> id str))
     :article/name name
     :article/type (cond-> [] type (conj type))
     :article/bookkeeping (cond-> {}
                             (some? seen-count) (assoc :seen-count seen-count)
                             (some? last-seen) (assoc :last-seen last-seen)
                             (some? pinned?) (assoc :pinned? pinned?))}))

(defn- store-opts [env]
  (assoc env :now (System/currentTimeMillis)))

(def default-capabilities
  {:articles {:crud? true}
   :metadata {:get? true :update? true}
   :events   {:multi-end? true :query? true :delete? true}
   :links    {:add? true :list? true}
   :plexus   {:ensure? true :assign? true :list? true}
   :slash    {:tail? true :ego? true :cooccur? true}})

(defn capabilities
  "Return a capability map describing the compat store surface area."
  [_env]
  default-capabilities)

(defn- data-dir [env]
  (or (:data-dir env)
      (throw (ex-info "data-dir required" {:env env}))))

(def ^:private entity-pull-pattern
  [:entity/id :entity/name :entity/type :entity/last-seen :entity/seen-count :entity/pinned?])

(def ^:private relation-pull-pattern
  [:relation/id :relation/type :relation/last-seen :relation/confidence
   {:relation/src entity-pull-pattern}
   {:relation/dst entity-pull-pattern}])

(defn- legacy-entity->article [entity]
  (when entity
    (entity->article {:id (:entity/id entity)
                      :name (:entity/name entity)
                      :type (:entity/type entity)
                      :seen-count (:entity/seen-count entity)
                      :last-seen (:entity/last-seen entity)
                      :pinned? (:entity/pinned? entity)})))

(defn- all-legacy-articles [conn]
  (if conn
    (->> (d/q '[:find (pull ?e ?pattern)
                :in $ ?pattern
                :where [?e :entity/id ?ident]]
              @conn entity-pull-pattern)
         (map first)
         (map legacy-entity->article)
         (remove nil?))
    []))

(defn- relation->hx [relation]
  (let [{:keys [id type src dst confidence last-seen provenance]} relation
        src-article (entity->article src)
        dst-article (entity->article dst)]
    (when (and id src-article dst-article)
      (cond-> {:hx/id id
               :hx/type type
               :hx/ends [{:role :source :article src-article}
                         {:role :target :article dst-article}]}
        confidence (assoc :hx/confidence confidence)
        last-seen (assoc :hx/last-seen last-seen)
        provenance (assoc :hx/provenance provenance)))))

(defn- legacy-end->entity [legacy]
  (when legacy
    {:id (:entity/id legacy)
     :name (:entity/name legacy)
     :type (:entity/type legacy)
     :seen-count (:entity/seen-count legacy)
     :last-seen (:entity/last-seen legacy)
     :pinned? (:entity/pinned? legacy)}))

(defn- legacy-relation->hx [relation]
  (relation->hx {:id (:relation/id relation)
                 :type (:relation/type relation)
                 :confidence (:relation/confidence relation)
                 :last-seen (:relation/last-seen relation)
                 :provenance (:relation/provenance relation)
                 :src (legacy-end->entity (:relation/src relation))
                 :dst (legacy-end->entity (:relation/dst relation))}))

(defn- ->article-ident [m]
  (when (some? m)
    (or (:article/ident m)
        (:article/name m)
        (:ident m)
        (:name m))))

(defn- hx-end-ident [end]
  (let [article (:article end)
        direct (or (->article-ident article)
                   (->article-ident end))
        id (:id end)]
    (or direct
        (when (vector? id)
          (let [[scope value] id]
            (when (= scope :article)
              value)))
        (when (map? id)
          (->article-ident id))
        (when (string? id)
          (let [trimmed (str/trim id)]
            (when (seq trimmed) trimmed))))))

(defn- hx-end->entity-spec [end]
  (when-let [ident (hx-end-ident end)]
    (let [article (:article end)
          article-type (:article/type article)
          type (cond
                 (keyword? article-type) article-type
                 (sequential? article-type) (first article-type)
                 :else nil)]
      (cond-> {:name ident}
        type (assoc :type type)))))

(defn- normalize-hx-type [t]
  (cond
    (keyword? t) t
    (string? t) (-> t str/lower-case keyword)
    :else nil))

(defn- hx->relation-spec [{:hx/keys [type ends id confidence last-seen provenance] :as hx}]
  (let [rel-type (normalize-hx-type type)
        src-end (or (some #(when (= :source (:role %)) %) ends)
                    (first ends))
        dst-end (or (some #(when (= :target (:role %)) %) ends)
                    (second ends))
        src-spec (hx-end->entity-spec src-end)
        dst-spec (hx-end->entity-spec dst-end)]
    (when-not rel-type
      (throw (ex-info "hx/type required" {:hx hx})))
    (when (or (nil? src-spec) (nil? dst-spec))
      (throw (ex-info "hx events must include :source and :target ends" {:hx hx})))
    (cond-> {:type rel-type
             :src src-spec
             :dst dst-spec}
      id (assoc :id id)
      confidence (assoc :confidence confidence)
      last-seen (assoc :last-seen last-seen)
      provenance (assoc :provenance provenance))))

(defn- ensure-relation-endpoints!
  [conn opts relation-spec]
  (let [src-spec (:src relation-spec)
        dst-spec (:dst relation-spec)
        src-entity (store/ensure-entity! conn opts src-spec)
        dst-entity (store/ensure-entity! conn opts dst-spec)]
    (-> relation-spec
        (assoc :src (select-keys src-entity [:id :name :type]))
        (assoc :dst (select-keys dst-entity [:id :name :type])))))

(defn- ensure-hx-id [hx]
  (if (:hx/id hx)
    hx
    (assoc hx :hx/id (UUID/randomUUID))))

(defn- now-ms []
  (System/currentTimeMillis))

(defn- stamp-hx [hx]
  (let [ts (now-ms)]
    (cond-> hx
      (nil? (:hx/last-seen hx)) (assoc :hx/last-seen ts)
      (nil? (:hx/recorded-at hx)) (assoc :hx/recorded-at ts))))

(defn- normalize-hx-id [value]
  (cond
    (instance? UUID value) value
    (string? value) (let [trimmed (str/trim value)]
                     (when (seq trimmed)
                       (try
                         (UUID/fromString trimmed)
                         (catch Exception _ nil))))
    (map? value) (or (normalize-hx-id (:hx/id value))
                     (normalize-hx-id (:id value)))
    :else nil))

(defn- hx-recency [hx]
  (long (or (:hx/last-seen hx) 0)))

(defn- entity-id-for-ident [conn ident]
  (some-> (store/resolve-name->eid conn ident)
          :id))

(defn- link-payload->hx [{:keys [source-ident target-ident types passage labels]}]
  (let [source (normalize-ident source-ident)
        target (normalize-ident target-ident)
        type (or (some-> types first normalize-hx-type) :link/refers-to)]
    (when (or (nil? source) (nil? target))
      (throw (ex-info "link requires source/target idents"
                      {:source source-ident :target target-ident})))
    {:hx/type type
     :hx/labels (when (seq labels) (vec labels))
     :hx/content (when passage {:passage passage})
     :hx/ends [{:role :source
                :article {:article/ident source}}
               {:role :target
                :article {:article/ident target}}]}))

(defn- hx-end-ident-safe [end]
  (or (get-in end [:article :article/ident])
      (hx-end-ident end)))

(defn- hx->relation-compatible? [hx]
  (let [ends (:hx/ends hx)
        source (some #(when (= :source (:role %)) %) ends)
        target (some #(when (= :target (:role %)) %) ends)]
    (and source target)))

(defn- compat-relation-spec [hx]
  (when (hx->relation-compatible? hx)
    (hx->relation-spec hx)))

(def ^:private outgoing-relations-q
  '[:find (pull ?r ?pattern)
    :in $ ?pattern ?eid
    :where
    [?src :entity/id ?eid]
    [?r :relation/src ?src]])

(def ^:private incoming-relations-q
  '[:find (pull ?r ?pattern)
    :in $ ?pattern ?eid
    :where
    [?dst :entity/id ?eid]
    [?r :relation/dst ?dst]])

(defn- legacy-links-for-ident [conn ident direction]
  (if-let [entity-id (and conn (entity-id-for-ident conn ident))]
    (let [query (case direction
                  :outgoing outgoing-relations-q
                  :incoming incoming-relations-q)
          raw (d/q query @conn relation-pull-pattern entity-id)]
      (->> raw
           (map first)
           (map legacy-relation->hx)
           (remove nil?)
           (sort-by hx-recency >)
           vec))
    []))

(defn- all-legacy-hx [conn]
  (if conn
    (->> (d/q '[:find (pull ?r ?pattern)
                :in $ ?pattern
                :where [?r :relation/id ?rid]]
              @conn relation-pull-pattern)
         (map first)
         (map legacy-relation->hx)
         (remove nil?))
    []))

(defn- plexus-root [env]
  (let [dir (io/file (data-dir env) "plexus")]
    (.mkdirs dir)
    dir))

(defn- plexus-dir [env plexus-id]
  (let [root (plexus-root env)
        dir (io/file root (str plexus-id))]
    (.mkdirs dir)
    dir))

(defn- plexus-meta-file [env plexus-id]
  (io/file (plexus-dir env plexus-id) "meta.edn"))

(defn- plexus-members-file [env plexus-id]
  (io/file (plexus-dir env plexus-id) "members.edn"))

(defn- read-edn-file [file default]
  (if (.exists file)
    (try
      (edn/read-string (slurp file))
      (catch Exception _ default))
    default))

(defn- write-edn-file! [file value]
  (.mkdirs (.getParentFile file))
  (spit file (pr-str value)))

(defn- normalize-plexus-id [plexus-id]
  (let [normalized (normalize-ident plexus-id)]
    (or normalized
        (throw (ex-info "plexus id required" {:value plexus-id})))))

(defn- read-members [env plexus-id]
  (set (read-edn-file (plexus-members-file env plexus-id) [])))

(defn- write-members! [env plexus-id members]
  (write-edn-file! (plexus-members-file env plexus-id) (vec members)))

(defn- ensure-plexus-meta! [env plexus-id attrs]
  (let [meta-file (plexus-meta-file env plexus-id)
        existing (read-edn-file meta-file {})
        name (or (:plexus/name attrs) (:name attrs) (:plexus/name existing) (:name existing) (str plexus-id))
        config (or (:plexus/config attrs) (:config attrs) (:plexus/config existing) (:config existing))
        doc (cond-> {:plexus/id plexus-id
                     :plexus/name name}
              config (assoc :plexus/config config))]
    (write-edn-file! meta-file doc)
    doc))

(defn- member->article [conn ident]
  (let [entity (some-> (store/resolve-name->eid conn ident) entity->article)]
    (or entity {:article/ident ident})))

(defn- ensure-temp-root! [cache prefix]
  (or @cache
      (let [path (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
        (.deleteOnExit path)
        (reset! cache (.getAbsolutePath path)))))

(defn- metadata-dir [env temp-root]
  (let [base (or (:metadata-root env)
                 (:data-dir env)
                 (ensure-temp-root! temp-root "alpha-metadata-"))
        dir (io/file base "metadata")]
    (.mkdirs dir)
    dir))

(defn- metadata-file [env temp-root ident]
  (let [dir (metadata-dir env temp-root)
        safe-ident (-> ident str (str/replace #"/" "__"))]
    (io/file dir (str safe-ident ".edn"))))

(defn- hx-dir [env temp-root]
  (let [base (or (:data-dir env)
                 (ensure-temp-root! temp-root "alpha-hx-"))
        dir (io/file base "hx")]
    (.mkdirs dir)
    dir))

(defn- hx-store-file [env temp-root]
  (io/file (hx-dir env temp-root) "store.edn"))

(defn- load-hx-conn [file]
  (let [conn (hx-ds/create-conn)]
    (when (.exists file)
      (try
        (let [db (edn/read-string (slurp file))]
          (d/reset-conn! conn db))
        (catch Exception _
          (spit file ""))))
    conn))

(defn- persist-hx-db! [conn file]
  (.mkdirs (.getParentFile file))
  (spit file (pr-str @conn)))

(defn- stored-hx [hx-conn hx-id]
  (some-> (d/pull @hx-conn '[*] [:hx/id hx-id])
          (dissoc :db/id)))

(defn- all-stored-hx [hx-conn]
  (->> (d/q '[:find (pull ?e [*])
              :where [?e :hx/id _]]
            @hx-conn)
       (map first)
       (map #(dissoc % :db/id))))

(defn- all-hx [conn hx-conn hx-temp-root]
  (let [stored (all-stored-hx hx-conn)
        legacy (all-legacy-hx conn)
        merged (reduce (fn [acc hx]
                         (if-let [id (:hx/id hx)]
                           (assoc acc id hx)
                           (update acc ::no-id (fnil conj []) hx)))
                       {}
                       (concat legacy stored))
        with-id (-> merged (dissoc ::no-id) vals)
        without-id (::no-id merged)]
    (concat with-id without-id)))

(defn alpha-store [{:keys [conn env]}]
  (let [unsupported #(throw (ex-info "operation not yet supported" {:op %}))
        metadata-temp-root (atom nil)
        hx-temp-root (atom nil)
        hx-file (hx-store-file env hx-temp-root)
        hx-conn (load-hx-conn hx-file)]
    (reify core/ArxanaStore
      (get-article [_ ident]
        (let [lookup (normalize-ident ident)]
          (when (seq lookup)
            (when-let [entity (store/resolve-name->eid conn lookup)]
              (entity->article entity)))))

      (put-article! [_ {:article/keys [ident name type bookkeeping]}]
        (let [resolved-ident (or (normalize-ident ident)
                                 (normalize-ident name))
              first-type (cond
                           (vector? type) (first type)
                           (seqable? type) (first type)
                           :else type)
              {:keys [seen-count last-seen pinned?]} bookkeeping
              spec (cond-> {:name resolved-ident}
                     first-type (assoc :type first-type)
                     (some? seen-count) (assoc :seen-count seen-count)
                     (some? last-seen) (assoc :last-seen last-seen)
                     (some? pinned?) (assoc :pinned? pinned?))
              stored (store/ensure-entity! conn (store-opts env) spec)]
          (entity->article stored)))

      (delete-article! [_ ident]
        (let [lookup (normalize-ident ident)]
          (when (seq lookup)
            (store/forget-entity! conn (store-opts env) {:name lookup}))))

      (get-meta [_ ident]
        (let [lookup (normalize-ident ident)]
          (if (seq lookup)
            (let [file (metadata-file env metadata-temp-root lookup)
                  data (when (.exists file)
                         (read-edn-file file nil))]
              (or data {:note "metadata support pending"}))
            {:note "metadata support pending"})))

      (update-meta! [_ ident f]
        (let [lookup (normalize-ident ident)]
          (when-not (seq lookup)
            (throw (ex-info "metadata ident required" {:ident ident})))
          (let [file (metadata-file env metadata-temp-root lookup)
                existing (read-edn-file file {})
                next-meta (try
                            (f existing)
                            (catch Throwable ex
                              (throw (ex-info "metadata updater failed"
                                              {:ident lookup}
                                              ex))))
                final-meta (or next-meta {})]
            (write-edn-file! file final-meta)
            final-meta)))

      (add-event! [_ hx-map]
        (let [hx (-> hx-map stamp-hx ensure-hx-id)
              opts (store-opts env)
              compat-spec (some-> (compat-relation-spec (assoc hx :id (:hx/id hx)))
                                  (ensure-relation-endpoints! conn opts))
              relation (when compat-spec
                         (store/upsert-relation! conn opts compat-spec))
              stored (cond-> hx
                        relation (assoc :compat/relation-id (:id relation)))]
          (d/transact! hx-conn [(assoc stored :hx/id (:hx/id hx))])
          (persist-hx-db! hx-conn hx-file)
          (:hx/id stored)))

      (delete-event! [_ hx-ref]
        (let [relation-id (normalize-hx-id hx-ref)]
          (when-not relation-id
            (throw (ex-info "hx/id required" {:value hx-ref})))
          (when-let [stored (stored-hx hx-conn relation-id)]
            (when-let [rel (:compat/relation-id stored)]
              (store/delete-relation! conn (store-opts env) rel))
            (d/transact! hx-conn [[:db.fn/retractEntity [:hx/id relation-id]]])
            (persist-hx-db! hx-conn hx-file))
          (when (store/delete-relation! conn (store-opts env) relation-id)
            relation-id)))
      (events-by-end [_ ident]
        (let [lookup (normalize-ident ident)]
          (if (seq lookup)
            (->> (all-hx conn hx-conn hx-temp-root)
                 (filter (fn [hx]
                           (some #(= lookup (hx-end-ident-safe %)) (:hx/ends hx))))
                 (sort-by hx-recency >)
                 vec)
            [])))

      (events-by-type [_ type-kw]
        (let [normalized (normalize-hx-type type-kw)]
          (if normalized
            (->> (all-hx conn hx-conn hx-temp-root)
                 (filter #(= normalized (:hx/type %)))
                 (sort-by hx-recency >)
                 vec)
            [])))
      (add-link! [this {:keys [types] :as payload}]
        (let [hx (link-payload->hx payload)
              opts (store-opts env)
              relation-spec (ensure-relation-endpoints! conn opts (hx->relation-spec hx))
              relation (store/upsert-relation! conn opts relation-spec)
              stored (relation->hx relation)]
          (or (:hx/id stored)
              (:id relation))))
      (ensure-plexus! [_ plexus-id attrs]
        (let [id (normalize-plexus-id plexus-id)]
          (ensure-plexus-meta! env id (or attrs {}))))

      (assign-to-plexus! [_ plexus-id entity-ident]
        (let [id (normalize-plexus-id plexus-id)
              member (normalize-ident entity-ident)]
          (when-not member
            (throw (ex-info "entity ident required" {:value entity-ident})))
          (ensure-plexus-meta! env id {})
          (let [members (conj (read-members env id) member)]
            (write-members! env id members)
            (->> members
                 (sort)
                 (map (partial member->article conn))
                 vec))))

      (members-in-plexus [_ plexus-id]
        (let [id (normalize-plexus-id plexus-id)
              members (read-members env id)]
          (->> members
               (sort)
               (map (partial member->article conn))
               vec)))

      (find-articles [_ pred]
        (let [matches? (or pred (constantly true))]
          (->> (all-legacy-articles conn)
               (filter matches?)
               (sort-by :article/ident)
               vec)))

      (links-from [_ ident]
        (let [lookup (normalize-ident ident)]
          (if (seq lookup)
            (legacy-links-for-ident conn lookup :outgoing)
            [])))

      (links-to [_ ident]
        (let [lookup (normalize-ident ident)]
          (if (seq lookup)
            (legacy-links-for-ident conn lookup :incoming)
            [])))

      (fulltext-dump [_]
        (let [articles (->> (all-legacy-articles conn)
                            (sort-by :article/ident)
                            vec)
              events (->> (all-hx conn hx-conn hx-temp-root)
                          (sort-by (juxt :hx/type :hx/id))
                          vec)]
          {:articles articles
           :events events})))))
