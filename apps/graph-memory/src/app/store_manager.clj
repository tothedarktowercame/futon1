(ns app.store-manager
  "Manage Datascript/XTDB resources and per-profile metadata for the headless API."
  (:require [app.config :as cfg]
            [app.charon-guard :as charon-guard]
            [app.focus :as focus]
            [app.invariants :as invariants]
            [app.model-penholder :as model-penholder]
            [app.store :as store]
            [app.type-counts :as type-counts]
            [app.xt :as xt]
            [charon.core :as charon]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon1.store.compat.alpha :as alpha-store]
            [xtdb.api :as xtdb])
  (:import (java.io File PushbackReader)))

(defn- delete-recursive! [^File f]
  (when f
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-recursive! child)))
    (io/delete-file f true)))

(declare profile-doc)

(def ^:private profile-filename "profile.edn")

(defonce ^:private !config (atom nil))
(defonce ^:private !profiles (atom {}))
(defonce ^:private !temp-data-roots (atom #{}))

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

(defn- apps-dir []
  (some-> (repo-root) (io/file "apps") .getAbsolutePath))

(defn- temp-data-root? [path]
  (when-let [p (some-> path str str/trim not-empty)]
    (let [abs-path (.getAbsolutePath (io/file p))
          name (.getName (io/file abs-path))
          apps-root (apps-dir)]
      (and (str/starts-with? name "tmp-client-data")
           (or (nil? apps-root)
               (str/starts-with? abs-path apps-root))))))

(defn- getenv-trim [k]
  (when-let [raw (System/getenv k)]
    (let [trimmed (str/trim raw)]
      (when (seq trimmed)
        trimmed))))

(defn- falsey-env? [raw]
  (let [value (some-> raw str/lower-case)]
    (contains? #{"false" "0" "off" "no"} value)))

(defn- xt-config []
  (let [resource (getenv-trim "BASIC_CHAT_XTDB_RESOURCE")
        enabled? (not (falsey-env? (System/getenv "BASIC_CHAT_XTDB_ENABLED")))
        sync-on-write? (not (falsey-env? (System/getenv "BASIC_CHAT_XTDB_SYNC_ON_WRITE")))
        base {:enabled? enabled?}
        from-resource (when (nil? resource)
                        (some-> (io/resource "xtdb.edn") io/file .getAbsolutePath))]
    (cond-> base
      resource (assoc :resource resource)
      (and (nil? resource) from-resource) (assoc :config-path from-resource)
      :always (assoc :sync-on-write? sync-on-write?))))

(defn- parse-long-env [value fallback]
  (if-let [raw (some-> value str/trim not-empty)]
    (try
      (Long/parseLong raw)
      (catch Exception _ fallback))
    fallback))

(defn- truthy-env? [value]
  (contains? #{"1" "true" "yes" "on"}
             (some-> value str/lower-case)))

(defn- xtdb-watchdog-config []
  {:interval-ms (parse-long-env (System/getenv "BASIC_CHAT_XTDB_WATCHDOG_MS") 30000)
   :stall-ms (parse-long-env (System/getenv "BASIC_CHAT_XTDB_STALL_MS") 120000)
   :auto-restart? (if (some? (System/getenv "BASIC_CHAT_XTDB_WATCHDOG_RESTART"))
                    (truthy-env? (System/getenv "BASIC_CHAT_XTDB_WATCHDOG_RESTART"))
                    true)})

(defn- xtdb-latest-completed-ms []
  (when (xt/started?)
    (when-let [tx (xtdb.api/latest-completed-tx (xt/node))]
      (some-> (:xtdb.api/tx-time tx) (.getTime)))))

(defn- start-xtdb-watchdog! []
  (when (nil? @!xtdb-watchdog)
    (let [{:keys [interval-ms stall-ms auto-restart?]} (xtdb-watchdog-config)]
      (reset! !xtdb-watchdog
              (future
                (loop []
                  (try
                    (let [last-ms (xtdb-latest-completed-ms)
                          now (System/currentTimeMillis)
                          stalled? (and last-ms (> (- now last-ms) stall-ms))]
                      (swap! !state assoc
                             :xtdb/watchdog {:last-completed-ms last-ms
                                             :stalled? (boolean stalled?)
                                             :last-check-ms now})
                      (when stalled?
                        (binding [*out* *err*]
                          (println (format "[xtdb-watchdog] stalled: last=%s now=%s"
                                           last-ms now)))
                        (when auto-restart?
                          (when-let [cfg (or (get-in (env (default-profile)) [:xtdb :config-path])
                                             (get-in (env (default-profile)) [:xtdb :resource]))]
                            (binding [*out* *err*]
                              (println "[xtdb-watchdog] restarting XTDB..."))
                            (xt/restart! cfg {:xt/created-by "xtdb-watchdog"})))))
                    (catch Exception e
                      (binding [*out* *err*]
                        (println (format "[xtdb-watchdog] error: %s" (.getMessage e))))))
                  (Thread/sleep interval-ms)
                  (recur))))))))

(defn- absolute-path [path]
  (some-> path io/file .getAbsolutePath))

(defn default-config []
  (let [data-root (or (getenv-trim "BASIC_CHAT_DATA_DIR")
                      (let [root (repo-root)]
                        (if root
                          (.getAbsolutePath (io/file root "data"))
                          (.getAbsolutePath (io/file "data")))))
        metadata-root-env (getenv-trim "BASIC_CHAT_METADATA_DIR")
        metadata-root (absolute-path metadata-root-env)
        snapshot (some-> (getenv-trim "ALPHA_SNAPSHOT_EVERY") Integer/parseInt)
        profile (or (getenv-trim "ALPHA_PROFILE") "default")
        penholder (or (getenv-trim "MODEL_PENHOLDER")
                      (getenv-trim "BASIC_CHAT_PENHOLDER"))]
    {:data-root (.getAbsolutePath (io/file data-root))
     :metadata-root metadata-root
     :snapshot-every (or snapshot 100)
     :xtdb (xt-config)
     :default-profile profile
     :model/penholder penholder}))

(defn configure!
  "Replace the active configuration and clear in-memory profile caches."
  ([]
   (configure! {}))
  ([opts]
   (let [cfg (merge (default-config) opts)
         raw-root (or (:data-root cfg) (:app/data-dir cfg))
         abs-root (absolute-path raw-root)
         raw-metadata (or (:metadata-root cfg) (:app/metadata-dir cfg))
         abs-metadata (absolute-path raw-metadata)
         cfg' (cond-> cfg
                abs-root (assoc :data-root abs-root)
                abs-metadata (assoc :metadata-root abs-metadata))]
     (reset! !config cfg')
     (reset! !profiles {})
     (when abs-root
       (System/setProperty "basic-chat.data-root" abs-root)
       (when (temp-data-root? abs-root)
         (swap! !temp-data-roots conj abs-root)))
     (when abs-metadata
       (System/setProperty "basic-chat.metadata-root" abs-metadata))
     cfg')))

(defn- log-type-counts!
  [state]
  (when-let [conn (:conn state)]
    (let [env (:env state)
          metadata-root (:metadata-root env)
          data-dir (:data-dir env)
          counts (type-counts/type-counts conn)
          durable (type-counts/durable-type-counts)]
      (println "[store] Type counts (volatile):" (type-counts/format-type-counts counts))
      (when durable
        (println "[store] Type counts (durable):" (type-counts/format-type-counts durable)))
      (when durable
        (let [divergence (type-counts/compare-counts-eq counts durable)]
          (when (and (not (:ok? divergence)) (seq (:failures divergence)))
            (println (str "[store] WARNING: Volatile vs durable counts diverged: "
                          (type-counts/format-counts-mismatch (:failures divergence)))))))
      (when (and durable metadata-root data-dir)
        (type-counts/save-baseline! metadata-root data-dir durable)))))

(defn shutdown!
  "Stop XTDB and clear cached profile state."
  []
  (doseq [[_ state] @!profiles]
    (try
      (log-type-counts! state)
      (catch Exception _)))
  (reset! !profiles {})
  (System/clearProperty "basic-chat.data-root")
  (xt/stop!)
  (let [paths @!temp-data-roots]
    (reset! !temp-data-roots #{})
    (doseq [path paths]
      (when (temp-data-root? path)
        (try
          (delete-recursive! (io/file path))
          (catch Exception _))))))

(defn config []
  (or @!config (configure! {})))

(defn default-profile []
  (:default-profile (config)))

(defn- profile-dir [profile]
  (let [{:keys [data-root]} (config)
        profile-id (if (= :me profile) (default-profile) profile)]
    (.getAbsolutePath (io/file data-root profile-id))))

(defn- ensure-dir! [^String path]
  (let [file (io/file path)]
    (.mkdirs file)
    (.getAbsolutePath file)))

(defn- read-profile [dir]
  (let [file (io/file dir profile-filename)]
    (if (.exists file)
      (try
        (with-open [r (io/reader file)]
          (binding [*read-eval* false]
            (edn/read (PushbackReader. r))))
        (catch Exception _
          {}))
      {})))

(defn- write-profile! [dir doc]
  (let [file (io/file dir profile-filename)]
    (.mkdirs (.getParentFile file))
    (spit file (pr-str doc))))

(defn profile-name
  "Return a human-friendly name for the profile."
  [profile]
  (let [doc (profile-doc profile)]
    (or (:preferred-name doc)
        (:name doc)
        (:handle doc)
        (:nickname doc)
        (:display-name doc)
        (when-let [aliases (:aliases doc)]
          (some identity aliases))
        (some-> profile str str/trim not-empty)
        "Me")))

(defn profile-interlocutor-name
  "Return a name to use for the default interlocutor (\"you\")."
  [profile]
  (let [doc (profile-doc profile)]
    (or (:interlocutor-name doc)
        (when-let [aliases (:interlocutor-aliases doc)]
          (some identity aliases))
        "You")))

(defn profile-collective-name
  "Return a name representing the default collective (\"we\")."
  [profile]
  (let [doc (profile-doc profile)
        me (profile-name profile)
        you (profile-interlocutor-name profile)]
    (or (:collective-name doc)
        (when-let [aliases (:collective-aliases doc)]
          (some identity aliases))
        (str me " & " you))))

(defn- ->canonical-state
  "Ensure required keys are present; attach optional XT handles if available."
  [st]
  (let [st (or st {})
        ;; If your store exposes these, wire them in. Otherwise they'll stay nil.
        xt-node (when (resolve 'store/xt-node) ((resolve 'store/xt-node)))
        xt-db   (when (resolve 'store/xt-db)   ((resolve 'store/xt-db)))]
    (merge
     {:profile     (:profile st)
      :profile-dir (:profile-dir st)             ;; java.io.File
      :env         (:env st)                     ;; {:data-dir <File> :snapshot-every N :xtdb {...}}
      :conn        (:conn st)                    ;; Datascript conn
      :me          (:me st)
      :last-anchors (:last-anchors st)
      ;; optional runtime handles (safe if missing)
      :xt-node     (or (:xt-node st) xt-node)
      :xt/db       (or (:xt/db st)   xt-db)}
     st)))

(defn- create-profile-state [profile]
  (let [{:keys [snapshot-every xtdb metadata-root]} (config)
        penholder-raw (:model/penholder (config))
        profile-id (if (= :me profile) (default-profile) profile)
        dir   (ensure-dir! (profile-dir profile))        ;; -> java.io.File
        metadata-base (or metadata-root dir)
        profile-metadata-root (-> (io/file metadata-base profile-id)
                                  str
                                  ensure-dir!)
        me-doc (read-profile dir)
        penholder (some-> penholder-raw str str/trim not-empty)
        env   {:data-dir       dir
               :metadata-root  profile-metadata-root
               :snapshot-every snapshot-every
               :xtdb           xtdb}
        conn  (store/restore! env me-doc)
        env-base (cond-> env
                   penholder (assoc :penholder penholder))
        _ (invariants/ensure-descriptors! conn env-base)
        _ (model-penholder/ensure-registry! conn env-base)
        _ (when (invariants/verify-on-write?)
            (charon/set-guardian! charon-guard/guardian))
        env'  (cond-> env-base
                (invariants/verify-on-write?)
                (assoc :verify-fn charon-guard/guard-event))
        _ (println "[store] Checking model invariants...")
        inv-result (invariants/verify-core conn {:metadata-root profile-metadata-root
                                                 :data-dir dir
                                                 :type-counts/check? true})
        _ (if (:ok? inv-result)
            (println "[store] Invariants OK")
            (println (str "[store] WARNING: Invariants failed for: "
                          (->> (:results inv-result)
                               (filter #(not (:ok? (second %))))
                               (map first)
                               (clojure.string/join ", ")))))
        _ (when-not (:ok? inv-result)
            (let [lines (->> (:results inv-result)
                             (mapcat (fn [[model result]]
                                       (cond
                                         (:ok? result) []
                                         (:error result) [(format "%s error=%s" model (:error result))]
                                         :else
                                         (let [inv-results (:results result)]
                                           (if (sequential? inv-results)
                                             (for [inv inv-results
                                                   :when (not (:ok? inv))]
                                               (let [failures (:failures inv)
                                                     count (count failures)
                                                     samples (take 3 failures)]
                                                 (format "%s %s failures=%d samples=%s"
                                                         model (:key inv) count (pr-str (vec samples)))))
                                             [(format "%s failures=%s" model (pr-str result))])))))
                             vec)]
              (when (seq lines)
                (println "[store] Invariant failure details:")
                (doseq [line lines]
                  (println (str "[store]  - " line))))))
        counts (type-counts/type-counts conn)
        _ (println "[store] Type counts (volatile):" (type-counts/format-type-counts counts))
        durable (type-counts/durable-type-counts)
        _ (when durable
            (println "[store] Type counts (durable):" (type-counts/format-type-counts durable)))
        baseline (type-counts/load-baseline profile-metadata-root)
        comparison (when durable
                     (type-counts/compare-type-counts baseline durable {:data-dir dir}))
        _ (when (and (not (:ok? comparison)) (seq (:failures comparison)))
            (println (str "[store] WARNING: Type counts decreased since last baseline: "
                          (type-counts/format-type-count-failures (:failures comparison)))))
        divergence (type-counts/compare-counts-eq counts durable)
        _ (when (and (not (:ok? divergence)) (seq (:failures divergence)))
            (println (str "[store] WARNING: Volatile vs durable counts diverged: "
                          (type-counts/format-counts-mismatch (:failures divergence)))))
        _ (when (and durable (or (:ok? comparison) (:skipped? comparison)))
            (type-counts/save-baseline! profile-metadata-root dir durable))
        node  (when (xt/started?) (xt/node))
        arxana-store (alpha-store/alpha-store {:conn conn :env env'})
        state {:profile      profile
               :profile-dir  dir
               :env          env'
               :conn         conn
               :arxana-store arxana-store
               :capabilities (alpha-store/capabilities env')
               :invariants   inv-result
               :me           (atom (or me-doc {}))
               :last-anchors (atom [])
               :xt-node      node
               :xt/db        (when node (xt/db node))}]
    (when-not (-> state :me deref :entity/id)
      (let [entity-spec {:id :me, :name "Me", :type :person, :pinned? true}]
        (store/ensure-entity! conn env' entity-spec)
        (swap! (:me state) assoc :entity/id :me)
        (write-profile! dir @(:me state))))
    (->canonical-state state)))

(defn state->ctx [state]
  {:profile     (:profile state)
   :profile-dir (:profile-dir state)
   :env         (:env state)
   :conn        (:conn state)
   :arxana-store (:arxana-store state)
   :capabilities (:capabilities state)
   ;; pass-through if present; harmless if nil
   :xt-node     (:xt-node state)
   :xt/db       (:xt/db state)})

(defn ensure-profile!
  "Return the state map for the given profile, creating it when necessary."
  ([profile]
   (let [id (or profile (default-profile))]
      (let [state (or (get @!profiles id)
                      (let [state (create-profile-state id)]
                        (swap! !profiles assoc id state)
                        state))
            state' (if (xt/started?)
                     (let [node (xt/node)
                           db   (xt/db node)
                           updated (assoc state :xt-node node :xt/db db)]
                       (swap! !profiles assoc id updated)
                       updated)
                     state)]
        state')))
  ([]
   (ensure-profile! nil)))

(defn conn [profile]
  (:conn (ensure-profile! profile)))

(defn env [profile]
  (:env (ensure-profile! profile)))

(defn arxana-store [profile]
  (:arxana-store (ensure-profile! profile)))

(defn profile-doc [profile]
  @(-> (ensure-profile! profile) :me))

(defn upsert-profile!
  [profile patch]
  (let [{:keys [me profile-dir]} (ensure-profile! profile)]
    (let [merged (swap! me #(merge (or % {}) (or patch {})))]
      (write-profile! profile-dir merged)
      merged)))

(defn record-anchors!
  [profile anchors]
  (reset! (-> (ensure-profile! profile) :last-anchors) (vec anchors)))

(defn current-anchors [profile]
  (let [state (ensure-profile! profile)
        cached @(:last-anchors state)]
    (if (seq cached)
      cached
      (let [candidates (focus/*focus-candidates* nil #{} nil 3)
            top (first candidates)]
        (if top
          (let [entity (:entity top)
                anchor {:id (:entity/id entity)
                        :name (:entity/name entity)
                        :type (:entity/type entity)}]
            [anchor])
          [])))))

(defn- render-value [v]
  (cond
    (map? v) (->> (sort-by key v)
                  (map (fn [[k val]]
                         (str (name k) ": " (render-value val))))
                  (str/join "; "))
    (sequential? v) (->> v (map render-value) (str/join ", "))
    (keyword? v) (name v)
    (string? v) v
    (number? v) (str v)
    (boolean? v) (str v)
    (nil? v) ""
    :else (pr-str v)))

(defn profile-summary
  [profile limit]
  (let [doc (profile-doc profile)
        text (if (seq doc)
               (->> (sort-by key doc)
                    (map (fn [[k v]]
                           (str (name k) ": " (render-value v))))
                    (remove str/blank?)
                    (str/join "\n"))
               "No profile data recorded.")
        trimmed (if (and (number? limit) (> (count text) limit))
                  (subs text 0 limit)
                  text)]
    (if (str/blank? trimmed)
      "No profile data recorded."
      trimmed)))

(defonce !state (atom nil))
(defonce ^:private !xtdb-watchdog (atom nil))

(defn current [] @!state)  ;; small helper, useful everywhere

;; Unchanged (good)
(defn ctx
  "Return a ctx map you can pass around (per-profile)."
  ([] (ctx (default-profile)))
  ([profile]
   (let [state (ensure-profile! profile)]
     {:profile     (:profile state)
      :profile-dir (:profile-dir state)
      :env         (:env state)   ;; has :xtdb config, etc.
      :conn        (:conn state)  ;; datascript conn
      :arxana-store (:arxana-store state)
      :capabilities (:capabilities state)
      :xtdb-node   (:xt-node state)
      :xtdb-db     (:xt/db state)})))

(defn default-capabilities []
  (alpha-store/capabilities {}))

;; Revised
(defn start!
  "Configure and ensure the default profile state is created (starts stores as needed).
   Precedence: ENV/JVM > config.edn > defaults. `opts` can further override at runtime."
  ([] (start! {}))
  ([opts]
   (cfg/log-once!)
   ;; merge config + optional runtime opts (your 'loophole')
   (configure! (merge (cfg/config) opts))

   (let [st        (ensure-profile! (default-profile))
         data-file (let [raw (or (some-> st :env :data-dir)
                                 (cfg/data-dir))]
                     (if (instance? java.io.File raw)
                       raw
                       (io/file (str raw))))]
     (when (get-in st [:env :xtdb :enabled?] true)
       (start-xtdb-watchdog!))
     (reset! !state
             {:data-dir     (.getPath ^java.io.File data-file)          ;; string for JSON/diag
              :config       (select-keys (cfg/config)
                                         [:app/data-dir :app/server-port :xtdb/config-path
                                          :warmup/enable? :warmup/focus-k])
              :profile      (:profile st)
              :profile-dir  (str (:profile-dir st))
              :env          (update (:env st) :data-dir #(str %))
              :invariants   (:invariants st)
              ;; quick booleans for health
              :has-ds       (boolean (:conn st))
              :has-xt-node  (boolean (:xt-node st))
              :has-xt-db    (boolean (:xt/db st))
              :xtdb/watchdog (get @!state :xtdb/watchdog)
              ;; (optional) expose live handles for REPL poking; drop if you prefer
              :xtdb-node    (:xt-node st)
              :conn         (:conn st)})

     (state->ctx st))))

(defn rehydrate!
  "Rebuild the in-memory store(s) from XTDB/legacy sources for the active config."
  ([] (rehydrate! {}))
  ([opts]
   (start! opts)))

(defn diag []
  (let [{:keys [data-dir config profile profile-dir]} (current)]
    {:ok true
     :data-dir data-dir
     :profile profile
     :profile-dir (str profile-dir)
     :config config
     :capabilities (:capabilities (ctx))
     :xtdb? (boolean (:xtdb (:env (ctx))))
     :xtdb/watchdog (get (current) :xtdb/watchdog)})) ;; or richer status if you like
