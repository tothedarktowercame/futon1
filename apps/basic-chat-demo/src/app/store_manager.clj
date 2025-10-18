(ns app.store-manager
  "Manage Datascript/XTDB resources and per-profile metadata for the headless API."
  (:require [app.focus :as focus]
            [app.store :as store]
            [app.xt :as xt]
            [app.config :as cfg]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [graph-memory.me-profile :as me-profile])
  (:import (java.io PushbackReader)
           (java.util UUID)))

(declare profile-doc)

(def ^:private profile-filename "profile.edn")

(defonce ^:private !config (atom nil))
(defonce ^:private !profiles (atom {}))

(defn- repo-root []
  (loop [dir (io/file (System/getProperty "user.dir"))]
    (when dir
      (if (.exists (io/file dir "AGENTS.md"))
        dir
        (recur (.getParentFile dir))))))

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
        base {:enabled? enabled?}
        from-resource (when (nil? resource)
                        (some-> (io/resource "xtdb.edn") io/file .getAbsolutePath))]
    (cond-> base
      resource (assoc :resource resource)
      (and (nil? resource) from-resource) (assoc :config-path from-resource))))

(defn default-config []
  (let [data-root (or (getenv-trim "BASIC_CHAT_DATA_DIR")
                      (let [root (repo-root)]
                        (if root
                          (.getAbsolutePath (io/file root "data"))
                          (.getAbsolutePath (io/file "data")))))
        snapshot (some-> (getenv-trim "ALPHA_SNAPSHOT_EVERY") Integer/parseInt)
        profile (or (getenv-trim "ALPHA_PROFILE") "default")]
    {:data-root (.getAbsolutePath (io/file data-root))
     :snapshot-every (or snapshot 100)
     :xtdb (xt-config)
     :default-profile profile}))

(defn configure!
  "Replace the active configuration and clear in-memory profile caches."
  ([]
   (configure! {}))
  ([opts]
   (let [cfg (merge (default-config) opts)]
     (reset! !config cfg)
     (reset! !profiles {})
     (when-let [root (:data-root cfg)]
       (System/setProperty "basic-chat.data-root" root))
     cfg)))

(defn shutdown!
  "Stop XTDB and clear cached profile state."
  []
  (reset! !profiles {})
  (System/clearProperty "basic-chat.data-root")
  (xt/stop!))

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

(defn- raw-profile-name [doc profile-id]
  (let [prefs (me-profile/preferences-from-manual doc)
        names (:preferred-names prefs)]
    (or (first names)
        (some-> profile-id str str/trim not-empty)
        "Me")))

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
  (let [{:keys [snapshot-every xtdb]} (config)
        dir   (ensure-dir! (profile-dir profile))        ;; -> java.io.File
        me-doc (read-profile dir)
        env   {:data-dir       dir
               :snapshot-every snapshot-every
               :xtdb           xtdb}
        conn  (store/restore! env me-doc)
        state {:profile      profile
               :profile-dir  dir
               :env          env
               :conn         conn
               :me           (atom (or me-doc {}))
               :last-anchors (atom [])}]
    (when-not (-> state :me deref :entity/id)
      (let [entity-spec {:id :me, :name "Me", :type :person, :pinned? true}]
        (store/ensure-entity! conn env entity-spec)
        (swap! (:me state) assoc :entity/id :me)
        (write-profile! dir @(:me state))))
    (->canonical-state state)))

(defn state->ctx [state]
  {:profile     (:profile state)
   :profile-dir (:profile-dir state)
   :env         (:env state)
   :conn        (:conn state)
   ;; pass-through if present; harmless if nil
   :xt-node     (:xt-node state)
   :xt/db       (:xt/db state)})

(defn ensure-profile!
  "Return the state map for the given profile, creating it when necessary."
  ([profile]
   (let [id (or profile (default-profile))]
     (or (get @!profiles id)
         (let [state (create-profile-state id)]
           (swap! !profiles assoc id state)
           state))))
  ([]
   (ensure-profile! nil)))

(defn ctx
  ([] (ctx (default-profile)))
  ([profile] (state->ctx (ensure-profile! profile))))

(defn conn [profile]
  (:conn (ensure-profile! profile)))

(defn env [profile]
  (:env (ensure-profile! profile)))

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
      (let [candidates (focus/focus-candidates nil #{} nil 3)
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
      :conn        (:conn state)}))) ;; datascript conn

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
     (reset! !state
             {:data-dir     (.getPath ^java.io.File data-file)          ;; string for JSON/diag
              :config       (select-keys (cfg/config)
                                         [:app/data-dir :app/server-port :xtdb/config-path
                                          :warmup/enable? :warmup/focus-k])
              :profile      (:profile st)
              :profile-dir  (str (:profile-dir st))
              :env          (update (:env st) :data-dir #(str %))
              ;; quick booleans for health
              :has-ds       (boolean (:conn st))
              :has-xt-node  (boolean (:xt-node st))
              :has-xt-db    (boolean (:xt/db st))
              ;; (optional) expose live handles for REPL poking; drop if you prefer
              :xtdb-node    (:xt-node st)
              :conn         (:conn st)})

     (state->ctx st))))

(defn diag []
  (let [{:keys [data-dir config profile profile-dir env]} (current)]
    {:ok true
     :data-dir data-dir
     :profile profile
     :profile-dir (str profile-dir)
     :config config
     :xtdb? (boolean (:xtdb (:env (ctx))))})) ;; or richer status if you like
