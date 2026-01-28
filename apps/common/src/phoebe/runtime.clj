(ns phoebe.runtime
  "Runtime census and startup banner."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]))

(def ^:private default-env-keys
  ["BASIC_CHAT_DATA_DIR"
   "ALPHA_PROFILE"
   "ALPHA_PORT"
   "FUTON1_API"
   "FUTON1_API_BASE"
   "FUTON1_PROFILE"
   "XTDB_CONFIG"
   "FUTON3_ROOT"
   "FUTON5_ROOT"
   "NONSTARTER_DB"
   "MUSN_DANA_ENABLED"
   "MUSN_DANA_CMD"
   "MUSN_PORT"
   "MUSN_SSL_PORT"
   "MUSN_SSL_DOMAIN"
   "FUTON3_DRAWBRIDGE"
   "ADMIN_TOKEN"
   "JAVA_HOME"])

(def ^:private redact-keys
  #{"ADMIN_TOKEN"})

(def ^:private redact-patterns
  [#"(?i)token"
   #"(?i)password"
   #"(?i)secret"])

(defn- redact?
  [k]
  (or (contains? redact-keys k)
      (some #(re-find % k) redact-patterns)))

(defn- env-value
  [k overrides]
  (let [raw (some-> (System/getenv k) str/trim)
        override (get overrides k)]
    (cond
      (and raw (not (str/blank? raw)))
      {:value (if (redact? k) "<redacted>" raw) :source :env}

      (and (some? override) (not (and (string? override) (str/blank? override))))
      {:value (if (redact? k) "<redacted>" override) :source :resolved}

      :else
      {:value "<unset>" :source :unset})))

(defn- env-snapshot
  [keys overrides]
  (mapv (fn [k] [k (env-value k overrides)]) keys))

(defn- sh*
  [& args]
  (try
    (apply sh/sh args)
    (catch Throwable _ nil)))

(defn- git-root
  []
  (let [res (sh* "git" "rev-parse" "--show-toplevel")]
    (when (and res (zero? (:exit res)))
      (str/trim (:out res)))))

(defn- git-head
  [root]
  (let [res (sh* "git" "-C" root "rev-parse" "HEAD")]
    (when (and res (zero? (:exit res)))
      (str/trim (:out res)))))

(defn- git-branch
  [root]
  (let [res (sh* "git" "-C" root "rev-parse" "--abbrev-ref" "HEAD")]
    (when (and res (zero? (:exit res)))
      (str/trim (:out res)))))

(defn- git-dirty?
  [root]
  (let [res (sh* "git" "-C" root "diff" "--quiet")]
    (when res
      (case (:exit res)
        0 false
        1 true
        nil))))

(defn- git-untracked
  [root]
  (let [res (sh* "git" "-C" root "status" "--porcelain")]
    (when (and res (zero? (:exit res)))
      (->> (str/split-lines (:out res))
           (filter #(str/starts-with? % "??"))
           count))))

(defn- git-info
  []
  (when-let [root (git-root)]
    {:root root
     :head (git-head root)
     :branch (git-branch root)
     :dirty? (git-dirty? root)
     :untracked (git-untracked root)}))

(defn- runtime-info
  []
  {:java (System/getProperty "java.version")
   :java-vendor (System/getProperty "java.vendor")
   :clojure (clojure-version)
   :os (System/getProperty "os.name")
   :os-version (System/getProperty "os.version")
   :arch (System/getProperty "os.arch")
   :cwd (System/getProperty "user.dir")})

(defn snapshot
  "Return a runtime census map. Accepts optional keys:
   :app, :config, :ports, :services, :env-keys, :env-overrides."
  ([]
   (snapshot {}))
  ([{:keys [app config ports services env-keys env-overrides] :as _opts}]
   {:ts (java.time.Instant/now)
    :app app
    :runtime (runtime-info)
    :git (git-info)
    :env (env-snapshot (or env-keys default-env-keys) (or env-overrides {}))
    :ports ports
    :services services
    :config config}))

(defn- format-pairs
  [pairs]
  (->> pairs
       (map (fn [[k v]] (format "  %-24s %s" k v)))
       (str/join "\n")))

(defn- format-env
  [pairs]
  (->> pairs
       (map (fn [[k {:keys [value source]}]]
              (let [suffix (case source
                             :env ""
                             :resolved " (resolved)"
                             :unset ""
                             "")]
                (format "  %-24s %s%s" k value suffix))))
       (str/join "\n")))

(defn- format-map
  [m]
  (when (seq m)
    (format-pairs (map (fn [[k v]] [(name k) (pr-str v)]) m))))

(defn format-banner
  [snap]
  (let [env-lines (format-env (:env snap))
        runtime (:runtime snap)
        git (:git snap)
        ports-lines (format-map (:ports snap))
        services-lines (format-map (:services snap))
        config-lines (format-map (:config snap))]
    (str
     "[phoebe] runtime census\n"
     (when-let [app (:app snap)]
       (str "app:\n" (format-pairs (map (fn [[k v]] [(name k) (pr-str v)]) app)) "\n"))
     "ts:\n"
     (format-pairs [["time" (str (:ts snap))]]) "\n"
     "runtime:\n"
     (format-pairs [["java" (:java runtime)]
                    ["clojure" (:clojure runtime)]
                    ["os" (str (:os runtime) " " (:os-version runtime) " " (:arch runtime))]
                    ["cwd" (:cwd runtime)]]) "\n"
     (when git
       (str "git:\n"
            (format-pairs [["root" (:root git)]
                           ["branch" (:branch git)]
                           ["head" (:head git)]
                           ["dirty?" (:dirty? git)]
                           ["untracked" (:untracked git)]])
            "\n"))
     (when ports-lines
       (str "ports:\n" ports-lines "\n"))
     (when services-lines
       (str "services:\n" services-lines "\n"))
     (when config-lines
       (str "config:\n" config-lines "\n"))
     "env:\n"
     env-lines)))

(defn print-banner!
  "Print a startup banner and return the snapshot."
  ([]
   (print-banner! {}))
  ([opts]
   (let [snap (snapshot opts)]
     (println (format-banner snap))
     (flush)
     snap)))
