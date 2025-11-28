(ns client.session
  "Session management for the new Clojure client."
  (:require [app.commands :as commands]
            [app.slash :as slash]
            [app.store-manager :as store-manager]
            [app.xt :as xt]
            [client.engine :as engine]
            [client.runner :as runner]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [xtdb.api :as xta]))

(defn- now-ms [] (System/currentTimeMillis))

(defn- profile-from [ctx]
  (or (:profile ctx) (store-manager/default-profile)))

(defn- healthy-node [node]
  (when node
    (try
      ;; ensure the executor is alive before reusing the node
      (xta/db node)
      node
      (catch Throwable _
        nil))))

(defn- ensure-xt-node! [{:keys [xtdb data-dir] :as _env}]
  (let [enabled? (get xtdb :enabled? true)
        cfg (or (:config-path xtdb)
                 (some-> (:resource xtdb) io/resource .getPath)
                 (some-> (io/resource "xtdb.edn") .getPath))
        xt-data-dir (or (:data-dir xtdb)
                        (when data-dir
                          (-> (io/file data-dir "xtdb") .getAbsolutePath)))
        existing (healthy-node (when (xt/started?) (xt/node)))]
    (if existing
      existing
      (do
        (when (xt/started?)
          (xt/stop!))
        (when (and enabled? cfg)
          (xt/restart! cfg (cond-> {:xt/created-by "client.session/ensure-xt-node!"}
                              xt-data-dir (assoc :data-dir xt-data-dir))))
        (healthy-node (xt/node))))))

(defn- make-xt-node-controller [env-atom]
  (let [!node (atom nil)]
    {:ensure (fn ensure-node!
               []
               (if-let [node (healthy-node @!node)]
                 node
                 (let [env @env-atom
                       node (when env (ensure-xt-node! env))]
                   (reset! !node node)
                   node)))
     :reset! #(reset! !node nil)}))

(defn- ctx->cli-ctx [profile ensure-node! env-atom]
  (let [ctx (store-manager/ctx profile)
        env (:env ctx)]
    (reset! env-atom env)
    {:db (:conn ctx)
     :conn (:conn ctx)
     :arxana-store (:arxana-store ctx)
     :capabilities (:capabilities ctx)
     :env env
     :default-profile profile
     :xtdb-node (ensure-node!)}))

(defn- make-slash-handler [profile ensure-node! env-atom]
  (let [base-ctx (store-manager/ctx profile)
        slash-state (atom {:conn (:conn base-ctx)
                           :arxana-store (:arxana-store base-ctx)
                           :capabilities (:capabilities base-ctx)})
        opts-fn (fn []
                  (let [ctx (store-manager/ctx profile)
                        env (:env ctx)]
                    (reset! env-atom env)
                    {:env env
                      :arxana-store (:arxana-store ctx)
                      :capabilities (:capabilities ctx)
                      :xt-node (ensure-node!)
                      :xt-node-fn ensure-node!}))
        handler-fn (fn []
                     (slash/handler (opts-fn) slash-state))]
    {:state slash-state
     :handler-fn handler-fn}))

(defn- make-bang-handler [profile ensure-node! env-atom]
  (letfn [(impl [cmd state ctx]
            (let [{:keys [conn env]} ctx
                  opts {:profile profile
                        :env env
                        :state state
                        :interactive? true
                        :data-dir (:data-dir env)
                        :xtdb (:xtdb env)}
                  {:keys [message result] :as out} (commands/handle conn opts cmd)
                  next-state (assoc state :last-bang {:cmd cmd :out out})]
            {:message (or message result (str "ok: " cmd))
             :new-state next-state}))]
    (fn
      ([cmd state ctx] (impl cmd state ctx))
      ([cmd state]
       (impl cmd state (ctx->cli-ctx profile ensure-node! env-atom))))))

(defn- call-runner [runner line ts state ctx]
  (try
    (runner line ts state ctx)
    (catch clojure.lang.ArityException _
      (runner line ts))))

(defn- call-command [handler cmd state ctx]
  (try
    (handler cmd state ctx)
    (catch clojure.lang.ArityException _
      (handler cmd state))))

(defn start
  "Create a new in-process client session.

  Optional opts are forwarded to store-manager/start!."
  ([] (start {}))
  ([opts]
   (let [sm-ctx (store-manager/start! opts)
         profile (profile-from sm-ctx)
         env-atom (atom (:env sm-ctx))
         node-controller (make-xt-node-controller env-atom)
         ensure-node! (:ensure node-controller)
         reset-node! (:reset! node-controller)
         _ (ensure-node!)
         slash (make-slash-handler profile ensure-node! env-atom)
         bang-handler (make-bang-handler profile ensure-node! env-atom)
         state (atom {})
         protocol (:protocol opts)]
     {:profile profile
      :state state
      :slash slash
      :bang-handler bang-handler
      :runner runner/runner
      :ctx-provider #(cond-> (ctx->cli-ctx profile ensure-node! env-atom)
                       protocol (assoc :protocol protocol))
      :env-atom env-atom
      :reset-xt-node! reset-node!
      :protocol protocol})))

(def ^:private slash-command-capabilities
  {"tail" [:links :list?]
   "ego" [:links :list?]
   "cooccur" [:events :query?]})

(defn- capability-supported? [ctx path]
  (get-in (:capabilities ctx) path true))

(def ^:private unavailable-command-msg
  #(str "Command /" % " is not available on this profile."))

(defn stop
  "Shutdown resources associated with the current store-manager."
  ([] (store-manager/shutdown!))
  ([session]
   (when session
     (when-let [reset-node! (:reset-xt-node! session)]
       (reset-node!))
     (store-manager/shutdown!))))

(defn process
  "Process a single line of user input.

  Returns {:type :say|:slash|:bang|:blank, :data <result map>}.
  Session state is maintained internally via atoms."
  [session line]
  (let [{:keys [runner ctx-provider state slash bang-handler profile]} session
        state-atom state
        classification (engine/classify line)]
    (case (:type classification)
      :blank {:type :blank :data {:raw line}}

      :slash (let [{:keys [handler-fn state]} slash
                   slash-state state
                   handler (handler-fn)
                   ctx (ctx-provider)
                   command-name (some-> classification :raw (str/split #"\s+") first str/lower-case)]
               (if (slash-supported? ctx command-name)
                 (do
                   (swap! slash-state assoc
                          :conn (:conn ctx)
                          :arxana-store (:arxana-store ctx)
                          :capabilities (:capabilities ctx))
                   (let [snapshot @state-atom
                         {:keys [message new-state] :or {new-state snapshot}}
                         (call-command handler (:raw classification) snapshot ctx)]
                     (reset! state-atom new-state)
                     {:type :slash :data {:message message}}))
                 {:type :slash
                  :data {:message (unavailable-command-msg command-name)}}))

      :bang  (let [ctx (assoc (ctx-provider) :profile profile)
                   snapshot @state-atom
                   {:keys [message new-state] :or {new-state snapshot}}
                   (call-command bang-handler (:raw classification) snapshot ctx)]
               (reset! state-atom new-state)
               {:type :bang :data {:message message}})

      :say   (let [ctx (ctx-provider)
                   snapshot @state-atom
                   ts (now-ms)
                   {:keys [new-state] :as out}
                   (call-runner runner (:raw classification) ts snapshot ctx)
                   next-state (or new-state snapshot)
                   data (-> out
                            (dissoc :new-state)
                            (assoc :input (:raw classification)))]
               (reset! state-atom next-state)
               {:type :say :data data}))))
