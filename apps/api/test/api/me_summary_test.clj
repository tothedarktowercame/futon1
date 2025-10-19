(ns api.me-summary-test
  (:require
   [api.handlers.me :as me]
   [app.command-service :as commands]
   [app.store-manager :as store-manager]
   [app.xt :as xt]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is use-fixtures]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; Dynamic bindings so fixtures can thread context into each test
(def ^:dynamic *ctx* nil)
(def ^:dynamic *conn* nil)
(def ^:dynamic *env* nil)
(def ^:dynamic *profile* nil)

(defn- temp-dir []
  (-> (Files/createTempDirectory "me-summary-test" (make-array FileAttribute 0))
      (.toFile)))

(defn- delete-recursively [^java.io.File f]
  (when (.exists f)
    (doseq [file (reverse (file-seq f))]
      (io/delete-file file true))))

(use-fixtures
  :each
  (fn [f]
    (let [tmp          (temp-dir)
          ;; Use the lightweight XT config that tests rely on
          config-path  (-> (io/resource "xtdb-test.edn") io/file .getAbsolutePath)
          cfg          {:data-root (.getAbsolutePath tmp)
                        :snapshot-every 100
                        :xtdb {:enabled? true
                               :config-path config-path}}
          ;; Start the full app store so we get conn/env/profile + xt node
          ctx          (store-manager/start! cfg)
          conn         (:conn ctx)
          env          (:env ctx)
          profile      (:profile ctx)]
      (try
        (binding [*ctx*     ctx
                  *conn*    conn
                  *env*     env
                  *profile* profile]
          (f))
        (finally
          (store-manager/shutdown!)
          (delete-recursively tmp))))))

;; --- helpers ---------------------------------------------------------------

(defn- ->db [xt-node]
  ;; Use your app.xt alias if needed; this returns the QueryDatasource (good)
  (xt/db xt-node))

(defn- eid-by-name*  ;; robust fallback if ensure-entity! doesn't give back an id
  [db nm]
  ;; Try map-form query with args map (XTDB 2 friendly)
  (try
    (xt/q db
          '{:find  [e .]
            :where [[e :entity/name n]]
            :in    [n]}
          {:n nm})
    (catch Throwable _
      ;; Fallback: minimal vector form (older adapters)
      (first (xt/q db
                   '[:find ?e
                     :in $ ?n
                     :where [?e :entity/name ?n]]
                   nm)))))

(defn- ensure+entity!
  "Ensure entity exists and return the enriched entity map (with :id)."
  [ctx db {:keys [name] :as ent}]
  (let [result (commands/ensure-entity! ctx ent)
        entity (:entity result)
        ;; some callers may only know the name; fall back to query if ensure missed the id
        eid    (or (:id entity)
                   (:entity/id entity)
                   (eid-by-name* db name))
        entity* (cond-> (or entity {})
                  eid (assoc :id eid))]
    (assert eid (str "Could not resolve entity id for " (pr-str name) " after ensure-entity!"))
    entity*))

;; --- seeding ---------------------------------------------------------------

(defn- seed-example-data! []
  ;; Return the IDs we need so later code never has to guess.
  (let [ctx  {:conn *conn* :env *env*}
        node (or (:xtdb-node *ctx*) (xt/node))
        db   (->db node)
        me-entity (ensure+entity! ctx db {:name "Me" :type :person})
        wd-entity (ensure+entity! ctx db {:name "Willie Dixon" :type :person})
        me-id (:id me-entity)
        wd-id (:id wd-entity)]

    ;; Upsert relation using concrete IDs to avoid name resolution at query time
    (commands/upsert-relation! ctx {:type :likes
                                    :src  (select-keys me-entity [:id :name :type])
                                    :dst  (select-keys wd-entity [:id :name :type])})
    ;; Make sure the exact node we'll query is in sync
;;    (xt/sync-node!)
    {:node node :me-id me-id :wd-id wd-id}))

;; --- the test --------------------------------------------------------------

(deftest api-summary-matches-cli-summary
  (let [{:keys [node me-id]} (seed-example-data!)
        now         (System/currentTimeMillis)
        profile-id  *profile*
        headers     {"accept" "text/plain"}]

    ;; CLI context — pass the real profile AND the concrete :entity id
    (let [cli-ctx    {:conn            *conn*
                      :env             *env*
                      :profile         profile-id
                      :default-profile profile-id
                      :entity          me-id
                      :now             now
                      :xt-node         node}
          ;; NOTE: if arity differs in your codebase, use (commands/profile-summary cli-ctx) or pass {} instead of nil
          cli-summary (commands/profile-summary cli-ctx nil)
          cli-text    (:text cli-summary)

          ;; API context — same ids/node so both paths see identical data
          base-ctx   {:conn            *conn*
                      :env             *env*
                      :profile         profile-id
                      :default-profile profile-id
                      :entity          me-id
                      :xt-node         node
                      :xtdb-node       node}
          response   (me/summary {:ctx base-ctx :headers headers :query-params {}})
          api-text   (:body response)

          drop-meta  (fn [lines]
                       (->> lines
                            (remove #(or (str/starts-with? % "Profile:")
                                         (str/starts-with? % "Generated at:")
                                         (str/starts-with? % "Generated:")
                                         (str/blank? %)))
                            vec))
          cli-lines  (vec (str/split-lines cli-text))
          api-lines  (vec (str/split-lines api-text))]

      (is (str/includes? cli-text "Willie Dixon") "CLI summary should include seeded relation")
      (is (= 200 (:status response)) "API should return HTTP 200")
      (is (str/includes? api-text "Willie Dixon") "API summary should include seeded relation")
      (is (= (drop-meta cli-lines)
             (drop-meta api-lines))
          "API and CLI summaries should match apart from metadata"))))
