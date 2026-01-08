;; scripts/pattern_language_includes_backfill.clj
(ns scripts.pattern-language-includes-backfill
  "Backfill :pattern/language relations from language entities to pattern libraries."
  (:require [app.store :as store]
            [app.store-manager :as store-manager]
            [clojure.string :as str]
            [datascript.core :as d])
  (:gen-class))

(def ^:private relation-type :arxana/scholium)
(def ^:private relation-note ":pattern-language/includes")
(def ^:private language-type :pattern/language)
(def ^:private pattern-type :pattern/library)
(def ^:private futon3-source-token "/futon3/library/")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clojure -M -m scripts.pattern-language-includes-backfill [--dry-run]"
    "Environment:"
    "  ALPHA_PROFILE      Profile name (defaults to configured profile)"
    "  BASIC_CHAT_DATA_DIR Data root (required if server uses a non-default data dir)"]))

(defn- parse-args [args]
  (loop [args args opts {:dry-run? false}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--dry-run" arg) (recur (rest args) (assoc opts :dry-run? true))
          (= "--help" arg) (do
                             (println (usage))
                             (System/exit 0))
          (str/starts-with? arg "-") (throw (ex-info "Unknown argument" {:arg arg}))
          :else (throw (ex-info "Unexpected argument" {:arg arg})))))))

(defn- language-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :pattern/language]]
                      db)
                  (map first))]
    (->> ids
         (map #(d/pull db [:entity/id :entity/name :entity/type :entity/source] %))
         (filter (fn [lang]
                   (let [source (or (:entity/source lang) "")]
                     (str/includes? source futon3-source-token)))))))

(defn- pattern-entities [db]
  (let [ids (->> (d/q '[:find ?e
                        :where
                        [?e :entity/type :pattern/library]]
                      db)
                  (map first))]
    (map #(d/pull db [:entity/id :entity/name :entity/type] %) ids)))

(defn- relation-exists? [db src-id dst-id]
  (boolean
   (seq (d/q '[:find ?r
               :in $ ?src-id ?dst-id
               :where
               [?r :relation/type :arxana/scholium]
               [?r :relation/src ?src]
               [?src :entity/id ?src-id]
               [?r :relation/dst ?dst]
               [?dst :entity/id ?dst-id]]
             db src-id dst-id))))

(defn- language-slug [name]
  (let [raw (or name "")
        trimmed (str/trim raw)]
    (cond
      (str/starts-with? trimmed "pattern-language/")
      (subs trimmed (count "pattern-language/"))
      :else trimmed)))

(defn- match-patterns [patterns slug]
  (let [prefix (str slug "/")]
    (filter (fn [pattern]
              (str/starts-with? (or (:entity/name pattern) "") prefix))
            patterns)))

(defn -main [& args]
  (let [{:keys [dry-run?]} (parse-args args)
        profile (store-manager/default-profile)
        conn (store-manager/conn profile)
        env (store-manager/env profile)]
    (try
      (let [db @conn
            languages (language-entities db)
            patterns (vec (pattern-entities db))
            created (atom 0)
            skipped (atom 0)]
        (println (format "Pattern language backfill: languages=%d patterns=%d dry-run=%s"
                         (count languages) (count patterns) dry-run?))
        (doseq [lang languages
                :let [slug (language-slug (:entity/name lang))
                      matches (match-patterns patterns slug)]]
          (doseq [pattern matches
                  :let [src-id (:entity/id lang)
                        dst-id (:entity/id pattern)
                        exists? (relation-exists? @conn src-id dst-id)]]
            (cond
              exists?
              (swap! skipped inc)

              dry-run?
              (do
                (swap! created inc)
                (println (format "Would link %s -> %s"
                                 (:entity/name lang)
                                 (:entity/name pattern))))

              :else
              (do
                (store/upsert-relation! conn env
                                        {:type relation-type
                                         :provenance {:note relation-note}
                                         :src {:id src-id
                                               :name (:entity/name lang)
                                               :type language-type}
                                         :dst {:id dst-id
                                               :name (:entity/name pattern)
                                               :type pattern-type}})
                (swap! created inc)))))
        (println (format "Backfill summary: created=%d skipped=%d"
                         @created @skipped)))
      (finally
        (store-manager/shutdown!)))))
