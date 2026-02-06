(ns app.invariant-features-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datascript.core :as d]
            [app.store :as store]
            [app.model :as model]
            [app.model-media :as model-media]
            [app.model-meta :as model-meta]
            [app.model-docbook :as model-docbook]
            [app.model-open-world :as model-open-world]
            [app.model-penholder :as model-penholder]
            [app.type-counts :as type-counts]
            [app.xt :as xt]
            [xtdb.api :as xtdb]
            [app.sigil-allowlist :as sigil-allowlist])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (str (.toAbsolutePath (Files/createTempDirectory "inv-test" (make-array FileAttribute 0)))))

(defn- read-edn-lines [path]
  (when (.exists (io/file path))
    (->> (line-seq (io/reader path))
         (remove str/blank?)
         (map edn/read-string)
         vec)))

(defn- model-result [results key]
  (first (filter #(= key (:key %)) results)))

(deftest tx-logs-write-attempt-and-commit
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        opts {:data-dir dir}
        event {:type :entity/upsert
               :entity {:id "e1" :name "Entity 1" :type :note}}]
    (store/tx! conn opts event)
    (let [attempts (read-edn-lines (io/file dir "logs" "write-attempts.ednlog"))
          commits (read-edn-lines (io/file dir "logs" "write-commits.ednlog"))]
      (testing "attempt log is written before commit"
        (is (seq attempts))
        (is (seq commits))
        (is (= (:attempt/id (first attempts)) (:attempt/id (first commits)))))
      (testing "commit log captures result"
        (is (contains? (first commits) :result))))))

(deftest meta-model-type-counts-nondecreasing
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        env {:data-dir dir}
        _ (model-meta/ensure-descriptor! conn env)
        baseline {:entity {:arxana/media-lyrics 4}}
        current {:entity {:arxana/media-lyrics 3}}]
    (with-redefs [type-counts/load-baseline (fn [_] baseline)
                  type-counts/durable-type-counts (fn [] current)
                  type-counts/compare-type-counts (fn [_ _ _]
                                                    {:ok? false
                                                     :failures [{:type :arxana/media-lyrics
                                                                 :baseline 4
                                                                 :current 3}]})]
      (let [result (model-meta/verify conn {:type-counts/check? true
                                            :metadata-root "/tmp"
                                            :data-dir dir})
            check (model-result (:results result) :meta-model/type-counts-nondecreasing)]
        (is (false? (:ok? check)))
        (is (seq (:failures check)))))))

(deftest media-model-invariants-detect-missing-fields-and-links
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        env {:data-dir dir}
        _ (model-media/ensure-descriptor! conn env)
        _ (d/transact! conn [{:entity/id "track-1"
                              :entity/name "Track 1"
                              :entity/type :arxana/media-track}
                             {:entity/id "lyrics-1"
                              :entity/name "Lyrics 1"
                              :entity/type :arxana/media-lyrics
                              :entity/external-id "lyrics-1"
                              :entity/source "external"}])
        _ (d/transact! conn [{:relation/id "rel-1"
                              :relation/type :media/lyrics
                              :relation/src [:entity/id "lyrics-1"]
                              :relation/dst [:entity/id "track-1"]}])
        result (model-media/verify conn)
        by-key #(model-result (:results result) %)]
    (testing "track required fields"
      (is (false? (:ok? (by-key :media/track-required)))))
    (testing "lyrics required fields"
      (is (false? (:ok? (by-key :media/lyrics-required)))))
    (testing "lyrics placeholder source"
      (is (false? (:ok? (by-key :media/lyrics-source-content)))))
    (testing "lyrics linked"
      (is (false? (:ok? (by-key :media/lyrics-linked)))))
    (testing "lyrics link types"
      (is (false? (:ok? (by-key :media/lyrics-link-types)))))))

(deftest penholder-model-invariants
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        env {:data-dir dir}
        _ (model-penholder/ensure-descriptor! conn env)
        _ (store/ensure-entity! conn env {:id "pen-1"
                                          :name "Penholder"
                                          :type :model/penholder})
        result (model-penholder/verify conn)
        req (model-result (:results result) :penholder/entry-required)
        schema (model-result (:results result) :penholder/entry-schema)]
    (is (false? (:ok? req)))
    (is (false? (:ok? schema)))))

(deftest patterns-model-invariants
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        env {:data-dir dir}
        _ (model/ensure-descriptor! conn env)
        _ (store/ensure-entity! conn env {:id "lang-1"
                                          :name "pattern/lang"
                                          :type :pattern/language
                                          :source "/futon3/library/alpha"})
        _ (store/ensure-entity! conn env {:id "catalog"
                                          :name "pattern-language/catalog"
                                          :type :pattern/language-catalog})
        _ (store/ensure-entity! conn env {:id "pat-1"
                                          :name "pattern/one"
                                          :type :pattern/library})
        _ (store/ensure-entity! conn env {:id "sigil-1"
                                          :name "sigil/😀/test"
                                          :type :sigil
                                          :external-id "😀|漢"
                                          :source "futon3/sigil"})]
    (with-redefs [sigil-allowlist/allowlist-from-root (fn [] #{})
                  sigil-allowlist/sigil-allowed? (fn [_ _] false)]
      (testing "language and sigil invariants"
        (let [result (model/verify conn)
              by-key #(model-result (:results result) %)]
          (is (false? (:ok? (by-key :patterns/language-has-source))))
          (is (false? (:ok? (by-key :patterns/language-has-status))))
          (is (false? (:ok? (by-key :patterns/language-in-catalog))))
          (is (false? (:ok? (by-key :patterns/language-has-includes))))
          (is (false? (:ok? (by-key :patterns/sigils-allowlisted))))))
      (testing "pattern core components"
        (store/upsert-relation! conn env {:type :arxana/scholium
                                          :src {:id "lang-1" :name "pattern/lang" :type :pattern/language}
                                          :dst {:id "pat-1" :name "pattern/one" :type :pattern/library}})
        (let [result (model/verify conn)
              by-key #(model-result (:results result) %)]
          (is (false? (:ok? (by-key :patterns/pattern-core-components)))))))))

(deftest docbook-model-invariants
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        env {:data-dir dir}
        _ (model-docbook/ensure-descriptor! conn env)
        heading-docs {"h1" {:doc/id "h1" :doc/book "book-a" :doc/title nil
                             :doc/outline_path "a" :doc/path_string "a" :doc/level 1}
                      "h2" {:doc/id "h2" :doc/book "book-a" :doc/title "Heading" :doc/outline_path "b"
                             :doc/path_string "b" :doc/level 1}
                      "h3" {:doc/id "h3" :doc/book "book-a" :doc/title "Heading 3" :doc/outline_path "c"
                             :doc/path_string "c" :doc/level 1}}
        entry-docs {"e1" {:doc/id "h1" :doc/entry-id "e1" :doc/book nil :doc/body "" :doc/status :active}
                    "e2" {:doc/id "h2" :doc/entry-id "e2" :doc/book "book-b" :doc/body "no summary yet"}
                    "e3" {:doc/id "missing" :doc/entry-id "e3" :doc/book "book-a" :doc/body "body"}}
        toc-docs {"t1" {:doc/book "book-a" :doc/toc-order ["h2" "unknown"]}}
        query->ids {(pr-str '{:find [?e]
                              :where [[?e :doc/id _]
                                      (not [?e :doc/entry-id _])
                                      (not [?e :doc/toc-order _])]}) [["h1"] ["h2"] ["h3"]]
                    (pr-str '{:find [?e]
                              :where [[?e :doc/entry-id _]]}) [["e1"] ["e2"] ["e3"]]
                    (pr-str '{:find [?e]
                              :where [[?e :doc/toc-order _]]}) [["t1"]]}]
    (with-redefs [xt/started? (fn [] true)
                  xt/db (fn [] :stub-db)
                  xt/q (fn [_ q]
                         (get query->ids (pr-str q) []))
                  xtdb/entity (fn [_ id]
                                (or (get heading-docs id)
                                    (get entry-docs id)
                                    (get toc-docs id)))]
      (let [result (model-docbook/verify conn)
            by-key #(model-result (:results result) %)]
        (is (false? (:ok? (by-key :docbook/heading-required))))
        (is (false? (:ok? (by-key :docbook/entry-required))))
        (is (false? (:ok? (by-key :docbook/heading-has-entry))))
        (is (false? (:ok? (by-key :docbook/entry-body-required))))
        (is (false? (:ok? (by-key :docbook/entry-has-heading))))
        (is (false? (:ok? (by-key :docbook/entry-book-matches-heading))))
        (is (false? (:ok? (by-key :docbook/toc-covers-headings))))
        (is (false? (:ok? (by-key :docbook/toc-known-headings))))))))

(deftest open-world-model-invariants
  (let [dir (temp-dir)
        conn (d/create-conn store/schema)
        env {:data-dir dir}
        _ (model-open-world/ensure-descriptor! conn env)
        entity-docs {"e1" {:entity/id "e1" :entity/label "Entity" :entity/kind "bad"
                            :entity/first-seen 1 :entity/updated-at 1}
                     "stub1" {:entity/id "stub1" :entity/name "Stub"}}
        mention-docs {"m1" {:mention/id "m1" :mention/entity "missing-entity"
                             :mention/utterance "missing-utt" :mention/span [5 3]}}
        relation-docs {"r1" {:relation/id "r1" :relation/src "missing-src" :relation/dst "missing-dst"
                              :relation/type :rel/bad :relation/label "rel"
                              :relation/polarity 1 :relation/confidence 0.4
                              :relation/utterance "missing-utt" :relation/sentence 1 :relation/ts 1}}
        utterance-docs {"u1" {:utterance/id "u1" :utterance/text nil :utterance/ts 1
                               :utterance/entity-count 1 :utterance/relation-count 1}}
        type-docs {"t1" {:type/id "t1"}}
        query->ids {(pr-str '{:find [?eid]
                              :where [[?m :mention/text _]
                                      [?m :mention/entity ?eid]]}) [["stub1"]]
                    (pr-str '{:find [?eid]
                              :where [[?r :relation/label _]
                                      [?r :relation/src ?eid]]}) [["stub1"]]
                    (pr-str '{:find [?eid]
                              :where [[?r :relation/label _]
                                      [?r :relation/dst ?eid]]}) [["stub1"]]
                    (pr-str '{:find [?eid]
                              :where [[?e :entity/id ?eid]
                                      [?e :entity/label _]]}) [["e1"]]
                    (pr-str '{:find [?eid]
                              :where [[?e :entity/id ?eid]
                                      [?e :entity/lower-label _]]}) [["e1"]]
                    (pr-str '{:find [?eid]
                              :where [[?e :entity/id ?eid]
                                      [?e :entity/kind _]]}) [["e1"]]
                    (pr-str '{:find [?eid]
                              :where [[?e :entity/id ?eid]
                                      [?e :entity/first-seen _]]}) [["e1"]]
                    (pr-str '{:find [?eid]
                              :where [[?e :entity/id ?eid]
                                      [?e :entity/updated-at _]]}) [["e1"]]
                    (pr-str '{:find [?id]
                              :where [[?e :mention/text _]
                                      [?e :mention/id ?id]]}) [["m1"]]
                    (pr-str '{:find [?id]
                              :where [[?e :relation/label _]
                                      [?e :relation/id ?id]]}) [["r1"]]
                    (pr-str '{:find [?id]
                              :where [[?e :utterance/entity-count _]
                                      [?e :utterance/id ?id]]}) [["u1"]]
                    (pr-str '{:find [?id]
                              :where [[?e :type/id ?id]]}) [["t1"]]
                    (pr-str '{:find [?id]
                              :where [[?e :entity/id ?id]]}) []
                    (pr-str '{:find [?id]
                              :where [[?e :utterance/id ?id]]}) []
                    (pr-str '{:find [?id]
                              :where [[?t :type/id ?id]
                                      [?t :type/kind :relation]]}) []}]
    (with-redefs [xt/q (fn [query] (get query->ids (pr-str query) []))
                  xt/entity (fn [id]
                              (or (get entity-docs id)
                                  (get mention-docs id)
                                  (get relation-docs id)
                                  (get utterance-docs id)
                                  (get type-docs id)))]
      (let [result (model-open-world/verify conn)
            by-key #(model-result (:results result) %)]
        (is (false? (:ok? (by-key :open-world/entity-required))))
        (is (false? (:ok? (by-key :open-world/entity-stub-detected))))
        (is (false? (:ok? (by-key :open-world/entity-kind-valid))))
        (is (false? (:ok? (by-key :open-world/mention-required))))
        (is (false? (:ok? (by-key :open-world/mention-links))))
        (is (false? (:ok? (by-key :open-world/mention-span-valid))))
        (is (false? (:ok? (by-key :open-world/relation-required))))
        (is (false? (:ok? (by-key :open-world/relation-links))))
        (is (false? (:ok? (by-key :open-world/relation-type-registered))))
        (is (false? (:ok? (by-key :open-world/utterance-required))))
        (is (false? (:ok? (by-key :open-world/type-required))))))))
