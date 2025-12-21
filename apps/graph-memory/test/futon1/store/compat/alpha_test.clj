(ns futon1.store.compat.alpha-test
  (:require [app.store :as store]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon1.store.compat.alpha :as compat]
            [futon1.store.core :as core])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "alpha-store-test" (make-array FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when f
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (rm-rf child)))
    (when (.exists f)
      (io/delete-file f true))))

(deftest article-crud-roundtrip
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (testing "put/get/delete article"
        (core/put-article! store {:article/ident "math/group:Intro"
                                  :article/type [:concept]
                                  :article/bookkeeping {:seen-count 5}})
        (let [article (core/get-article store "math/group:Intro")]
          (is (= "math/group:Intro" (:article/ident article)))
          (is (= 5 (get-in article [:article/bookkeeping :seen-count]))))
        (core/delete-article! store "math/group:Intro")
        (is (nil? (core/get-article store "math/group:Intro"))))
      (finally
        (rm-rf dir)))))

(deftest metadata-not-supported
  (let [store (compat/alpha-store {:conn nil :env {}})]
    (is (= {:note "metadata support pending"} (core/get-meta store :default)))
    (is (= {:foo :bar}
           (core/update-meta! store :default (fn [_] {:foo :bar}))))))

(deftest find-articles-scan-legacy-entities
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (doseq [ident ["math/group:Intro"
                     "math/group:Advanced"
                     "physics/optics:Lens"]]
        (core/put-article! store {:article/ident ident}))
      (let [matches (core/find-articles store #(str/starts-with? (:article/ident %) "math/"))]
        (is (= ["math/group:Advanced" "math/group:Intro"]
               (map :article/ident matches))))
      (is (empty? (core/find-articles store #(str/starts-with? (:article/ident %) "bio/"))))
      (finally
        (rm-rf dir)))))

(deftest fulltext-dump-includes-relations
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (doseq [ident ["math/group:Intro"
                     "math/group:Homomorphism"]]
        (core/put-article! store {:article/ident ident}))
      (store/upsert-relation! conn env {:type :link/refers-to
                                        :src {:name "math/group:Intro"}
                                        :dst {:name "math/group:Homomorphism"}
                                        :confidence 0.75})
      (let [{:keys [articles events]} (core/fulltext-dump store)]
        (is (seq events))
        (let [hx (first events)
              end-idents (map (comp :article/ident :article) (:hx/ends hx))]
          (is (= #{"math/group:Homomorphism" "math/group:Intro"}
                 (set (map :article/ident articles))))
          (is (= :link/refers-to (:hx/type hx)))
          (is (= ["math/group:Intro" "math/group:Homomorphism"]
                 end-idents))
          (is (number? (:hx/confidence hx)))))
      (finally
        (rm-rf dir)))))

(deftest links-from-and-to-return-hx-records
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (doseq [ident ["math/group:Intro"
                     "math/group:Target"
                     "physics/optics:Lens"]]
        (core/put-article! store {:article/ident ident}))
      (store/upsert-relation! conn env {:type :link/refers-to
                                        :src {:name "math/group:Intro"}
                                        :dst {:name "math/group:Target"}
                                        :confidence 0.9})
      (store/upsert-relation! conn env {:type :link/mentions
                                        :src {:name "physics/optics:Lens"}
                                        :dst {:name "math/group:Intro"}
                                        :confidence 0.7})
      (let [outgoing (core/links-from store "math/group:Intro")
            incoming (core/links-to store "math/group:Intro")]
        (is (= 1 (count outgoing)))
        (is (= 1 (count incoming)))
        (is (= :link/refers-to (:hx/type (first outgoing))))
        (is (= :link/mentions (:hx/type (first incoming))))
        (is (= "math/group:Intro"
               (-> outgoing first :hx/ends first :article :article/ident)))
        (is (= "math/group:Intro"
               (-> incoming first :hx/ends second :article :article/ident))))
      (finally
        (rm-rf dir)))))

(deftest event-crud-supports-hx-links
  (let [dir (temp-dir)]
    (try
      (let [env {:data-dir (.getAbsolutePath dir)
                 :snapshot-every 100
                 :xtdb {:enabled? false}}
            conn (store/restore! env nil)
            store (compat/alpha-store {:conn conn :env env})
            hx {:hx/type :link/refers-to
                :hx/confidence 0.42
                :hx/ends [{:role :source
                           :article {:article/ident "math/group:Intro"
                                     :article/type [:concept]}}
                          {:role :target
                           :article {:article/ident "math/group:Glossary"}}]}
            hx-id (core/add-event! store hx)]
        (is (uuid? hx-id))
        (is (= 1 (count (core/links-from store "math/group:Intro"))))
        (is (= 1 (count (core/links-to store "math/group:Glossary"))))
        (is (= hx-id (core/delete-event! store hx-id)))
        (is (empty? (core/links-from store "math/group:Intro")))
        (is (empty? (core/links-to store "math/group:Glossary"))))
      (finally
        (rm-rf dir)))))

(deftest add-link-wraps-event-paths
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (core/put-article! store {:article/ident "math/group:Intro"})
      (core/put-article! store {:article/ident "math/group:Target"})
      (let [hx-id (core/add-link! store {:source-ident "math/group:Intro"
                                         :target-ident "math/group:Target"
                                         :types [:link/supports]})
            events-by-type (core/events-by-type store :link/supports)
            events-by-end (core/events-by-end store "math/group:Target")]
        (is (uuid? hx-id))
        (is (= 1 (count events-by-type)))
        (is (= 1 (count events-by-end)))
        (is (= :link/supports (:hx/type (first events-by-type))))
        (is (= "math/group:Target"
               (-> events-by-end first :hx/ends second :article :article/ident))))
      (finally
        (rm-rf dir)))))

(deftest multi-end-events-persist-to-hx-files
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (core/put-article! store {:article/ident "math/group:A"})
      (core/put-article! store {:article/ident "math/group:B"})
      (let [hx {:hx/type :hx/note
                :hx/content {:text "multi"}
                :hx/ends [{:role :author :article {:article/ident "math/group:A"}}
                          {:role :subject :article {:article/ident "math/group:B"}}
                          {:role :topic :article {:article/ident "math/topic"}}]}
            hx-id (core/add-event! store hx)
            events (core/events-by-type store :hx/note)
            files (.listFiles (io/file dir "hx"))]
        (is (uuid? hx-id))
        (is (= 1 (count events)))
        (is (= :hx/note (:hx/type (first events))))
        (is (seq files))
        (is (= hx-id (core/delete-event! store hx-id)))
        (is (empty? (core/events-by-type store :hx/note))))
      (finally
        (rm-rf dir)))))

(deftest plexus-ops-persist-to-profile-files
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        conn (store/restore! env nil)
        store (compat/alpha-store {:conn conn :env env})]
    (try
      (core/put-article! store {:article/ident "math/group:Intro"})
      (core/put-article! store {:article/ident "math/group:Glossary"})
      (let [plexus (core/ensure-plexus! store "beta-team" {:name "Beta Team"
                                                           :config {:scope :beta-only}})]
        (is (= "beta-team" (:plexus/id plexus)))
        (is (= "Beta Team" (:plexus/name plexus)))
        (is (= {:scope :beta-only} (:plexus/config plexus)))
        (core/assign-to-plexus! store "beta-team" "math/group:Intro")
        (core/assign-to-plexus! store "beta-team" "math/group:Glossary")
        (let [members (core/members-in-plexus store "beta-team")
              member-idents (set (map :article/ident members))]
          (is (= #{"math/group:Intro" "math/group:Glossary"} member-idents))))
      (let [meta-file (io/file dir "plexus" "beta-team" "meta.edn")
            members-file (io/file dir "plexus" "beta-team" "members.edn")]
        (is (.exists meta-file))
        (is (.exists members-file)))
      (finally
        (rm-rf dir)))))

(deftest metadata-roundtrip-uses-filesystem
  (let [dir (temp-dir)
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100
             :xtdb {:enabled? false}}
        store (compat/alpha-store {:conn nil :env env})]
    (try
      (is (= {:note "metadata support pending"}
             (core/get-meta store "math/group:Intro")))
      (let [updated (core/update-meta! store "math/group:Intro"
                                       #(assoc % :notes ["hello" "world"]))]
        (is (= {:notes ["hello" "world"]} updated))
        (is (= updated (core/get-meta store "math/group:Intro"))))
      (finally
        (rm-rf dir)))))

(deftest metadata-roundtrip-without-data-dir
  (let [store (compat/alpha-store {:conn nil :env {}})]
    (is (= {:note "metadata support pending"}
           (core/get-meta store "math/group:Intro")))
    (core/update-meta! store "math/group:Intro" #(assoc % :note "beta"))
    (is (= {:note "beta"}
           (core/get-meta store "math/group:Intro")))))

(deftest metadata-prefers-configured-root
  (let [root (temp-dir)
        env {:metadata-root (.getAbsolutePath root)
             :xtdb {:enabled? false}}
        store (compat/alpha-store {:conn nil :env env})
        ident "math/group:Intro"]
    (try
      (core/update-meta! store ident #(assoc % :note "gamma"))
      (let [safe-name (str/replace ident #"/" "__")
            meta-file (io/file root "metadata" (str safe-name ".edn"))]
        (is (.exists meta-file))
        (is (= {:note "gamma"}
               (edn/read-string (slurp meta-file)))))
      (finally
        (rm-rf root)))))
