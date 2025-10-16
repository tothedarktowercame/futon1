(ns v4-demo-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [app.context :as context]
            [app.store :as store]
            [app.store-manager :as store-manager]
            [basic-chat-demo.basic-chat-demo :as sut]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [nlp-interface.nlp-interface :as nlp]))

(defn- temp-dir-file []
  (-> (java.nio.file.Files/createTempDirectory "v4-demo-test" (make-array java.nio.file.attribute.FileAttribute 0))
      (.toFile)))

(defn- delete-recursively [f]
  (when (.exists f)
    (doseq [file (reverse (file-seq f))]
      (io/delete-file file true))))

(use-fixtures
  :each
  (fn [f]
    (let [tmp (temp-dir-file)]
      (store-manager/configure! {:data-root (.getAbsolutePath tmp)
                                 :xtdb {:enabled? false}})
      (try
        (f)
        (finally
          (store-manager/shutdown!)
          (delete-recursively tmp))))))

(deftest v4-demo-roundtrip
  (fn [conn]
    (println "[debug - v4-demo-roundtrip] initial entities:" (d/q '[:find ?name :where [_  :entity/name ?name]] @conn))
    (let [env (store-manager/env nil)
          now (System/currentTimeMillis)]
        ;; utterance creates two entities via NER
      (let [res (nlp/handle-input-v4 conn "I am looking forward to meeting Pat in Boston." now {:enable-fallback? true})]
        (doseq [{:keys [name type]} (:entities res)]
          (when name
            (store/ensure-entity! conn env {:name name :type type}))))
        ;; relation upsert with provenance
      (store/upsert-relation! conn env {:type :advisor-of
                                        :src {:name "Pat" :type :person}
                                        :dst {:name "Joe" :type :person}
                                        :provenance {:since "2001"
                                                     :until "2005"
                                                     :note "former undergrad advisor"}})
        ;; version context setup
      (store/ensure-entity! conn env {:name "v3" :type :version})
      (store/ensure-entity! conn env {:name "v4" :type :version})
      (store/upsert-relation! conn env {:type :supersedes
                                        :src {:name "v4" :type :version}
                                        :dst {:name "v3" :type :version}})
      (let [res (nlp/handle-input-v4 conn "v4 is getting there." (+ now 1000) {:enable-fallback? true})]
        (doseq [{:keys [name type]} (:entities res)]
          (when name
            (store/ensure-entity! conn env {:name name :type type}))))
        ;; reboot to exercise persistence
      (let [conn' (store/restore! env)]
        (testing "entities survive reload"
          (is (store/resolve-name->eid conn' "Pat"))
          (is (store/resolve-name->eid conn' "Boston")))
        (testing "relation provenance stored"
          (let [prov (ffirst
                      (d/q '[:find ?prov
                             :where
                             [?r :relation/type :advisor-of]
                             [?r :relation/provenance ?prov]
                             [?r :relation/src ?src]
                             [?src :entity/name "Pat"]
                             [?r :relation/dst ?dst]
                             [?dst :entity/name "Joe"]]
                           @conn'))]
            (is (= {:since "2001" :until "2005" :note "former undergrad advisor"}
                   prov))))
        (testing "context shows top neighbor"
          (let [v4 (store/resolve-name->eid conn' "v4")
                ctx (context/enrich-with-neighbors conn' [{:name "v4" :entity-id (:id v4)}]
                                                   {:neighbors 1 :context-cap 5})]
            (is (= 1 (count ctx)))
            (is (= {:entity "v4"
                    :entity-id (:id v4)
                    :neighbor "v3"
                    :relation :supersedes
                    :direction :out}
                   (select-keys (first ctx) [:entity :entity-id :neighbor :relation :direction])))))))))

(deftest v4-ner-builds-dynamic-catalog
  (fn [conn]
    (println "[debug - v4-ner-builds-dynamic-catalog] initial entities:" (d/q '[:find ?name :where [_  :entity/name ?name]] @conn))
    (let [env (store-manager/env nil)
          base (System/currentTimeMillis)]
      (testing "initial mentions create entities"
        (let [res (nlp/handle-input-v4 conn "Charlotte and I went out to eat Turkish food." base {:enable-fallback? true})
              charlie (some #(when (= "Charlotte" (:name %)) %) (:entities res))
              turkish (some #(when (= "Turkish" (:name %)) %) (:entities res))]
          (is charlie)
          (is (= :person (:type charlie)))
          (is (= :pos (:source charlie)))
          (is turkish)
          (is (= :place (:type turkish)))
          (doseq [{:keys [name type]} (:entities res)]
            (when name
              (store/ensure-entity! conn env {:name name :type type}))))
        (is (store/resolve-name->eid conn "Charlotte"))
        (testing "catalog supplies previously mentioned people"
          (let [res (nlp/handle-input-v4 conn "Charlotte hasn't met Pat." (+ base 1000) {:enable-fallback? true})]
            (doseq [{:keys [name type]} (:entities res)]
              (when name
                (store/ensure-entity! conn env {:name name :type type})))
            (let [charlie (some #(when (= "Charlotte" (:name %)) %) (:entities res))
                  pat (some #(when (= "Pat" (:name %)) %) (:entities res))]
              (is charlie)
              (is (= :catalog (:source charlie)))
              (is (= :person (:type charlie)))
              (is pat)
              (is (= :person (:type pat)))
              (is (= :pos (:source pat))))
            (is (store/resolve-name->eid conn "Charlotte"))
            (is (store/resolve-name->eid conn "Pat"))))
        (testing "places are inferred and reused"
          (let [res (nlp/handle-input-v4 conn "I am in Arlington." (+ base 2000) {:enable-fallback? true})]
            (doseq [{:keys [name type]} (:entities res)]
              (when name
                (store/ensure-entity! conn env {:name name :type type})))
            (let [arl (some #(when (= "Arlington" (:name %)) %) (:entities res))]
              (is arl)
              (is (= :place (:type arl)))))
          (let [res (nlp/handle-input-v4 conn "Arlington is North of Boston." (+ base 3000) {:enable-fallback? true})]
            (let [arl (some #(when (= "Arlington" (:name %)) %) (:entities res))]
              (is arl)
              (is (= :catalog (:source arl)))
              (is (= :place (:type arl))))
            (is (store/resolve-name->eid conn "Arlington"))))))))
