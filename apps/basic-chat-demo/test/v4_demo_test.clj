(ns v4-demo-test
  (:require [clojure.test :refer [deftest is testing]]
            [app.context :as context]
            [app.store :as store]
            [basic-chat-demo.basic-chat-demo :as sut]
            [clojure.java.io :as io]
            [datascript.core :as d]
            [nlp-interface.nlp-interface :as nlp]))

(def default-env {:data-dir "data" :snapshot-every 100})

(defn with-temp-store [f]
  (let [dir (doto (java.io.File. (System/getProperty "java.io.tmpdir")
                                  (str (gensym "v4-demo-")))
              (.mkdirs))
        env {:data-dir (.getAbsolutePath dir)
             :snapshot-every 100}]
    (try
      (reset! sut/!env env)
      (sut/boot!)
      (f)
      (finally
        (reset! sut/!env default-env)
        (sut/boot!)
        (when (.exists dir)
          (io/delete-file dir true))))))

(deftest v4-demo-roundtrip
  (with-temp-store
    (fn []
      (let [conn @sut/!conn
            now (System/currentTimeMillis)]
        ;; utterance creates two entities via NER
        (let [res (nlp/handle-input-v4 conn "I am looking forward to meeting Pat in Boston." now {:enable-fallback? true})]
          (doseq [{:keys [name type]} (:entities res)]
            (when name
              (store/ensure-entity! conn @sut/!env {:name name :type type}))))
        ;; relation upsert with provenance
        (store/upsert-relation! conn @sut/!env {:type :advisor-of
                                               :src {:name "Pat" :type :person}
                                               :dst {:name "Joe" :type :person}
                                               :provenance {:since "2001"
                                                            :until "2005"
                                                            :note "former undergrad advisor"}})
        ;; version context setup
        (store/ensure-entity! conn @sut/!env {:name "v3" :type :version})
        (store/ensure-entity! conn @sut/!env {:name "v4" :type :version})
        (store/upsert-relation! conn @sut/!env {:type :supersedes
                                               :src {:name "v4" :type :version}
                                               :dst {:name "v3" :type :version}})
        (let [res (nlp/handle-input-v4 conn "v4 is getting there." (+ now 1000) {:enable-fallback? true})]
          (doseq [{:keys [name type]} (:entities res)]
            (when name
              (store/ensure-entity! conn @sut/!env {:name name :type type}))))
        ;; reboot to exercise persistence
        (sut/boot!)
        (let [conn' @sut/!conn]
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
                     (select-keys (first ctx) [:entity :entity-id :neighbor :relation :direction]))))))))))
