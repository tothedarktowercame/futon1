(ns basic-chat-demo.basic-chat-demo-test
  (:require [clojure.test :refer [deftest is testing]]
            [basic-chat-demo.basic-chat-demo :as sut]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.io :as io])) ; system under test

(defn run-script
  ([script-path]
   (run-script "basic-chat/v1" script-path))
  ([protocol script-path]
   (run-script protocol script-path []))
  ([protocol script-path extra-cli]
   (let [cmd (concat ["clojure" "-M:run-m" "--"
                      "--protocol" protocol
                      "--script" script-path]
                     extra-cli)
         {:keys [out err exit]} (apply sh/sh cmd)]
    (is (zero? exit) (str "non-zero exit: " err))
    (edn/read-string out))))

(defn round2 [n]
  (when (number? n)
    (/ (Math/round (* n 100.0)) 100.0)))

(deftest hello-script
  (let [got (run-script "test/scripts/hello.edn")
        ;; normalize: replace unpredictable bits in :links with {}
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context)
                         (update :links (fn [ls] (mapv (constantly {}) ls)))))
                   got)
        exp (-> "test/golden/hello.out.edn" slurp edn/read-string)]
    (is (= exp got'))))

(deftest v2-script
  (let [got (mapv #(dissoc % :context)
                   (run-script "basic-chat/v2" "test/scripts/v2-basic.edn"))
        exp (-> "test/golden/v2-basic.out.edn" slurp edn/read-string)]
    (is (= exp got))))

(deftest v3-script
  (let [got (run-script "basic-chat/v3" "test/scripts/basic-chat/v3/entities.edn")
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context)
                         (update :entities (fn [ents]
                                             (mapv #(select-keys % [:name :type]) ents)))
                         (update :relations (fn [rels]
                                              (mapv #(select-keys % [:type]) rels)))))
                   got)
        exp (-> "test/golden/basic-chat/v3/entities.out.edn" slurp edn/read-string)]
    (is (= exp got'))))

(deftest v4-script
  (let [got (run-script "basic-chat/v4" "test/scripts/basic-chat/v4/entities.edn" ["--ner-fallback"])
        got' (mapv (fn [m]
                     (-> m
                         (dissoc :context)
                         (update :intent #(select-keys % [:type :conf]))
                         (update :links (fn [links]
                                          (mapv (constantly {}) links)))
                         (update :entities (fn [ents]
                                             (mapv (fn [ent]
                                                     (-> ent
                                                         (select-keys [:name :type :source :confidence :id :span :aliases :value])
                                                         (update :confidence round2)
                                                         (update :aliases #(vec %))))
                                                   ents)))
                         (update :relations (fn [rels]
                                              (mapv #(select-keys % [:type :src :dst]) rels)))))
                    got)
        exp (-> "test/golden/basic-chat/v4/entities.out.edn" slurp edn/read-string)]
    (is (= exp got'))))
