(ns open-world-ingest.trace-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [open-world-ingest.trace :as trace]))

(deftest append-and-load-triples-roundtrip
  (let [tmp (java.io.File/createTempFile "trace-openie" ".edn")
        path (.getAbsolutePath tmp)
        config {:path path}
        triples [{:sent "Alpha" :sent-idx 0 :subj "A" :pred "p" :obj "B" :lemma "p" :spans {}}
                 {:sent "Beta" :sent-idx 1 :subj "C" :pred "p" :obj "D" :lemma "p" :spans {}}]]
    (.delete tmp)
    (trace/reset! config)
    (trace/append! config triples)
    (is (= triples (trace/load-triples config)))
    (io/delete-file path true)))

(deftest index-by-sentence-provides-fallbacks
  (let [triples [{:sent "Alpha" :sent-idx 0}
                 {:sent "Beta" :sent-idx 1}]
        index (trace/index-by-sentence triples)]
    (is (= [(first triples)] (get index [0 "Alpha"])))
    (is (= [(second triples)] (get index [1 :any])))
    (is (= triples (get index [:any :any])))))
