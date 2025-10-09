(ns open-world-ingest.util
  (:require [clojure.string :as str])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.time Instant)))

(defn sha1
  ^String
  [^String s]
  (let [digest (MessageDigest/getInstance "SHA-1")]
    (.update digest (.getBytes s StandardCharsets/UTF_8))
    (format "%040x" (BigInteger. 1 (.digest digest)))))

(defn canonical-kind
  [kind]
  (cond
    (keyword? kind) kind
    (string? kind)
    (let [normalized (-> kind
                         str/lower-case
                         (str/replace #"[^a-z0-9]+" "-")
                         (str/replace #"^-+" "")
                         (str/replace #"-+$" ""))]
      (if (str/blank? normalized)
        :proper
        (keyword normalized)))
    :else :proper))

(defn now
  ^Instant []
  (Instant/now))

(defn timestamp-ms
  [^Instant inst]
  (.toEpochMilli inst))
