(ns futon1.id
  "Utilities for canonicalizing entity identifiers so callers can supply either UUIDs or external IDs."
  (:require [clojure.string :as str])
  (:import (java.nio ByteBuffer)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util Arrays UUID)))

(def ^:private uuid-pattern
  #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}")

(defn uuid-string?
  [s]
  (boolean (some->> s (re-matches uuid-pattern))))

(defn- sha1-bytes [s]
  (let [digest (MessageDigest/getInstance "SHA-1")]
    (.update digest (.getBytes s StandardCharsets/UTF_8))
    (.digest digest)))

(def ^:private version-mask (unchecked-long 0xffffffffffff0fffN))
(def ^:private version-flag 0x0000000000004000)
(def ^:private variant-mask 0x3fffffffffffffff)
(def ^:private variant-flag (unchecked-long 0x8000000000000000N))

(defn- uuid-from-sha1
  "Turn a SHA-1 digest into a pseudo type-4 UUID by truncating to 128 bits."
  [^bytes digest]
  (let [bytes (Arrays/copyOf digest 16)
        buffer (ByteBuffer/wrap bytes)
        msb (.getLong buffer)
        lsb (.getLong buffer)
        msb' (bit-or (bit-and msb version-mask) version-flag)
        lsb' (bit-or (bit-and lsb variant-mask) variant-flag)]
    (UUID. msb' lsb')))

(defn- clean-source [source]
  (let [value (cond
                (keyword? source) (name source)
                (string? source) source
                :else nil)
        trimmed (some-> value str/trim)]
    (when (seq trimmed) trimmed)))

(defn- clean-id [id]
  (some-> id str str/trim not-empty))

(defn coerce-id
  "Return a canonical {:entity/id uuid ...} map for the provided payload.

  When :id is nil, generates a random UUID.
  When :id is a UUID string, returns it directly.
  Otherwise derive a deterministic UUID using source/type/external id."
  [{:keys [id external-id type external-source]}]
  (let [raw-id (clean-id id)
        raw-external (clean-id external-id)
        src (or (clean-source external-source) "external")
        type-name (let [value (cond
                                (keyword? type) (str type)
                                (string? type) type
                                (some? type) (str type)
                                :else nil)]
                    (or (clean-id value) "entity"))
        basis (or raw-id raw-external)]
    (cond
      (nil? basis)
      {:entity/id (UUID/randomUUID)}

      (uuid-string? basis)
      {:entity/id (UUID/fromString basis)}

      :else
      (let [uuid (uuid-from-sha1 (sha1-bytes (str src ":" type-name ":" basis)))
            external (or raw-external basis)]
        {:entity/id uuid
         :entity/external-id external
         :entity/source src}))))
