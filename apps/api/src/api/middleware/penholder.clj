;; apps/api/src/api/middleware/penholder.clj
(ns api.middleware.penholder
  (:require [api.routes :as routes]
            [clojure.string :as str]
            [reitit.ring :as ring]))

(def ^:private versioned-prefixes
  ["/api/α"
   "/api/%CE%B1"
   "/api/%ce%b1"
   "/api/alpha"])

(defn- normalize-template [path]
  (let [raw (or path "")
        trimmed (if (str/blank? raw) "" raw)
        normalized (reduce (fn [acc prefix]
                             (if (str/starts-with? acc prefix)
                               (str "/api" (subs acc (count prefix)))
                               acc))
                           trimmed
                           versioned-prefixes)]
    (if (str/blank? normalized)
      "/api/unknown"
      normalized)))

(defn- penholder-from-request [request]
  (or
   ;; Explicit header override (for scripts, batch operations, etc.)
   (some-> (get-in request [:headers "x-penholder"]) str/trim not-empty)
   ;; Default: derive from route
   (let [method (some-> (:request-method request) name str/lower-case)
         match (ring/get-match request)
         template (or (:template match) (:path match) (:uri request))
         template' (normalize-template template)]
     (str "api:" method ":" template'))))

(defn wrap-penholder
  "Attach a per-route penholder identity into :ctx/:env."
  [handler]
  (fn [request]
    (let [penholder (penholder-from-request request)]
      (handler (update request :ctx
                       (fn [ctx]
                         (let [ctx (or ctx {})]
                           (update ctx :env #(assoc (or % {}) :penholder penholder)))))))))
