(ns api.middleware.qparams)

(defn- my-parse-long [s]
  (try (Long/parseLong (str s)) (catch Throwable _ nil)))

(defn- clamp [x lo hi] (-> x (max lo) (min hi)))

(defn wrap-query-ctx
  "Copy common query params into :ctx:
   - ?limit=     -> :limit    (1..50)
   - ?profile=   -> :profile
   - ?window-days= -> :time-hint (now - days)
   Only sets keys when params are present (won't clobber existing ctx)."
  [handler]
  (fn [req]
    (let [qps        (:query-params req)            ;; string keys
          now        (get-in req [:ctx :now])
          ;; parse individual params (only set when present)
          q-limit    (some-> (get qps "limit") my-parse-long (clamp 1 50))
          q-profile  (some-> (get qps "profile") str)
          q-win-days (some-> (get qps "window-days") my-parse-long)
          time-hint  (when (and now q-win-days)
                       (- now (* q-win-days 24 60 60 1000)))
          ctx-add    (cond-> {}
                       q-limit   (assoc :limit q-limit)
                       q-profile (assoc :profile q-profile)
                       time-hint (assoc :time-hint time-hint))]
      (handler (if (seq ctx-add)
                 (update req :ctx merge ctx-add)
                 req)))))
