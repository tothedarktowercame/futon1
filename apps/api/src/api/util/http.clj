(ns api.util.http)

(defn ok-json
  ([body]
   {:status 200
    :body body})
  ([body status]
   {:status status
    :body body}))

(defn ok-text
  ([text]
   (ok-text text 200 {}))
  ([text status headers]
   {:status status
    :headers (merge {"Content-Type" "text/plain; charset=utf-8"} headers)
    :body text}))

(defn ok-edn
  ([value]
   (ok-edn value 200 {}))
  ([value status headers]
   {:status status
    :headers (merge {"Content-Type" "application/edn; charset=utf-8"} headers)
    :body (pr-str value)}))
