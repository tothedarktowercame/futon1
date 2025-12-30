(ns api.routes
  (:require [api.handlers.diag :as diag]
            [api.handlers.graph :as graph]
            [api.handlers.me :as me]
            [api.handlers.model :as model]
            [api.handlers.slash :as slash]
            [api.handlers.trails :as trails]
            [api.handlers.turns :as turns]
            [api.handlers.types :as types]
            [api.handlers.patterns :as patterns]
            [api.handlers.docbook :as docbook]
            [reitit.ring :as ring]))

(def ^:private shared-routes
  [["/__diag" {:get diag/ctx-snapshot}]
   ["/help" {:get slash/help}]
   ["/tail" {:get slash/tail}]
   ["/ego" {:get slash/ego}]
   ["/ego/:name" {:get slash/ego}]
   ["/cooccur" {:get slash/cooccur}]
   ["/cooccur/:name" {:get slash/cooccur}]
;   ["/forget" {:post slash/forget!}]
   ["/expire" {:post slash/expire!}]
   ["/turns" {:post turns/process-turn-handler}]
   ["/focus-header" {:get turns/current-focus-header-handler}]
   ["/me" {:get me/fetch-handler
           :post me/upsert-handler}]
   ["/me/summary" {:get me/summary-handler}]
   ["/entity" {:post graph/ensure-entity-handler}]
   ["/entity/:id" {:get graph/fetch-entity!}]
   ["/entities/latest" {:get graph/entity-latest!}]
   ["/entities/history/:id" {:get graph/entity-history!}]
   ["/relation" {:post graph/upsert-relation-handler}]
   ["/trails" {:post trails/record-trail-handler
               :get trails/recent-trails-handler}]
   ["/meta/model" {:get model/describe-handler}]
  ["/meta/model/registry" {:get model/registry-handler}]
  ["/meta/model/queue" {:get model/queue-handler}]
   ["/meta/model/verify" {:get model/verify-handler}]
   ["/meta/model/docbook" {:get model/describe-docbook-handler}]
   ["/meta/model/docbook/verify" {:get model/verify-docbook-handler}]
   ["/meta/model/open-world-ingest" {:get model/describe-open-world-handler}]
   ["/meta/model/open-world-ingest/verify" {:get model/verify-open-world-handler}]
   ["/patterns/registry" {:get patterns/registry-handler}]
   ["/types" {:get types/list-types-handler}]
   ["/types/parent" {:post types/set-parent-handler}]
   ["/types/merge" {:post types/merge-aliases-handler}]
   ["/docs/:book/contents" {:get docbook/contents-handler}]
   ["/docs/:book/toc" {:get docbook/toc-handler}]
   ["/docs/:book/contents/order" {:post docbook/update-contents-order-handler}]
   ["/docs/:book/heading/:doc-id" {:get docbook/heading-handler}]
   ["/docs/:book/recent" {:get docbook/recent-handler}]
   ["/docs/:book/doc/:doc-id" {:delete docbook/delete-handler}]
   ["/docs/:book/toc/:doc-id" {:delete docbook/delete-toc-handler}]])

(def ^:private versioned-prefixes
  ["/api/α"
   "/api/%CE%B1"
   "/api/%ce%b1"
   "/api/alpha"])

(def router
  (ring/router
   (into [["/healthz" {:get diag/healthz}]
          ["/api" shared-routes]]
         (map (fn [prefix]
                [prefix shared-routes])
              versioned-prefixes))))

(def dispatch
  (ring/ring-handler
   router
   (ring/create-default-handler)))

;; (defn- dispatch [request]
;;   (let [path (canonical-path (:uri request))
;;         method (:request-method request)]
;;     (case [method path]
;;       [:post "/api/α/turns"] (turn-handler request)
;;       [:get "/api/α/focus-header"] (focus-header-handler request)
;;       [:get "/api/α/me"] (me-get-handler request)
;;       [:post "/api/α/me"] (me-post-handler request)
;;       [:get "/api/α/me/summary"] (me-summary-handler request)
;;       [:post "/api/α/entity"] (entity-handler request)
;;       [:post "/api/α/relation"] (relation-handler request)
;;       [:get "/api/α/types"] (types-handler request)
;;       [:post "/api/α/types/parent"] (types-parent-handler request)
;;       [:post "/api/α/types/merge"] (types-merge-handler request)
;;       [:get "/api/α/help"] (help-handler request)
;;       (not-found request))))
