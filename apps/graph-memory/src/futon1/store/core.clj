(ns futon1.store.core
  "Backend-agnostic protocol for interacting with the beta Arxana store.")

(defprotocol ArxanaStore
  ;; --- Articles -----------------------------------------------------------
  (get-article [store ident]
    "Return the article identified by `ident` (string/keyword) or nil.")
  (put-article! [store article-map]
    "Insert/update an article (primary scholium). Returns the stored map.")
  (delete-article! [store ident]
    "Remove the article and any derived metadata/events scoped to it.")

  ;; --- Metadata -----------------------------------------------------------
  (get-meta [store ident]
    "Fetch the metadata scholium for the given article ident (if present).")
  (update-meta! [store ident f]
    "Apply `f` to the metadata map for `ident`, persisting the result.")

  ;; --- Events / hyperedges ------------------------------------------------
  (add-event! [store hx-map]
    "Insert a hyperedge {:hx/type … :hx/ends […]}. Returns the :hx/id.")
  (delete-event! [store hx-id]
    "Delete the given hyperedge/event.")
  (events-by-end [store entity-ident]
    "Return all events whose :hx/ends include `entity-ident` as an endpoint.")
  (events-by-type [store type-kw]
    "Return all events of the provided :hx/type keyword.")

  ;; --- Link conveniences --------------------------------------------------
  (add-link! [store {:keys [source-ident target-ident types passage labels]}]
    "Convenience wrapper over add-event! for 2-end links.")
  (links-from [store ident]
    "Return events/links originating at the given article ident.")
  (links-to [store ident]
    "Return events/links targeting the given article ident.")

  ;; --- Plexus / collection management ------------------------------------
  (ensure-plexus! [store plexus-id attrs]
    "Create (or update) a plexus record with optional attrs (name/config).")
  (assign-to-plexus! [store plexus-id entity-ident]
    "Associate an article/event with the plexus membership set.")
  (members-in-plexus [store plexus-id]
    "List articles/events that belong to the plexus.")

  ;; --- Search / export ----------------------------------------------------
  (find-articles [store pred]
    "Return articles whose map satisfies pred (Datascript query helper).")
  (fulltext-dump [store]
    "Generate an ordered dump of articles/events for indexing."))
