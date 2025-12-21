(ns futon1.store.datascript
  "Datascript schema helpers for the beta (nema + hyperedge) model."
  (:require [datascript.core :as d]))

(def schema
  {;; --- Articles / primary scholia ---
   :article/ident       {:db/unique :db.unique/identity}
   :article/ns          {}
   :article/name        {}
   :article/text        {}
   :article/about       {}                                   ; legacy projection for Emacs renderers
   :article/type        {:db/cardinality :db.cardinality/many}
   :article/bookkeeping {}
   :article/meta        {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/one}

   ;; --- Metadata scholia ---
   :meta/of             {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/one
                         :db/unique :db.unique/identity}
   :meta/data           {}

   ;; --- Events / hyperedges ---
   :hx/id               {:db/unique :db.unique/identity}
   :hx/type             {}
   :hx/ends             {:db/cardinality :db.cardinality/many}
   :hx/content          {}
   :hx/labels           {:db/cardinality :db.cardinality/many}

   ;; --- Plexus collections ---
   :plexus/id           {:db/unique :db.unique/identity}
   :plexus/name         {}
   :plexus/members      {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/many}
   :plexus/ground       {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/many}
   :plexus/config       {}})

(defn create-conn
  "Return a Datascript connection initialised with the beta schema."
  []
  (d/create-conn schema))
