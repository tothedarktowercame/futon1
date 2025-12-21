# Beta schema

The **beta** schema (“futon1-beta”) generalizes the alpha graph without breaking
existing clients. Alpha-era features—entities, relations, utterances, `/types`
metadata, focus headers—continue to work by translating into the new primitives.
The upgrade packages the Arxana/GravPad model of *nema* (entity-like objects) and
*hyperedge events* inside Datascript/XTDB, so downstream tools can capture rich
annotations, CRDT ops, and provenance while legacy clients keep speaking in
terms of nodes and links.

> **Design rule:** everything is a nema. Articles, events, metadata, and plexus
> collections share the same identity and label machinery, and graph-style links
> become 2-place projections of generic hyperedges.

## 1. Logical model

- **Nema** – the universal record: unique id, label set, optional endpoints, and
  arbitrary payloads.
- **Article (primary scholium)** – a nema labeled `:label/article`; stores the
  textual payload and bookkeeping metadata.
- **Metadata scholium** – derived data keyed 1–1 with an article; keeps
  backlinks, labels, and search indexes in sync.
- **Event (hyperedge / nema)** – a reified relation with type, payload, and
  arbitrary number of endpoints (“ends”). Two-end events look like classic
  links; larger ones encode provenance, CRDT operations, or annotations.
- **Plexus** – a named collection that scopes a working set (similar to alpha’s
  profile graph) and carries configuration hints.

Alpha compatibility:

- `:entity/*` → Article nemas with `:article/ident` derived from the original
  `:entity/id` or `:entity/name`.
- `:relation/*` → `:hx/*` events with exactly two ends (`:role :source`/`:target`).
- The `/types` registry persists and applies to `:article/type` and `:hx/type`
  keywords so alpha slash commands still list the same type vocabulary.

## 2. Datascript schema (`futon1.store.datascript/schema`)

```clojure
{;; --- Articles ---
 :article/ident       {:db/unique :db.unique/identity}
 :article/ns          {}
 :article/name        {}
 :article/text        {}
 :article/about       {} ; legacy projection of link summaries
 :article/type        {:db/cardinality :db.cardinality/many}
 :article/bookkeeping {}
 :article/meta        {:db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one}

 ;; --- Metadata ---
 :meta/of             {:db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/identity}
 :meta/data           {}

 ;; --- Events / Hyperedges ---
 :hx/id               {:db/unique :db.unique/identity}
 :hx/type             {}
 :hx/ends             {:db/cardinality :db.cardinality/many}
 :hx/content          {}
 :hx/labels           {:db/cardinality :db.cardinality/many}

 ;; --- Plexus ---
 :plexus/id           {:db/unique :db.unique/identity}
 :plexus/name         {}
 :plexus/members      {:db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/many}
 :plexus/ground       {:db/valueType :db.type/ref
                       :db/cardinality :db.cardinality/many}
 :plexus/config       {}}
```

`(:hx/ends …)` stores small EDN maps of the shape `{:role kw :id ref
:span [start end] :weight double …}`. Queries unpack them as needed. Nothing in
alpha needs to express hyperedges explicitly; helpers will keep emitting two-end
records.

## 3. XTDB representation

XTDB remains schemaless; docs mirror Datascript attributes verbatim so hydration
is lossless:

```edn
{:xt/id [:article "math/group:Intro"]
 :article/ident "math/group:Intro"
 :article/ns :math/group
 :article/name "Intro"
 :article/text "..."
 :article/type [:type/prose :type/org]
 :article/meta [:meta "math/group:Intro"]}

{:xt/id [:hx #uuid "…"]
 :hx/id #uuid "…"
 :hx/type :link/refers-to
 :hx/ends [{:role :source :id [:article "math/group:Intro"]}
           {:role :target :id [:article "math/group:GroupDef"]}]
 :hx/content {:passage [10 15]}}
```

Multi-end events simply add more maps to `:hx/ends`. XT queries can filter by
`:hx/type`, inspect individual ends, or reconstruct legacy `:relation/*` views.

## 4. Backend-agnostic API (ArxanaStore)

A protocol layers semantic operations over the raw schema so alpha-era callers
(`app.command-service`, slash commands, API handlers) keep their surface area:

- **Articles** – `get-article`, `put-article!`, `delete-article!`
- **Metadata** – `get-meta`, `update-meta!` (derived backlink caches)
- **Events** – `add-event!`, `delete-event!`, `events-by-end`, `events-by-type`
- **Link conveniences** – `add-link!`, `links-from`, `links-to` wrap
  `:hx/type :link/*` helpers for classic two-end relations.
- **Plexus management** – `ensure-plexus!`, `assign-to-plexus!`,
  `members-in-plexus` coordinate collections/config.
- **Search/export** – `find-articles`, `fulltext-dump`

Alpha compatibility lives in these helpers: `add-link!` takes the same payload
as today’s `ensure-entity!`/`upsert-relation!` combo and translates it into an
`add-event!` call. Focus headers can read salience by counting `:hx` events per
article, while `/tail` inspects recent `:link/*` events.

## 5. Compatibility and projections

| Concept | Alpha implementation | Beta equivalent | Notes |
| --- | --- | --- | --- |
| Entities | `:entity/*` | `:article/*` + optional `:article/meta` | Names map to `:article/ident`; `:entity/type` → `:article/type`. |
| Relations | `:relation/*` | `:hx/type :link/*` with two ends | `/relation` command emits `add-link!`; `/tail` reads link events. |
| Backlinks | Stored in `:entity/about` | Projected from `:hx/ends`; optionally mirrored into `:article/about` for Emacs clients. |
| Types | Registry of keywords | Same registry; subject extended to event types if desired. | No breaking change required. |
| Profiles/data roots | `store-manager` profile dirs | `:plexus` entities referencing articles/events | Legacy CLI can pin one plexus per profile. |

### Maintaining alpha functionality

1. **Entity ensure** – create/update an `:article/*` by `:article/ident`; keep
   salience in metadata or derived stats.
2. **Relation upsert** – emit `add-link!` which wraps `add-event!` with two ends
   and optional provenance.
3. **Focus headers** – compute salience from metadata plus recent link events;
   pinned entities become pinned articles.
4. **`/types` command** – still lists registry docs. Beta may introduce an
   `:event`/`:hx` kind later; until then the alpha doc remains accurate.
5. **Hydration/snapshots** – event journal + snapshot continue to persist
   Datascript state; XT mirroring pulls both `:article/*` and `:hx/*` docs.

### Migration posture

- Legacy data can be lazily migrated: when a profile boots under beta, existing
  `:entity/*` and `:relation/*` rows hydrate into the new schema via compatibility
  transactors.
- Backfill `:article/about` from `:hx` events to keep Emacs renderers working.
- Datascript/XT indexes stay small: everything is still an entity doc; hyperedge
  semantics live in query helpers, not separate tables.

## 6. Summary

The beta schema makes “everything a reified event” the new normal while keeping
alpha clients happy:

- Articles and metadata scholia represent the narrative state; plexuses scope
  networks.
- Events/hyperedges unify links, annotations, provenance, CRDT ops, and pattern
  matches.
- Compatibility shims expose the alpha API by translating nodes/links into
  articles + `:hx` events, so `/types`, `/relation`, `/ego`, and focus headers
  operate unchanged.

As new downstream clients arrive (GravPad, Arxana agents), they can consume the
same datastore without additional migrations—hyperedges and plexus metadata give
us the expressive power we lacked in alpha, while alpha remains a valid view of
beta.
