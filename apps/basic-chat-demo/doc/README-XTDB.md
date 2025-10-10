# XTDB quick reference

This note collects a few common XTDB tasks for the basic-chat demo. It assumes
you are running inside `apps/basic-chat-demo/`.

## 1. Start a REPL with the same datastore

```bash
# default interactive runs write XTDB files into data/xtdb/
BASIC_CHAT_DATA_DIR=data/xtdb \
BASIC_CHAT_XTDB_RESOURCE=resources/xtdb-test.edn \
clojure -M:repl
```

At the prompt, require the XT helper and start the node (the store
automatically reuses the configured RocksDB path and keeps logging quiet via the
SLF4J NOP backend):

```clojure
(require '[app.xt :as xt]
         '[xtdb.api :as xta])
(xt/start! (or (System/getenv "BASIC_CHAT_XTDB_RESOURCE") "resources/xtdb-test.edn")
          {:data-dir (System/getenv "BASIC_CHAT_DATA_DIR")})

(def db (xt/db))
```

> **Why `data/xtdb`?** The application stores its Datascript journal in
> `data/`, but it instructs XTDB to keep its RocksDB indices under the nested
> `data/xtdb/` directory. Pointing the REPL at `data` directly will create a
> fresh, empty RocksDB catalog (you will see sibling `doc-store/`, `index-store/`
> and `tx-log/` directories appear). Always target the nested `data/xtdb/`
> directory if you want to inspect data produced by the chat demo.

## 2. Inspect entities mentioning Pat

```clojure
(xta/q db '{:find  [(pull ?e [:entity/id :entity/name :entity/type :entity/seen-count :entity/last-seen :entity/pinned?])]
          :where [[?e :entity/name "Pat"]]})
```

The result is a vector containing an entity map. You can transform the pull to
see linked relations:

```clojure
(xta/q db '{:find  [(pull ?r [:relation/id :relation/type :relation/confidence :relation/last-seen
                             {:relation/src [:entity/name]}
                             {:relation/dst [:entity/name]}])]
          :where [[?r :relation/type :links-to]]})
```

The `:relation/src` and `:relation/dst` attributes store the entity UUIDs that
serve as foreign keys. Pull expressions can follow those UUIDs to the
underlying entity documents, or you can join manually if you prefer a tailored
projection:

```clojure
(xta/q db '{:find  [?src-name ?dst-name ?confidence]
          :where [[?r :relation/type :links-to]
                  [?r :relation/src ?src]
                  [?r :relation/dst ?dst]
                  [?r :relation/confidence ?confidence]
                  [?src :entity/name ?src-name]
                  [?dst :entity/name ?dst-name]]})
```

## 3. Count all stored entities or relations

```clojure
(xta/q db '{:find  [(count ?e)]
          :where [[?e :entity/id _]]})

(xta/q db '{:find  [(count ?r)]
          :where [[?r :relation/id _]]})
```

To get both counts at once:

```clojure
(xta/q db '{:find  [(count ?e) (count ?r)]
          :where [[?e :entity/id _]
                  [?r :relation/id _]]})
```

## 4. Shut the node down

Always close the node when the session ends:

```clojure
(xta/stop!)
```

This keeps RocksDB from holding open file locks between runs.

## 5. Salience heuristics at a glance

The focus header and slash commands lean on XTDB to rank entities and
relations. Two core scoring functions live in `app.focus`:

* **Entity salience (`focus-candidates`)** – every entity pulled from XTDB is
  scored using recency, frequency, and semantic type information:
  * recency contributes up to `1.5` points, tapering linearly across the focus
    window (default 30 days)
  * mention frequency adds `0.8 * log1p(seen-count)` so repeated mentions help
    but with diminishing returns
  * entity types carry weights (`:person` → 1.0, `:org` → 0.8, `:project` → 0.7,
    `:place` → 0.6, everything else → 0.5) that are multiplied by `1.2`
  * anchors earn a flat `+3` and pinned entities earn `+4`, guaranteeing they
    appear even when they are old
* **Neighbor salience (`top-neighbors`)** – relation edges are scored as
  `confidence + 1.2 * recency`. Recent, high-confidence edges surface first,
  and XTDB lookups hydrate the neighbor entity documents so their names and
  types can be rendered in the focus header.

Because both heuristics read directly from XTDB, you will see stale focus data
if the node is not running.

## 6. Troubleshooting empty query results

If the examples above return `{}`:

1. Ensure you have run a chat session that created entities or relations.
2. Confirm the REPL is pointed at the same XTDB catalog as the app (the default
   is `<project>/apps/basic-chat-demo/data/xtdb`). Accidentally using `data/`
   without the `xtdb/` suffix spins up a brand-new catalog.
3. Check that `xt/started?` returns `true`; if not, restart the node with the
   steps in section 1.
