# graph-memory

`graph-memory` holds the shared Datascript schema and helpers that back the
`basic-chat` demos. The module balances two kinds of storage:

- **Datascript** – an in-memory, immutable graph that makes per-turn salience
  queries cheap (low-latency focus headers, neighbour lookups, REPL ergonomics).
- **XTDB** – a persistent, bitemporal document store that excels at keeping
  time-associated metadata such as `:entity/last-seen`, `:entity/seen-count`,
  and `:relation/last-seen` across restarts.

At runtime we write to both: Datascript powers the live conversation, while XTDB
records every entity/relation so the next boot can restore salience context
without replaying the entire event log.

## Schema highlights

The schema lives in [`src/graph_memory/main.clj`](src/graph_memory/main.clj).
Key entity types:

- `:entity/*` – canonical entities tracked across conversations, with salience
  metadata (`:entity/last-seen`, `:entity/seen-count`, `:entity/pinned?`).
- `:relation/*` – directed edges between entities, optionally carrying
  `:relation/provenance`, `:relation/confidence`, and `:relation/last-seen`.
- `:utterance/*`, `:mention/*`, `:link/*` – artefacts produced by the NLP layer
  to document utterances, mentions, and heuristic co-occurrence links.

The namespace also exposes convenience fns used throughout the demos:

- `init-db` / `seed!` – build an empty Datascript connection or load the seed
  sample graph.
- `ensure-entity!`, `add-relation!`, `link!` – simple helpers for REPL/manual
  curation.
- `entities-by-name`, `neighbors` – query utilities for the CLI and tests.

## Why XTDB?

XTDB gives us:

- **Persistence without migration pain** – documents can evolve independently
  and XT handles schema-less storage.
- **Bitemporal history** – we can reason about “last seen” and confidence over
  time, and (in the future) query past valid-times.
- **Crash resilience** – the CLI can restart and immediately reacquire salience
  metadata, which keeps focus headers stable.

We retain Datascript because:

- **Speed** – Datascript queries are in-process and tuned for the most common
  lookups (`entities-by-name`, `neighbors`).
- **Ease of augmentation** – bang commands and REPL tooling can mutate the
  in-memory store without round-tripping through XT.

XT mirroring lives in `apps/basic-chat-demo/src/app/store.clj`, but this module
exposes the helpers (`graph-memory/src/app/xt.clj`) that start nodes and submit
documents.

## XT integration helpers

The application-level XT node management lives in
[`src/app/xt.clj`](src/app/xt.clj). `graph-memory` depends on it to:

- Start/stop an embedded XTDB node based on a config EDN.
- Submit entity/relation documents (`put-entity!`, `put-rel!`).
- Run XT queries (`q`, `entity`, `db`) from the focus-header pipeline.

Two classpath configs are provided:

- [`resources/xtdb.edn`](resources/xtdb.edn) – default RocksDB-backed config for
  long-lived runs.
- Tests typically point to a temp copy so each run gets fresh storage (see
  `apps/basic-chat-demo/resources/xtdb-test.edn`).

### Demo: salience survives restarts

```bash
# First run – mention Serena twice to boost salience
clojure -M:run-m -- --protocol basic-chat/v5 --fh-only <<'EOS'
Serena joined PatCon today.
Serena is presenting tomorrow.
EOS

# Observe the second focus header; Serena's seen-count increments

# Restart using the same BASIC_CHAT_DATA_DIR
BASIC_CHAT_DATA_DIR=data clojure -M:run-m -- --protocol basic-chat/v5 --fh-only <<'EOS'
Show me the most relevant entities.
EOS

# The focus header still lists Serena with the preserved seen-count/last-seen,
# even though Datascript was freshly initialised.

> **RocksDB note:** XTDB uses RocksDB under the hood. Each server process needs
> its own `BASIC_CHAT_DATA_DIR`; running multiple JVMs against the same data
> directory will cause the second startup to fail with a `LOCK`/`BindException`.
```

### Demo: fast local edits

```bash
clojure -M:run-m -- --protocol basic-chat/v5 --fh --fh-only <<'EOS'
!entity Futon v5 :project
Futon v5 shipped focus headers.
EOS

# The bang command immediately updates Datascript, so the very next focus header
# reflects the new project anchor without waiting for XT round-trips.
```

## Development workflow

From the project root you can run the small Datascript regression suite:

```bash
cd apps/graph-memory
clojure -M:test -m clojure.test
```

The tests only cover the seed graph today; higher-level integration checks live
under `apps/basic-chat-demo/test`.

When editing the schema:

1. Update `graph_memory/main.clj` and keep the schema, helper fns, and tests in
   sync.
2. Run `bb lint` (only defined in this app) to enforce `clj-kondo` rules.
3. Re-run the `basic-chat-demo` tests (`clojure -M:test -m cognitect.test-runner`)
   to ensure the front-end CLI still hydrates correctly.

## Known gaps

- XT hydration currently tolerates relations whose endpoints are missing by
  creating stub entities. The focus-header logic still prefers fully-populated
  docs; populate the missing entities to avoid warning logs.
- Golden tests in `basic-chat-demo` spawn separate JVMs and can time out in
  heavily sandboxed environments when XT startup is slow.

## License

Copyright © 2025 Joe

Distributed under the Eclipse Public License version 1.0.
