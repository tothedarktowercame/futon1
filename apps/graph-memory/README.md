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
- `tail` – now hydrates full hyperedge events (type, content, labels, and
  additional ends) so `/tail` in the CLI/API reports rich multi-end relations
  instead of lossy two-end projections.

### Identity vs version nodes

Entities now split into two logical layers:

- **Identity nodes** (`:entity/*` attrs) hold the canonical ID, salience stats,
  external identifier (`:entity/external-id`), and the pointer to the most
  recent version via `:entity/current-version`.
- **Version nodes** (`:entity.version/*` attrs) capture every write as an
  immutable snapshot, chaining to the previous version via
  `:entity.version/prev`. Each version records `:entity.version/data` plus
  `:created-at`/`:updated-at` timestamps.

The write path derives a deterministic UUID from `(source, type, external-id)`
for the identity node, creates a new version node, links it, and updates the
identity’s `:entity/current-version`. Multiple writes with the same external ID
therefore grow a version DAG instead of overwriting state.

`apps/api` exposes the feature through two endpoints:

- `GET /entity/:id` – returns the latest version by default, or a historical
  snapshot when called with `?version=<uuid>` or `?as-of=<millis>`.
- `GET /entities/history/:id` – lists version metadata (newest first) so a
  client can browse the timeline before requesting a specific version.

Underlying helpers live in `app.store` (`fetch-entity`, `entity-history`) and
`app.command-service`, so CLI slash commands and the API share the same logic.
External callers still own their namespace: reusing an external ID appends to
the same version chain, enabling full auditability with no extra schema churn.

### Hyperedge-aware `/tail`

The CLI and API both consume the ArxanaStore abstraction. When the underlying
store exposes true hyperedges (more than two ends, labels, payloads), the
`/tail` command surfaces those attributes directly:

```
> /tail 3
Recent relations:
  - math/group:Intro —supports→ math/group:Glossary
    (seen 2025-01-08T14:05:22.484Z,
     others context=math/topic:Context; labels fresh; content {:text "beta"})
  - me:profile —mentions→ arxana/hyperedges
    (seen 2025-01-08T14:02:11.011Z, labels recap)
```

Even if only one hyperedge matches the criteria, the legacy fallback still
shows the historic two-end projection, so deployments that have not yet
recorded hyperedges see identical output.

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
- **Guaranteed anchors** – entities marked with `:entity/pinned? true` are
  always included in the activation set that feeds focus headers, so even brand
  new profiles (which may have zero relations yet) still emit stable salience
  summaries.

XT mirroring lives in `apps/graph-memory/src/app/store.clj`, alongside the
helpers (`src/app/xt.clj`) that start nodes and submit documents.

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
  [`resources/xtdb-test.edn`](resources/xtdb-test.edn)).

### Demo: salience survives restarts

```bash
# First run – mention Serena twice to boost salience
clojure -M:run-m <<'EOS'
Serena joined PatCon today.
Serena is presenting tomorrow.
EOS

# Observe the second focus header; Serena's seen-count increments

# Restart using the same BASIC_CHAT_DATA_DIR
BASIC_CHAT_DATA_DIR=data clojure -M:run-m <<'EOS'
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
clojure -M:run-m <<'EOS'
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

The tests here cover the seed graph, slash commands, store helpers, and
focus-header builders.

When editing the schema:

1. Update `graph_memory/main.clj` and keep the schema, helper fns, and tests in
   sync.
2. Run `bb lint` (only defined in this app) to enforce `clj-kondo` rules.
3. Re-run the `apps/graph-memory` tests (`clojure -M:test -m cognitect.test-runner`)
   to ensure the front-end CLI still hydrates correctly.

## Known gaps

- XT hydration currently tolerates relations whose endpoints are missing by
  creating stub entities. The focus-header logic still prefers fully-populated
  docs; populate the missing entities to avoid warning logs.
- Demo/client sessions spawn in-process loops and can time out in heavily
  sandboxed environments when XT startup is slow.

## License

Copyright © 2025 Joe

Distributed under the Eclipse Public License version 1.0.
