# futon1

futon1 is a collection of deterministic chat demos that showcase a tiered NLP
stack, an in-memory knowledge graph, and an append-only persistence layer. The
current interactive entry point is the `apps/demo` wrapper around the
`apps/client` session runner. Futon1’s mandate is primarily **storage and
durability**—it keeps the fact graph coherent and exposes a minimal HTTP/CLI
surface so higher futons (via Futon3’s interface layer) can rely on a stable
data store without embedding UI or orchestration logic here.

## Repository structure

- `apps/demo` – user-facing CLI that wraps the Clojure client session with a
  minimal REPL-like interface.
- `apps/client` – deterministic session runner that drives the ingest pipeline,
  slash/bang commands, and focus-header rendering.
- `apps/nlp-interface` – deterministic NER/POS pipeline implementations shared
  across protocols (including the v4 gazetteer + pattern recogniser).
- `apps/graph-memory` – Datascript/XTDB schema, store manager, focus-header
  helpers, and persistence utilities reused by every frontend.
- `apps/open-world-ingest` – standalone CLI for streaming arbitrary utterances
  into XTDB using the open-domain CoreNLP pipeline.

## Core features

- **Protocol variants**: the legacy `basic-chat` flows have been retired from
  active builds. The CLI now always drives the shared ingest pipeline described
  below; see `HISTORY.md` if you need the historical notes for those versions.
- **Persistence**: the Datascript cache mirrors every mutation into XTDB. On
  boot the session hydrates from XT so salience metadata (seen-counts,
  last-seen timestamps, pinned flags) is ready before focus headers are
  generated; if XT is disabled it falls back to the legacy event log + snapshot.
- **Inline graph editing**: use bang commands in interactive mode (e.g.
  `!entity Pat :person`, `!rel "Pat" advisor-of "Joe" since 2001 ...`) to record
  curated facts; these are persisted through the same append-only log.
- **Graph context**: sessions emit top-k neighbours plus the JSON **focus
  header** consumed by the API and demo client. Neighbor/focus heuristics live
  in `app.focus*` and are shared across transports.
- **Open-world ingest**: the `apps/open-world-ingest` CLI bootstraps a
  CoreNLP-backed pipeline that stores entities, mentions, and relations in XTDB
  from arbitrary text (see module README for command reference and XT
  configuration knobs).

## Limitations

- Pronouns such as “I” and “you” are intentionally ignored by
  the deterministic NER layers. They will not create or update entities unless a
  future personification pass resolves them to concrete participants.
- OpenIE now derives typed relation keywords (e.g. `:works-at`, or
  `:sister/assure` when the object offers a natural namespace) and registers
  them in the relation type registry; if the predicate cannot be resolved, the
  ingestion pipeline still falls back to the generic `:links-to` edge.

## Getting started

```bash
cd apps/demo
clojure -M:run-m
```

You can also start the demo from the repository root via `clojure -M:run-m` or
`bb demo`. Slash (`/tail`, `/me`, …) and bang (`!entity`, `!rel`, …) commands
work exactly as they did in the legacy CLI, and focus-header summaries stream to
stdout alongside chat replies.

### Running the HTTP API server

External clients should talk to the API module rather than the demo CLI. From
the repository root run `clojure -M:api`, or from within `apps/api` launch the
module-specific alias:

```bash
cd apps/api
clojure -M:server
```

Set `BASIC_CHAT_DATA_DIR` (and optional `BASIC_CHAT_XTDB_*` overrides) before
starting the server so it points at the storage root your client should use.
Detailed route and environment docs live in `apps/api/README.md`.

### Operator guides

- `README-archivist.md` covers client usage patterns and the model registry.
- `README-storage.md` documents hydration and XTDB internals.

### Installing Clojure and Java dependencies

The demos require a working JVM and the Clojure CLI tools. On Debian/Ubuntu
hosts you can provision everything with the helper script in this repository:

```bash
sudo ./scripts/install-clojure-env.sh
clojure -Sdescribe  # verify the CLI installation
```

The script installs OpenJDK 21, `rlwrap`, and the official Clojure CLI release.
Run it whenever you need to bootstrap a fresh environment.

Flags mirror the features above; pass `-- --help` (invalid option) to see usage
from the CLI. Add `--fh` when you need a machine-readable focus header (or
`--fh-only` to suppress the normal EDN output).


## Persistence layout

All persisted data lives under the repo-level `data/` directory by default.
Override the root via `config.edn` or `BASIC_CHAT_DATA_DIR` to keep per-profile
state outside the workspace:

- `events.ndjson` / `snapshot.edn` – legacy append-only log + snapshot used when
  XTDB is disabled.
- `xtdb/` – RocksDB directories created when XT mirroring is active (paths set
  via `BASIC_CHAT_DATA_DIR`).

On boot the CLI hydrates from XT first; if no XT data exists it replays the
snapshot + events.

## Configuration

A repository-level `config.edn` file controls the default data directory,
snapshot cadence, and XTDB settings that the demos use on startup. The values
are loaded by `apps/common/src/app/config.clj` and feed into
`app.store-manager/start!`. Override them by editing `config.edn` directly or by
setting the documented environment variables (see below). When you run multiple
instances side-by-side it’s usually easiest to point `config.edn` at a different
`data-root` before launching the CLI.

### Environment overrides

Set these when running multiple instances in parallel (tests, tooling, CLI):

- `BASIC_CHAT_DATA_DIR` – root directory for snapshots, events, and XT storage.
- `BASIC_CHAT_XTDB_RESOURCE` – classpath resource for XT config (defaults to
  `xtdb.edn`).
- `BASIC_CHAT_XTDB_ENABLED` – disable XT hydration/mirroring when set to a
  falsy string (`false`, `0`, `off`, `no`).

## Baseline snapshot and determinism

`docs/baseline.md` describes the canonical ingest→NLP→graph-memory→XTDB pipeline and references
`resources/baseline/demo_session.edn`, the Willie/Jane transcript used by tests. Regenerate the
fixture via `clojure -M:baseline/snapshot --write resources/baseline/demo_session.edn`. The
`apps/client/test/client/session_test.clj` suite compares focus headers plus Datascript/XTDB
summaries against this snapshot, keeping FUTON1 prototypes 0–1 in a DONE state. The
`Futon1 Deterministic Stack` GitHub Action (`.github/workflows/futon1-stack-tests.yml`) runs the
client determinism test, the `apps/graph-memory` regression suite, and the
`apps/nlp-interface` pipeline tests on every push/PR to `main`, so broken storage/NLP behaviour is
caught before downstream futons rely on it.

## Testing

Each app provides a Test Runner. The graph/persistence regressions now live
under `apps/graph-memory/test`, alongside the NLP fixtures in
`apps/nlp-interface/test`.

```bash
cd apps/graph-memory
clojure -M:test -m cognitect.test-runner

cd ../nlp-interface
clojure -M:test -m cognitect.test-runner
```

Golden fixtures (EDN) capture expected protocol output. Update them by rerunning
the matching scripts and copying the printed vector when intentional changes are
made.

> **Note:** End-to-end demos spawn in-process sessions that can take longer than
> the default sandbox timeout. Retry outside the sandbox (or with a larger
> timeout) if you encounter this warning.

For additional operational caveats see [LIMITATIONS.md](LIMITATIONS.md).

## Relations

- `:links-to` – default co-occurrence relation emitted by the deterministic
  pipeline.
- Custom relations (`:advisor-of`, `:supersedes`, etc.) are only created by
  inline `!rel` commands or bespoke rules.

## CLI reference

- `!entity <name> [:type]` – ensure an entity exists and record it in the event
  log.
- `!rel <src> <type> <dst> [since <val> until <val> note <text>]` –
  create/update a relation between entities.
- `!links <name>` – print the neighbours recorded for the named entity.
- `/tail [n]`, `/ego <entity>`, `/me [summary] [limit]`, `/types`, etc. – see
  `app.slash` for the full reference shared by the CLI and HTTP API.
