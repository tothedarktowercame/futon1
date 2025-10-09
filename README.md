# futon1

futon1 is a collection of deterministic chat demos that showcase a tiered NLP
stack, an in-memory knowledge graph, and an append-only persistence layer. Each
demo is surfaced through the `apps/basic-chat-demo` CLI and can be driven via
interactive input, scripted conversations, or golden tests.

## Repository structure

- `apps/basic-chat-demo` – user-facing CLI with protocol selection, inline
  commands, persistence bootstrapping, and context rendering.
- `apps/nlp-interface` – deterministic NER/POS pipeline implementations shared
  across protocols (including the v4 gazetteer + pattern recogniser).
- `apps/graph-memory` – Datascript schema/helpers for the entity/relationship
  store.
- `apps/open-world-ingest` – standalone CLI for streaming arbitrary utterances
  into XTDB using the open-domain CoreNLP pipeline.

## Core features

- **Protocol variants**: choose between `basic-chat/v1` .. `basic-chat/v5` using
  `--protocol basic-chat/vN`. `basic-chat/v5` is the default focus-header flow,
  layering XT persistence on top of the v4 NER, relation extraction, and
  context-aware output.
- **Persistence**: the Datascript cache mirrors every mutation into XTDB. On
  boot the CLI first hydrates from XT so salience metadata (seen-counts,
  last-seen timestamps, pinned flags) is ready before focus headers are
  generated; if XT is disabled it falls back to the legacy event log + snapshot.
- **Inline graph editing**: use bang commands in interactive mode (e.g.
  `!entity Pat :person`, `!rel "Pat" advisor-of "Joe" since 2001 ...`) to record
  curated facts; these are persisted through the same append-only log.
- **Graph context**: v4 and later protocols can emit top-k neighbours and the
  JSON **focus header** consumed by the v5 agent flows. Tweak with `--context`,
  `--neighbors`, `--context-cap`, `--fh`, `--fh-only`, `--focus-days`, and
  `--allow-works`.
- **Operational knobs**: `--reset` wipes the data dir, `--compact` forces a
  fresh snapshot, and `--export edn` emits a serialisable view of the current
  database.
- **Open-world ingest**: the `apps/open-world-ingest` CLI bootstraps a
  CoreNLP-backed pipeline that stores entities, mentions, and relations in XTDB
  from arbitrary text (see module README for command reference and XT
  configuration knobs).

## Limitations

- Pronouns such as “I” and “you” are intentionally ignored by
  the deterministic NER layers. They will not create or update entities unless a
  future personification pass resolves them to concrete participants.
- Only the generic `:links-to` relation is inferred automatically
  when entities co-occur. Richer edges (e.g. `:advisor-of`) require explicit
  commands or custom rule code.

## Getting started

```bash
cd apps/basic-chat-demo
clojure -M:run-m -- --protocol basic-chat/v5 --fh
```

`--fh` prints the JSON focus header alongside the standard EDN payload. To
exercise the legacy v4 pipeline instead, add `--protocol basic-chat/v4` and keep
`--ner-fallback` when you want the conservative NER stage that recognises
single title-case tokens (e.g. “Tom”).

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

### Scripts

Scripted runs use EDN files containing utterance vectors:

```bash
clojure -M:run-m -- --protocol basic-chat/v5 \
        --script test/scripts/basic-chat/v5/focus-header.edn --fh-only
```

## Persistence layout

All persisted data lives in `apps/basic-chat-demo/data` (configurable via
`app.store/!env`):

- `events.ndjson` / `snapshot.edn` – legacy append-only log + snapshot used when
  XTDB is disabled.
- `xtdb/` – RocksDB directories created when XT mirroring is active (paths set
  via `BASIC_CHAT_DATA_DIR`).

On boot the CLI hydrates from XT first; if no XT data exists it replays the
snapshot + events.

### Environment overrides

Set these when running multiple instances in parallel (tests, tooling, CLI):

- `BASIC_CHAT_DATA_DIR` – root directory for snapshots, events, and XT storage.
- `BASIC_CHAT_XTDB_RESOURCE` – classpath resource for XT config (defaults to
  `xtdb.edn`).
- `BASIC_CHAT_XTDB_ENABLED` – disable XT hydration/mirroring when set to a
  falsy string (`false`, `0`, `off`, `no`).

## Testing

Each app provides a Test Runner. The main regressions live under
`apps/basic-chat-demo/test` and `apps/nlp-interface/test`.

```bash
cd apps/basic-chat-demo
clojure -M:test -m cognitect.test-runner

cd ../nlp-interface
clojure -M:test -m cognitect.test-runner
```

Golden fixtures (EDN) capture expected protocol output. Update them by rerunning
the matching scripts and copying the printed vector when intentional changes are
made.

> **Note:** The `basic-chat-demo` test runner shells out to `clojure -M:run-m`
> for each golden fixture. In sandboxed environments the spawned CLI can outlive
> the default timeout and the test command reports `command timed out` even
> though the behaviour is correct. Retry outside the sandbox (or with a larger
> timeout) if you encounter this warning.

For additional operational caveats see [LIMITATIONS.md](LIMITATIONS.md).

## Relations

- `:links-to` – default co-occurrence relation emitted by the deterministic
  pipeline.
- Custom relations (`:advisor-of`, `:supersedes`, etc.) are only created by
  inline `!rel` commands or bespoke rules.

## CLI reference

- `!entity <name> [:type]` (interactive) – ensure an entity exists and record it
  in the event log.
- `!rel <src> <type> <dst> [since <val> until <val> note <text>]` (interactive)
  – create/update a relation between entities.
- `!links <name>` (interactive) – print the neighbours recorded for the named
  entity.
- `--reset` – delete and reinitialise the persistence directory.
- `--compact` – snapshot the current Datascript DB and truncate
  `events.ndjson`.
- `--export edn` – print the serialisable EDN snapshot of the current database.
- `--context`, `--neighbors`, `--context-cap` – control neighbour context output.
- `--ner-fallback` – enable the conservative fallback NER stage in protocol v4.
- `--fh` / `--fh-only` – emit the focus header JSON alongside (or instead of)
  the normal CLI reply.
- `--fh-debug` – expand the focus header JSON with scoring and policy metadata.
- `--focus-days <n>` – adjust the salience lookback window used when building
  focus headers and neighbour slices.
- `--allow-works <on|off>` – include `:work/*` entity types in focus header
  output (off by default).
