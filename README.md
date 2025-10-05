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

## Core features

- **Protocol variants**: choose between `basic-chat/v1` .. `basic-chat/v4` using
  `--protocol basic-chat/vN`. Later protocols layer on NER, relation extraction,
  and context-aware output.
- **Persistence**: all graph mutations flow through `app.store`, which appends
  events to `data/events.ndjson` and periodically snapshots to
  `data/snapshot.edn`. Restarting the CLI replays the log so entities,
  relations, and utterance mentions survive restarts.
- **Inline graph editing**: use bang commands in interactive mode (e.g.
  `!entity Pat :person`, `!rel "Pat" advisor-of "Joe" since 2001 ...`) to record
  curated facts; these are persisted through the same append-only log.
- **Graph context**: when v4 runs, the CLI can print the top-k neighbours for
  recognised entities to provide conversational memory. Tweak with
  `--context`, `--neighbors`, and `--context-cap`.
- **Operational knobs**: `--reset` wipes the data dir, `--compact` forces a
  fresh snapshot, and `--export edn` emits a serialisable view of the current
  database.

> **Limitation**: Pronouns such as “I” and “you” are intentionally ignored by
> the deterministic NER layers. They will not create or update entities unless a
> future personification pass resolves them to concrete participants.
> **Limitation**: Only the generic `:links-to` relation is inferred automatically
> when entities co-occur. Richer edges (e.g. `:advisor-of`) require explicit
> commands or custom rule code.

## Getting started

```bash
cd apps/basic-chat-demo
clojure -M:run-m -- --protocol basic-chat/v4 --ner-fallback
```

`--ner-fallback` enables the conservative fallback stage of the v4 NER pipeline,
allowing single title-case tokens (e.g. “Tom”) to be recognised when higher
confidence signals are absent. Leave it off for a stricter gazetteer/pattern
pass.

Flags mirror the features above; pass `-- --help` (invalid option) to see usage
from the CLI.

### Scripts

Scripted runs use EDN files containing utterance vectors:

```bash
clojure -M:run-m -- --protocol basic-chat/v4 \
        --script test/scripts/basic-chat/v4/entities.edn --ner-fallback
```

## Persistence layout

All persisted data lives in `apps/basic-chat-demo/data` (configurable via
`app.store/!env`):

- `events.ndjson` – append-only log of `:entity/upsert`, `:relation/upsert`,
  and future event types.
- `snapshot.edn` – periodic Datascript snapshots storing a list of `[:db/add
  ...]` triples for deterministic restores.

On boot the CLI loads the snapshot (when present) and replays events to produce
the live Datascript connection.

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
