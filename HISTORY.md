# History

This file tracks notable milestones for the retired `basic-chat` protocols and
retains documentation for earlier versions now superseded by the consolidated
ingest pipeline. The code that implemented these protocols has been removed;
keep these notes around only when you need historical context.

## Protocol timeline

### Legacy basic-chat/v5 — XT-backed focus header
- Launch interactively: `clojure -M:run-m` (default protocol `basic-chat/v5`, focus headers stream as `fh>` lines).
- Scriptable demo: drive the CLI via `client.api/run-script` and capture the vector of turns you care about (fixture paths now live under `apps/client/test/scripts/`).
- Mirrors Datascript mutations into XTDB, hydrates from XT on boot, and emits the
  JSON focus header consumed by the agent integrations.

### Legacy basic-chat/v1 — baseline intent echo
- Launch interactively: `clojure -M:run-m` and send turns with `{:protocol "basic-chat/v1"}` via the HTTP API or client session options.
- Scripted demo: `client.api/run-script` with `test/scripts/hello.edn` (set `:protocol` on each turn request).
- Emits intent analyses and derives links without entity storage. Useful for
  regression coverage of the original CLI behaviour.

### Legacy basic-chat/v2 — POS tagging + entity log
- Launch interactively: `clojure -M:run-m` with `{:protocol "basic-chat/v2"}` on your turn payloads.
- Extras: `/diff` lists node labels from the latest turn, `/dump` prints the
  in-memory graph for inspection.
- Scripted demo: `client.api/run-script` + `test/scripts/v2-basic.edn` using the same protocol override.
- Introduced Datascript-backed entity tracking in tandem with POS tagging.

- Launch interactively: `clojure -M:run-m` while targeting the API with `{:protocol "basic-chat/v3"}`.
- Helpful flags: `--list-entities` prints known entities after a run, and
  `--links "Name"` shows direct neighbours recorded in the graph.
- Layered deterministic gazetteer/entity-relation extraction before the v4
  overhaul.

For completeness we still reference the `basic-chat/v4` tiered NER stack below,
but remember these flows are historical. Modern builds always call the shared
NLP/ingest pipeline described in `README.md`.
