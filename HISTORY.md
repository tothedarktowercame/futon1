# History

This file tracks notable milestones for the `basic-chat` protocols and retains
documentation for earlier versions now superseded by the default
`basic-chat/v5` pipeline.

## Protocol timeline

### basic-chat/v5 — XT-backed focus header
- Launch interactively: `clojure -M:run-m -- --protocol basic-chat/v5 --fh`
- Scripted demo: `clojure -M:run-m -- --protocol basic-chat/v5 --script test/scripts/basic-chat/v5/focus-header.edn --fh-only`
- Mirrors Datascript mutations into XTDB, hydrates from XT on boot, and emits the
  JSON focus header consumed by the agent integrations.

### basic-chat/v1 — baseline intent echo
- Launch interactively: `clojure -M:run-m -- --protocol basic-chat/v1`
- Scripted demo: `clojure -M:run-m -- --protocol basic-chat/v1 --script test/scripts/hello.edn`
- Emits intent analyses and derives links without entity storage. Useful for
  regression coverage of the original CLI behaviour.

### basic-chat/v2 — POS tagging + entity log
- Launch interactively: `clojure -M:run-m -- --protocol basic-chat/v2`
- Extras: `/diff` lists node labels from the latest turn, `/dump` prints the
  in-memory graph for inspection.
- Scripted demo: `clojure -M:run-m -- --protocol basic-chat/v2 --script test/scripts/v2-basic.edn`
- Introduced Datascript-backed entity tracking in tandem with POS tagging.

### basic-chat/v3 — classical gazetteer NER
- Launch interactively: `clojure -M:run-m -- --protocol basic-chat/v3`
- Helpful flags: `--list-entities` prints known entities after a run, and
  `--links "Name"` shows direct neighbours recorded in the graph.
- Layered deterministic gazetteer/entity-relation extraction before the v4
  overhaul.

For the tiered NER stack without XT mirroring, use `basic-chat/v4` from the
project README. Later protocols build on this pipeline.
