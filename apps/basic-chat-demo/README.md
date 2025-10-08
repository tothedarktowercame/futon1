# basic-chat-demo

Baseline chat pipeline that wires the NLP interface and graph-memory apps together.

## Prerequisites

- [Clojure CLI tools](https://clojure.org/guides/getting_started) installed locally.
- This repository checked out with the sibling apps (`apps/nlp-interface`, `apps/graph-memory`) intact.

## Quickstart

Start an interactive session (defaults to `basic-chat/v4`):

```bash
clojure -M:run-m
```

You’ll see a `you>` prompt; type messages and the bot will reply with the processed EDN summary plus a human-readable focus header. Send `:quit` (or press `Ctrl+D`) to exit.

Helpful flags for the default v4 pipeline:

- `--ner-fallback` — include conservative single-token fallback entities (e.g. unknown proper names).
- `--list-entities` / `--links` — inspect the shared graph neighbours.
- `--fh-json` — emit a machine-readable focus header alongside the readable block.

To experiment with earlier pipelines (`basic-chat/v1`..`v3`) or replay their scripted demos, see `HISTORY.md`.

Run the scripted v4 demo from the app directory:

```bash
clojure -M:run-m -- --protocol basic-chat/v4 --script test/scripts/basic-chat/v4/entities.edn
```
## Testing

```bash
clojure -M:test -m cognitect.test-runner
```

The test harness replays the same script and compares it against the golden fixture in `test/golden/hello.out.edn`.

## Protocols

Protocols live under `protocols/`. The runner defaults to `basic-chat/v4`; use `--protocol` to opt into legacy variants recorded in `HISTORY.md`.

## License

Distributed under the Eclipse Public License version 1.0.
