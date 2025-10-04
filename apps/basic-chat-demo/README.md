# basic-chat-demo

Baseline chat pipeline that wires the NLP interface and graph-memory apps together.

## Prerequisites

- [Clojure CLI tools](https://clojure.org/guides/getting_started) installed locally.
- This repository checked out with the sibling apps (`apps/nlp-interface`, `apps/graph-memory`) intact.

## Quickstart

Start an interactive session (default protocol is `basic-chat/v1`):

```bash
clojure -M:run-m
```

You’ll see a `you>` prompt; type messages and the bot will reply with the processed EDN summary. Send `:quit` (or press `Ctrl+D`) to exit.

To try the richer v2 pipeline (POS tagging + entity tracking), launch:

```bash
clojure -M:run-m -- --protocol basic-chat/v2
```

Interactive extras for v2:

- `/diff` — list the new node labels introduced by your most recent message.
- `/dump` — print the full in-memory graph (nodes + edges) for inspection.

For the classical v3 NER/ER pass:

```bash
clojure -M:run-m -- --protocol basic-chat/v3
```

Helpful flags for `basic-chat/v3`:

- `--list-entities` — print all known entities after the run.
- `--links "Name"` — show direct neighbors for the specified entity.

Run the scripted v1 demo from the app directory:

```bash
clojure -M:run-m -- --protocol basic-chat/v1 --script test/scripts/hello.edn
```

You should see stable EDN output describing each turn, e.g.:

```clojure
[{:in "hello there", :intent {:type :greet, :conf 0.99}, :links [{:type :derives}]}
 {:in "ok bye", :intent {:type :farewell, :conf 0.99}, :links [{:type :derives}]}]
```

Run the v2 scripted demo to see entity aggregation:

```bash
clojure -M:run-m -- --protocol basic-chat/v2 --script test/scripts/v2-basic.edn
```

Expected output summary (IDs omitted for brevity):

```clojure
[{:in "Met Serena at PatCon 30" :summary "noted. you mentioned: Met, Serena, at, PatCon, 30, Met Serena."}
 {:in "Graph Memory feels like a notebook" :summary "noted. you mentioned: Met, Serena, at, PatCon, 30, Met Serena, Graph, Memory, feels, like, a, notebook, Graph Memory."}]
```
## Testing

```bash
clojure -M:test -m cognitect.test-runner
```

The test harness replays the same script and compares it against the golden fixture in `test/golden/hello.out.edn`.

## Protocols

Protocols live under `protocols/`. The runner defaults to `basic-chat/v1`; pass `--protocol basic-chat/v1` explicitly if you want to be explicit or when trying other versions (e.g. upcoming `basic-chat/v2`).

## License

Distributed under the Eclipse Public License version 1.0.
