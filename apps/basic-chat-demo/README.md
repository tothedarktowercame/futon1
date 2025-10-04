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

Run the scripted v1 demo from the app directory:

```bash
clojure -M:run-m -- --protocol basic-chat/v1 --script test/scripts/hello.edn
```

You should see stable EDN output describing each turn, e.g.:

```clojure
[{:in "hello there", :intent {:type :greet, :conf 0.99}, :links [{:type :derives}]}
 {:in "ok bye", :intent {:type :farewell, :conf 0.99}, :links [{:type :derives}]}]
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
