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

### Intent examples

The deterministic intent classifier now compares every utterance against a large
set of keyword families inspired by the Regressive Imagery Dictionary. In
addition to conversational intents (greetings, farewells, gratitude, apologies,
affirmations, negations, and help requests) it tags thematic clusters such as
`primary need orality`, `voyage`, `positive affect`, `sadness`, `aggression`,
`abstract thought`, and more. The lookup covers hundreds of lexical stems so you
should see fewer `Intent: unknown` responses when chatting about concrete
topics.

Some quick prompts to try:

```
you> hello there
bot> Intent: greet (confidence 0.99)

you> can you help me understand this report?
bot> Intent: help request (confidence 0.73)

you> we will sail across the ocean and wander new shores
bot> Intent: voyage (confidence 0.69)

you> I cooked dinner with garlic and wine.
bot> Intent: primary need orality (confidence 0.82)
```

The greeting/farewell strings still double as regression tests—the `hello.edn`
script drives the golden output in `test/golden/hello.out.edn` to guard the
baseline behaviour. The extended dictionary is exercised by the
`nlp-interface` unit tests.

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
