# nlp-interface

Deterministic NLP utilities that power Futon's in-memory session runner and demo agent. The
module exposes helper fns that downstream apps call to turn free-form text into intents,
entities, relations, and graph-friendly metadata. The pipeline is intentionally staged so
tests (and future callers) can stop at individual phases without recreating the orchestration.

## Pipeline overview

```
text → tokenize → POS tag → chunk → intent
     ↘ entity recognizers (gazetteer + patterns + heuristics) → relation heuristics
     ↘ graph-memory store writes (utterance, intent, entities, mentions, relations)
```

```clojure
(require '[nlp-interface.nlp-interface :as nlp])

(def ctx (nlp/run-pipeline "Meet Tom in Minneapolis on 12 Oct"))
;; => {:tokens [...], :pos [["Meet" "NNP"] ...],
;;     :chunks [{:text "Tom" :kind :noun} ... {:text "12 Oct" :kind :temporal}],
;;     :intent {:type :greet ...}}

;; You can stop at any stage
(nlp/run-pipeline "Schedule a trip" [:tokenize :tag])
;; => {:tokens ["Schedule" "a" "trip"], :pos [["Schedule" "NNP"] ...]}
```

### Public API contracts

- `run-pipeline` always returns a map containing the requested stages (`:tokens`, `:pos`,
  `:chunks`, `:intent`, `:entities`, `:relations`). Passing an explicit vector of stages keeps the
  function pure; no storage writes occur when NLP is used for inspection/testing.
- `handle-input` / `handle-input-v4` wrap `run-pipeline` and emit a response map with these keys:
  - `:utterance` – deterministic UUID + timestamp (persisted via `graph-memory.main/add-utterance!`).
  - `:intent` – map with `:type`, `:conf`, and `:source` (`:dictionary` vs. `:fallback`).
  - `:entities` – vector of `{ :name :type :id :span [:start :end] :value? ... }` maps ready for
    storage.
  - `:relations` – vector of derived relation triples (`:type`, `:src`, `:dst`).
  - `:features` / `:intent-candidates` – debug vectors surfaced to the CLI and focus headers.
- Gazetteer/pattern resources (`resources/gazetteer/*.edn`, `resources/patterns.edn`,
  `resources/intent_*.edn`) define the deterministic vocabulary and therefore form part of the
  stable API. Updates must keep the EDN schemas unchanged and be accompanied by fixture updates
  under `test/nlp_interface`.

Callers outside the demo/client workflow should stick to these entry points; lower-level helpers
remain intentionally private so the schema/heuristic layers can evolve without breaking clients.

### Stage catalogue

- **Tokenize** – `tokenize` is the shared splitter used by the tests and downstream apps. It
  keeps "'s" and punctuation as standalone tokens so later rules can distinguish possessives vs.
  contractions. `run-pipeline` stores the resulting vector on `:tokens`.
- **Tag** – `pos-tag` is a deterministic tagger that recognises capitalised proper nouns (NNP),
  numbers (CD), gerunds (VBG), and leaves the rest as nouns (NN). The tagged data is emitted as
  `:pos` plus a convenience `:tags` vector.
- **Chunk** – `chunk-stage` (exposed via `run-pipeline`) groups contiguous noun, verb/command,
  and temporal expressions so downstream heuristics can grab ready-made phrases. The parse data
  returned by `parse-tree` is also attached at this stage and keeps the legacy
  `[:utterance [NNP Tom] ...]` structure used by the UI and logs.
- **Intent** – `intent-stage` first scores the RID-style dictionary housed in
  `resources/intent_lexicons.edn` / `intent_thresholds.edn`. When no lexical bucket clears its
  threshold, `nlp-interface.features/extract` supplies fallback indicators (`:imperative?
  true`, `:temporal-window 2`, etc.) so `intent/analyze` can emit classified intents such as
  `:scheduling`, `:support-request`, or `:unknown`. The stage records the detected intent as well
  as `:intent-candidates` for debuggability.

### Intent inference
- `nlp-interface.intent/analyze` first scores against a large dictionary of lexical buckets
  (greetings, sadness, voyage, etc.). Confidence is derived by counting keyword hits; the first
  matching bucket above its threshold wins.
- When the dictionary is unsure, `nlp-interface.features/extract` computes hand-written
  features (imperative phrasing, temporal phrasing, politeness markers, lexicon hits). The
  fallback layer promotes intents like `:scheduling`, `:support-request`, or `:unknown` with a
  confidence noted as `:fallback` in the result’s `:source` key.
- `handle-input`/`handle-input-v4` store both the best scoring intent and the alternative
  `:intent-candidates` vector on the utterance node so clients can inspect the confidence gap.

### Entity recognition and relation heuristics
- Legacy gazetteer mode (`handle-input`) pairs the regex tokenizer, POS tags, and
  `nlp-interface.ner-er/ner`. Gazetteer data lives under `resources/gazetteer/*.edn`. All spans
  are enumerated, scored against the gazetteer, pruned to non-overlapping mentions, and stored
  via `graph-memory.main/ensure-entity!` + `add-mention!`. Basic `:links-to` relations are
auto-generated between adjacent entities.
- Tiered v4 mode (`handle-input-v4`) is what the client and demo now call. It layers multiple
  deterministic recognizers implemented in `nlp-interface.ner-v4`:
  - Gazetteer + catalog merges (graph-memory entities are folded back in so "Me" can resolve to
    a person the user already mentioned.)
  - Pattern recognizers backed by `resources/patterns.edn` capture dates, times, versions,
    files, URLs, mailboxes, tags, etc., performing best-effort normalization (e.g. dates →
    `YYYY-MM-DD`).
  - POS/titlecase heuristics grow multi-token noun phrases, with contextual nudges (prepositions
    like "in" favour places; organization suffixes like "Lab" favour orgs).
  - Domain triggers look for product/version keywords (XTDB, v5) and resolve ad-hoc tokens.
  - Optional fallback heuristics (`:enable-fallback? true`) turn on a forgiving suffix/prefix
    sweep used in tests for regression coverage.
- After entity spans are sorted, adjacent entities are run through lightweight relation
  builders that look at the connective text to emit relation triples such as
  `:located-in`, `:scheduled`, `:with`, or a default `:links-to`.

### Storage integration
Every entry point receives a `graph-memory` connection. As soon as text arrives, the module:
1. Adds the utterance node + timestamp (`graph-memory.main/add-utterance!`).
2. Records the best intent, links it back with `:derives` edges, and persists intent metadata.
3. Ensures entity nodes exist, writes mention spans, and adds relation facts.
4. Returns a convenient response map that front-ends can render or log without re-querying the
   store (`:tokens`, `:pos`, `:entities`, `:relations`, etc.).

## Key namespaces

| Namespace | Responsibility |
|-----------|----------------|
| `nlp-interface.nlp-interface` | Orchestrates the token→intent→entity pipeline and graph writes. |
| `nlp-interface.intent` | Dictionary + heuristic fallback intent scoring. |
| `nlp-interface.features` | Feature extractor used by the fallback intent layer. |
| `nlp-interface.ner-er` | Original gazetteer + relation heuristics. |
| `nlp-interface.ner-v4` | Tiered recognizer with gazetteers, catalog merging, pattern and POS layers, fallback spans. |

The resources that feed the recognizers live in `resources/` and are pure EDN so they can be
extended without code changes (`gazetteer/*.edn`, `patterns.edn`, `stoplists.edn`,
`intent_thresholds.edn`, `intent_lexicons.edn`).

## Tests and references

Run from `apps/nlp-interface/`:

```bash
clojure -M:test
```

The suite documents the behaviour of each stage:
- `test/nlp_interface/nlp_interface_test.clj` covers the staged pipeline (token → tag → chunk →
  intent), the tiered NER recogniser (including fallback toggles), and the dictionary intent
  analyser.
- `test/nlp_interface/intent_fallback_test.clj` locks in the fallback heuristics that promote
  scheduling/support intents, along with the `:unknown` safety valve.

Each test namespace includes deterministic fixtures so regressions can be reproduced without a
model server.

## Development workflow

- `clojure -M:run-m` prints `"nlp-interface ready"`; the richer APIs are consumed by the
  graph-memory, client, and demo apps rather than exposing their own CLI flags.
- `clojure -T:build test` exercises the build namespace’s test target; `clojure -T:build ci`
  extends that with pom syncing and uberjar packaging as noted in the root guidance.

When changing the pipeline, keep intent and entity logic in this module (per the repository
guidelines) and extend tests in `apps/nlp-interface/test` to cover any new heuristics or
resources before declaring the code “ready”.
