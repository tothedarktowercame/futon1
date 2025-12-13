# FUTON1 Baseline Snapshot

This note captures the deterministic substrate referenced by FUTON1 prototypes 0–1. It
documents the ingest → NLP → graph-memory → XTDB pipeline, points to the canonical transcript,
and explains how to reproduce the reference snapshot.

## Pipeline summary

1. **Ingest** – `apps/client` runs scripted lines (or the CLI) and classifies each turn via
   `client.engine/classify`, delegating `/`, `!`, and conversational text to the shared handlers.
2. **NLP interface** – `apps/nlp-interface` tokenises, tags, chunks, and detects intents/entities
   deterministically (prototype 2). Both the dictionary and fallback heuristics are exercised here.
3. **Graph memory** – `apps/graph-memory` ensures entities/relations exist in Datascript and mirrors
   writes into XTDB, preserving salience metadata and hyperedge context (prototype 3).
4. **XTDB persistence** – `app.xt` mirrors every mutation into XTDB so a restart hydrates salience
   immediately. Focus headers pull from Datascript but rely on XTDB to keep last-seen metadata stable.

Each stage is pure Clojure; no remote services are involved. The demo/client runner therefore acts
as calibrated instrumentation rather than an informal REPL.

## Canonical snapshot

`resources/baseline/demo_session.edn` records the Willie/Jane scenario used by the tests. It
includes:

- `:script` – the exact utterances fed to `client.api/run-line`.
- `:focus-header` – the rendered focus-header lines per turn (context, recent, enriched sections).
- `:datascript-*` – sorted entity and relation summaries immediately after the third turn.
- `:xtdb-*` – the same summaries when queried from XTDB, proving Datascript↔XTDB mirroring.

Tests read this file directly (see `apps/client/test/client/session_test.clj`) so drift in either the
focus header or the persistence layer causes an immediate regression.

## Regenerating the baseline

1. Ensure Clojure + Java are installed (`./scripts/install-clojure-env.sh` if needed).
2. From the `futon1` root run:

   ```bash
   clojure -Sdeps '{:deps {client/client {:local/root "apps/client"}}}' \\
     -M -m scripts.baseline-snapshot --write resources/baseline/demo_session.edn
   ```

   or, using the alias added in `deps.edn`:

   ```bash
   clojure -M:baseline/snapshot --write resources/baseline/demo_session.edn
   ```

3. Commit the updated EDN when the snapshot intentionally changes (e.g., schema or header format
   updates). The deterministic test will fail until fixture + behaviour match.

## Determinism guarantees

- `apps/client/test/client/session_test.clj` now compares live runs against the baseline snapshot,
  covering focus headers plus Datascript/XTDB summaries.
- `scripts/baseline_snapshot.clj` is the single reference for generating fixtures, so document shape
  and semantics stay in sync.
- XTDB accesses go through `app.xt`, keeping the instrumentation identical to production runs.
- The `Futon1 Deterministic Stack` GitHub Action mirrors these suites (client session, graph-memory,
  and NLP tests) on every push/PR, so regressions surface even when nobody is manually running the
  determinism script.

When these checks pass, FUTON1 prototype 0 (baseline substrate) and prototype 1 (demo/client
instrumentation) remain in the DONE state, giving FUTON0 and FUTON3 a reproducible foundation.
