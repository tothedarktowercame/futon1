# Known limitations

This repository still carries a few operational rough edges. Keep the following in
mind when exercising the v5 focus-header flow.

## Test runner timeouts

`clojure -M:test -m cognitect.test-runner` shells out to `clojure -M:run-m` for
each golden fixture (including the v5 focus-header script). On slower disks or in
sandboxed environments those subprocesses can outlive the default watchdog and
you will see output like:

```
command timed out after 240000 milliseconds
```

This does **not** necessarily indicate a regression. To double-check locally:

1. Stop any lingering `clojure` processes (`ps aux | grep clojure`).
2. Run a single script manually with a fresh data dir (any EDN under
   `apps/client/test/scripts/` works—for example the `hello.edn` smoke test):
   ```bash
   BASIC_CHAT_DATA_DIR=$(mktemp -d) \
   BASIC_CHAT_XTDB_RESOURCE=resources/xtdb-test.edn \
   clojure -M:run-m -- --fh-only --script apps/client/test/scripts/hello.edn
   ```
3. If that succeeds, rerun the full test suite outside the sandbox or with a
   larger timeout budget.

## XT hydration warnings

During startup you may see logs such as:

```
[store] skipped relation hydration for 1 item(s): 1234-...
```

XT stored an edge whose endpoints were missing. Populate the missing entities
(or remove the orphan relation) and the warning disappears on the next run.

## RocksDB lock contention

Running multiple Futon processes against the same `BASIC_CHAT_DATA_DIR` triggers
RocksDB lock errors. Point each process at a unique data directory (or shut down
other runs) before starting the CLI or tests.

## Deterministic NER gaps

Pronouns and section headings used to produce noisy graph nodes. The current codebase now:

- normalises first-person pronouns to the active profile name;
- normalises second-person pronouns to a default interlocutor (`"You"` unless overridden in the profile doc);
- normalises first-person plural pronouns to a collective label (`"Me & You"` by default);
- expands the titlecase stop-list to drop headings such as “Personal”, “Creative”, etc.

Further work (still outstanding):

- refine stop-lists and heuristics so adjective-only spans or dangling nominal phrases do not mint entities;
- expose configuration hooks so profiles can explicitly name their interlocutor/collective roles without editing EDN by hand.

These TODOs are tracked here so development can resume smoothly if interrupted.

## Typed relations

The ingest pipeline now turns Stanford CoreNLP OpenIE triples into typed
keywords (for example `:works-at`, or `:sister/assure` when the predicate can be
namespaced from its object) before they are persisted. New relation labels are
registered automatically with the Graph Memory type registry, and aliases such
as lemma vs. gloss variants are merged on first sighting. When OpenIE fails to
provide a predicate, the system continues to emit the `:links-to` fallback so
ingestion remains robust.

- The ingest pipeline currently treats questions as normal assertions. For example,
  “Do I own a pocketknife?” will store a relation as if the user had asserted the fact.
  There is no question→Datalog mapping yet, so queries are not run programmatically.

## Operational next steps

- Track the new focus/XT helper namespaces before publishing a build:
  `apps/graph-memory/src/app/header.clj`, `apps/graph-memory/src/app/*.clj`,
  and `apps/graph-memory/resources/xtdb-test.edn` are required for clean
  bootstrap.
- Long-running tests still need to run outside the sandbox (or with a larger
  timeout). Use `clojure -M:test -m cognitect.test-runner` from
  `apps/graph-memory/` once you have full shell access.
- Interactive smoke test: `clojure -M:run-m` (from the repo root or
  `apps/demo/`) exercises the focus header; inspect the emitted `fh>` lines for
  debugging output.

See the individual README files for usage instructions. This document focuses on
operational gotchas that have bitten us recently.
