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
2. Run a single script manually with a fresh data dir:
   ```bash
   BASIC_CHAT_DATA_DIR=$(mktemp -d) \
   BASIC_CHAT_XTDB_RESOURCE=resources/xtdb-test.edn \
   clojure -M:run-m -- --protocol basic-chat/v5 --script test/scripts/basic-chat/v5/focus-header.edn --fh-only
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

Pronouns (“I”, “you”) and speculative entities are intentionally ignored by the
current deterministic pipelines. They will not create or update nodes unless a
future resolution pass is added.

## Operational next steps

- Track the new focus/XT helper namespaces before publishing a build:
  `apps/basic-chat-demo/src/app/header.clj`, `apps/graph-memory/src/app/*.clj`,
  and `apps/basic-chat-demo/resources/xtdb-test.edn` are required for clean
  bootstrap.
- Long-running tests still need to run outside the sandbox (or with a larger
  timeout). Use `clojure -M:test -m cognitect.test-runner` from
  `apps/basic-chat-demo/` once you have full shell access.
- Interactive smoke test: `clojure -M:run-m -- --protocol basic-chat/v5 --fh`
  exercises the new focus header; add `--fh-debug` when you need the detailed
  payload.

See the individual README files for usage instructions. This document focuses on
operational gotchas that have bitten us recently.
