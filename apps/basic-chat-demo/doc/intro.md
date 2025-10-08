# Introduction to basic-chat-demo

`basic-chat-demo` is the command-line harness that exercises the shared NLP (`apps/nlp-interface`) and graph memory (`apps/graph-memory`) services inside this repository. Recent work concentrated on two areas:

1. **XT-backed persistence** — every entity and relation written to the in-memory Datascript store is mirrored into XTDB. On restart the app hydrates Datascript from XT first, guaranteeing that salience metadata (`:entity/seen-count`, `:relation/last-seen`, etc.) is available before focus headers are generated. If XT is unavailable the app falls back to the legacy snapshot + NDJSON event log.
2. **Focus Header surfacing** — the CLI now exposes the focus header JSON the agent stack consumes. Use `--fh` to print it alongside responses or `--fh-only` to suppress the normal reply and emit just the header. Policy flags such as `--focus-days` and `--allow-works` flow directly into the salience heuristics implemented in `app.focus`.

## Environment configuration

The runtime accepts several environment variables so multiple instances (tests, CLI, Emacs client) can co-exist without clobbering each other:

| Variable | Purpose | Default |
|----------|---------|---------|
| `BASIC_CHAT_DATA_DIR` | Root directory for snapshots, NDJSON events, and XT RocksDB files | `data/` under the app |
| `BASIC_CHAT_XTDB_RESOURCE` | Classpath resource that defines the XT node configuration | `xtdb.edn` when present |
| `BASIC_CHAT_XTDB_ENABLED` | Disable XT mirroring/hydration when set to `false`/`0`/`off`/`no` | enabled |

The test suite sets `BASIC_CHAT_DATA_DIR` to a temporary directory and points `BASIC_CHAT_XTDB_RESOURCE` at `resources/xtdb-test.edn` so each run uses a unique RocksDB path.

## Test status

- `clojure -M:test -m cognitect.test-runner` exercises the end-to-end CLI by shelling out to `clojure -M:run-m` with golden scripts. In constrained or sandboxed environments these subprocesses can outlive the default watchdog and the runner reports a timeout even though the underlying logic is sound. Re-run the command with a longer timeout (or outside the sandbox) if you hit that condition.
- XT hydration logs warnings (`[store] skipped relation hydration …`) when it encounters relations whose endpoints are still missing. These are informational and the CLI will continue booting; populate the missing entities to clear the warning on the next run.

## Next steps

- Add an explicit integration test that boots against XT, writes a small graph, and verifies focus header output (currently only manual smoke tests cover this path).
- Publish CLI examples that demonstrate piping `--fh-only` output into the Emacs client shim.
