# basic-chat-demo (v5 focus header edition)

`basic-chat-demo` is the command-line driver for the Futon focus-header stack. It
streams user turns through the deterministic NLP pipeline, mirrors entities and
relations into XTDB, and emits a JSON **focus header** suitable for agent
prompts.

If you are looking for older protocols (`basic-chat/v1` … `v4`) see
[HISTORY.md](HISTORY.md) — this README focuses on v5 behaviour.

## Quick start (v5)

What happens:

1. An XTDB node starts (using `resources/xtdb.edn`).
2. Datascript hydrates from XT so salience metadata is available immediately.
3. The CLI enters interactive mode. After each user turn you will see:
   - the standard EDN result map from the pipeline, **and**
   - a `fh>` line containing the focus header JSON (because of `--fh`).

Use `:quit` (or `Ctrl+D`) to exit. Add `--fh-only` if you only need the JSON
header (for example, when an external client handles the response rendering).

## Key concepts

### Focus header

The header summarises high-salience anchors and neighbours the agent should
prefer when answering. It is influenced by:

- recent entities (`--focus-days`, default 30)
- whether `:work/*` nodes are allowed (`--allow-works on|off`)
- per-edge caps defined in `app.header/default-policy`

### Datascript + XTDB mirroring

All mutations are applied to Datascript and mirrored into XTDB. On reboot the
app first hydrates from XT and then replays any remaining legacy events (when
XT was disabled). Salience metadata such as `:entity/seen-count` and
`:relation/last-seen` are read from XT during focus-header construction.

### Command helpers

Interactive bang commands still work in v5:

- `!entity <name> [:type]` – upsert an entity and update salience timestamps.
- `!rel <src> <type> <dst> [since … until … note …]` – record structured edges.
- `/links`, `/diff`, etc. continue to work for inspection.

## CLI reference (v5 focus header)

| Flag | Purpose |
|------|---------|
| `--fh` | Print the focus header JSON after each turn. |
| `--fh-only` | Suppress the normal EDN response; emit only the focus header. |
| `--focus-days <n>` | Salience lookback window (days). |
| `--allow-works <on|off>` | Include work/project entities in focus slices. |
| `--context` / `--context-cap` / `--neighbors` | Control the legacy textual context (still available). |
| `--compact` | Snapshot Datascript and reset the legacy event log. |
| `--reset` | Delete the data directory and reinitialise the store. |
| `--export edn` | Print an EDN snapshot of the Datascript DB. |

## Environment configuration

Set these env vars when running multiple instances or isolating tests:

- `BASIC_CHAT_DATA_DIR` – root directory for snapshots and XT RocksDB storage
  (defaults to `data/`).
- `BASIC_CHAT_XTDB_RESOURCE` – classpath resource with the XT config (defaults
  to `xtdb.edn`).
- `BASIC_CHAT_XTDB_ENABLED` – disable XT mirroring by setting to `false`, `0`,
  `off`, or `no`.

Tests configure `BASIC_CHAT_DATA_DIR` to a temp directory and point
`BASIC_CHAT_XTDB_RESOURCE` at `resources/xtdb-test.edn` so each run uses fresh
storage.

## Testing

```bash
clojure -M:test -m cognitect.test-runner
```

The suite shells out to `clojure -M:run-m` with golden scripts (v5 included). In
sandboxed or slow environments these subprocesses can outlive the default
watchdog and the command reports `command timed out`. Retry outside the sandbox
or raise the timeout if that happens.

## Scripts & automation

Scripted conversations live in `test/scripts/`. For the focus-header flow, use
`test/scripts/basic-chat/v5/focus-header.edn` (see HISTORY for legacy protocol
examples).

```bash
clojure -M:run-m -- --protocol basic-chat/v5 \
        --script test/scripts/basic-chat/v5/focus-header.edn \
        --fh-only
```

The command prints a vector of focus headers — perfect for golden tests or
client integration checks.

## Troubleshooting

- **Hydration warnings**: `[store] skipped relation hydration …` means XT stored
  an edge whose endpoints were missing. Populate the missing entities (or remove
  the orphan relation) to silence the warning.
- **RocksDB locks**: if XT refused to start (`LockFile: ...`), ensure no other
  Futon process is using the same `BASIC_CHAT_DATA_DIR`, or point the CLI at a
  unique data directory.

## Legacy protocols

Details about protocols v1–v4, their CLI flags, and the historical persistence
mechanics have moved to [HISTORY.md](HISTORY.md).
