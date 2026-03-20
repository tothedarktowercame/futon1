# Windows Script Wrappers

This directory contains Windows batch wrappers for futon1 local operations.

## XTDB inspection wrappers

- `xtdb-browser-windows.bat`
- `xtdb-count-windows.bat`
- `xtdb-peek-windows.bat`
- `xtdb-scan-windows.bat`
- `archive-xt-snapshot-windows.bat`

## Deterministic stack test wrappers

These wrappers mirror the current futon1 deterministic stack CI matrix in
`.github/workflows/futon1-stack-tests.yml`:

- `client-test-windows.bat` -> `apps/client` with `clojure -M:test`
- `graph-memory-test-windows.bat` -> `apps/graph-memory` with
  `clojure -M:test -m cognitect.test-runner`
- `nlp-interface-test-windows.bat` -> `apps/nlp-interface` with
  `clojure -M:test`
- `futon1-stack-tests-windows.bat` -> runs the three wrappers above and returns
  non-zero if any suite fails
  - canonical single-script local equivalent to the current
    `.github/workflows/futon1-stack-tests.yml` matrix
  - executes matrix entries directly via `run-clojure-test-windows.bat`
    (`apps/client`, `apps/graph-memory`, `apps/nlp-interface`)
  - emits machine-readable receipt:
    - default path:
      `scripts/windows/raw/latest-futon1-stack-tests-receipt.json`
    - override path:
      `FUTON1_STACK_TESTS_RECEIPT=<custom-path>`
    - schema:
      - `schema` (`futon1-stack-tests-receipt/v1`)
      - `generated_at_utc`
      - `workflow_source`, `workflow_source_exists`, `workflow_source_sha256`
      - `jobs[]`: `name`, `command`, `start_utc`, `end_utc`,
        `duration_seconds`, `exit_code`,
        `workflow_job_name`, `workflow_workdir`, `workflow_command`,
        `local_windows_lwjgl_patch`
      - `summary`: `start_utc`, `end_utc`, `duration_seconds`, `failures`,
        `aggregate_exit_code`, `aggregate_verdict`

Shared helper:

- `run-clojure-test-windows.bat` provides common app-directory + command
  dispatch wiring for the wrappers.
  - usage: `run-clojure-test-windows.bat APP_DIR TEST_MODE [ARGS...]`
  - `TEST_MODE` values:
    - `default` => `clojure -M:test`
    - `cognitect` => `clojure -M:test -m cognitect.test-runner`
  - clojure launcher path: sibling
    `../futon3c/.tools/clojure/bin/clojure.bat`
  - clears `HTTP_PROXY`, `HTTPS_PROXY`, `ALL_PROXY`, `GIT_HTTP_PROXY`,
    `GIT_HTTPS_PROXY` in script scope before invoking Clojure.
  - writes per-app run logs to `scripts/windows/raw/latest-<app>-test.log`
  - applies explicit log-based failure detection because the current
    `futon3c/.tools/clojure/bin/clojure.bat` path can return exit `0` even when
    test output contains runtime errors.
  - default Windows LMDB native override:
    - writes `:windows-lmdb` alias into generated `CLJ_CONFIG/deps.edn`
    - runs tests via `-M:windows-lmdb:test`
    - set `FUTON1_WINDOWS_LWJGL_NATIVE_PATCH=0` to disable

## Known Windows constraints

- `apps/client` and `apps/graph-memory` currently depend on
  `com.xtdb/xtdb-lmdb:1.24.4`.
- The resolved `xtdb-lmdb` artifact includes Linux/macOS LWJGL native
  classifiers but not Windows by default, so local Windows runs can fail with:
  - `Failed to locate library: lwjgl.dll`
  - `Error locating module {:module xtdb.lmdb/->kv-store}`
  - secondary compile symptoms in `lmdb.clj` / `lmdb__init.class`
