# Repository Guidelines

## Project Structure & Module Organization

The apps live in `apps/` — Each app must have `deps.edn` and aliases `:run-m`, `:test`, `:build`. Namespaces map hyphen→underscore; config stays in `resources/`. Ephemeral dirs (`target/`, logs, caches) are app-scoped and git-ignored.

## Development status of included apps

`common` — lightweight configuration/shared utilities

`graph-memory` `nlp-interface` `open-world-ingest` — all provide core functionality
(graph-memory now owns the store manager, Datascript/XTDB helpers, slash/bang
logic, and focus header builders.)

`client` — the deterministic session runner used by both CLI and automated tests
`demo` — wraps the client with a user interface layer

`api` — packages the system for external clients

## Build and Test 
Run commands from the target app directory unless noted:
- `clojure -M:run-m` starts the module’s primary entry point.
- `clojure -M:test` executes the Cognitect test-runner suite (unit + scripted cases).
- `clojure -T:build test` runs the build namespace test task; `clojure -T:build ci` adds uberjar packaging and cleanup.

## Coding Style & Naming Conventions
Stick to idiomatic Clojure with 2-space indentation and align binding forms (`let`, `for`, `cond`). Namespace paths should mirror directories (e.g. `apps/demo/src/demo/main.clj` -> `demo.main`). Favor `kebab-case` for functions/vars, `SCREAMING_SNAKE_CASE` for constants, and `UpperCamelCase` only for records/types. Run `clj-kondo` before submitting; organize requires alphabetically and group `:refer` forms sparingly.

## Testing Guidelines
Create test namespaces that mirror production namespaces and end in `-test`. Keep deterministic fixtures in `test/golden` or `test/scripts`, and document scenario intent at the top of each test namespace. Always run `clojure -M:test` (and pertinent `bb` tasks) before opening a PR, ensuring new behavior has coverage or an explicit rationale when no test is practical.

## Commit & Pull Request Guidelines
Commit messages should be concise, present-tense imperatives (history examples: “add a test…”, “remove old dir”). Squash incidental WIP commits locally; keep one logical change per commit. Pull requests need a short summary of why the change matters, links to any tracked issues, notes on manual verification, and screenshots or transcripts for UI or agent workflow impacts.

## Separation of concerns

- `apps/open-world-ingest` owns the NLP pipeline and exposes ingestion adapters; it should call into the shared storage layer rather than redefine persistence.
- `apps/graph-memory` provides the storage/graph primitives (entity/relation handling, XT utilities). Reuse these from other apps.
- `apps/client` and `apps/api` must remain thin: parse input, call shared adapters/protocols, format responses. Do **not** reimplement ingest or reasoning logic.
- When adding capabilities, extend the feature module (e.g. `open-world ingest` or `graph-memory`) first, then delegate to it from the frontends.
