# Repository Guidelines

## Project Structure & Module Organization

Three apps live in `apps/` — `basic-chat-demo`, `graph-memory`, `nlp-interface` — each with `src/`, `resources/`, `test/` (incl. `test/golden`, `test/scripts`) and per-app `doc/`. Shared contracts live in `protocols/`; repo scripts in `scripts/`. Apps don’t import each other—share via `protocols/` (or future `libs/`). Each app must have `deps.edn` and aliases `:run-m`, `:test`, `:build`. Namespaces map hyphen→underscore; config stays in `resources/`. Ephemeral dirs (`target/`, logs, caches) are app-scoped and git-ignored.

## Build and Test 
Run commands from the target app directory unless noted:
- `clojure -M:run-m` starts the module’s primary entry point.
- `clojure -M:test` executes the Cognitect test-runner suite (unit + scripted cases).
- `clojure -T:build test` runs the build namespace test task; `clojure -T:build ci` adds uberjar packaging and cleanup.

## Coding Style & Naming Conventions
Stick to idiomatic Clojure with 2-space indentation and align binding forms (`let`, `for`, `cond`). Namespace paths should mirror directories (e.g. `src/basic_chat_demo/basic_chat_demo.clj` -> `basic-chat-demo.basic-chat-demo`). Favor `kebab-case` for functions/vars, `SCREAMING_SNAKE_CASE` for constants, and `UpperCamelCase` only for records/types. Run `clj-kondo` before submitting; organize requires alphabetically and group `:refer` forms sparingly.

## Testing Guidelines
Create test namespaces that mirror production namespaces and end in `-test`. Keep deterministic fixtures in `test/golden` or `test/scripts`, and document scenario intent at the top of each test namespace. Always run `clojure -M:test` (and pertinent `bb` tasks) before opening a PR, ensuring new behavior has coverage or an explicit rationale when no test is practical.

## Commit & Pull Request Guidelines
Commit messages should be concise, present-tense imperatives (history examples: “add a test…”, “remove old dir”). Squash incidental WIP commits locally; keep one logical change per commit. Pull requests need a short summary of why the change matters, links to any tracked issues, notes on manual verification, and screenshots or transcripts for UI or agent workflow impacts.

## Separation of concerns

- `apps/open-world-ingest` owns the NLP pipeline and exposes ingestion adapters; it should call into the shared storage layer rather than redefine persistence.
- `apps/graph-memory` provides the storage/graph primitives (entity/relation handling, XT utilities). Reuse these from other apps.
- `apps/basic-chat-demo` and `apps/headless-api` must remain thin: parse input, call shared adapters/protocols, format responses. Do **not** reimplement ingest logic.
- When adding capabilities, extend the feature module (e.g. open-world ingest or graph-memory) first, then delegate to it from the frontends.
