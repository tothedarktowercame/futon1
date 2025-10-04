# Repository Guidelines
## Project Structure & Module Organization
The repository hosts three standalone Clojure applications under `apps/`: `basic-chat-demo`, `graph-memory`, and `nlp-interface`. Each app keeps source in `src/`, shared runtime assets in `resources/`, and tests in `test/`; golden fixtures and scripted conversations live under `test/golden` and `test/scripts`. Module-specific reference notes belong in the local `doc/` directory—avoid leaking cross-app utilities into the repo root without consensus.

## Build, Test, and Development Commands
Run commands from the target app directory unless noted:
- `clojure -M:run-m` starts the module’s primary entry point.
- `clojure -M:test` executes the Cognitect test-runner suite (unit + scripted cases).
- `clojure -T:build test` runs the build namespace test task; `clojure -T:build ci` adds uberjar packaging and cleanup.
- `bb lint` (graph-memory only) wraps `clj-kondo --lint src test` for fast linting.

## Coding Style & Naming Conventions
Stick to idiomatic Clojure with 2-space indentation and align binding forms (`let`, `for`, `cond`). Namespace paths should mirror directories (e.g. `src/basic_chat_demo/basic_chat_demo.clj` -> `basic-chat-demo.basic-chat-demo`). Favor `kebab-case` for functions/vars, `SCREAMING_SNAKE_CASE` for constants, and `UpperCamelCase` only for records/types. Run `clj-kondo` before submitting; organize requires alphabetically and group `:refer` forms sparingly.

## Testing Guidelines
Create test namespaces that mirror production namespaces and end in `-test`. Keep deterministic fixtures in `test/golden` or `test/scripts`, and document scenario intent at the top of each test namespace. Always run `clojure -M:test` (and pertinent `bb` tasks) before opening a PR, ensuring new behavior has coverage or an explicit rationale when no test is practical.

## Commit & Pull Request Guidelines
Commit messages should be concise, present-tense imperatives (history examples: “add a test…”, “remove old dir”). Squash incidental WIP commits locally; keep one logical change per commit. Pull requests need a short summary of why the change matters, links to any tracked issues, notes on manual verification, and screenshots or transcripts for UI or agent workflow impacts.

## v2 NLP Hooks (follow-up tasks)
- Replace `*tokenize*` with a whitespace+punct tokenizer that returns a vector of tokens.
- Wire `*pos-tag*` to OpenNLP’s English POS model; ensure it returns `[[String String]]`.
- Implement `*wordnet*` using clj-wordnet so `(word :noun|:verb)` yields `[{ :gloss ...} ...]`.
