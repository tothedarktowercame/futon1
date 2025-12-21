# Futon1 storage defaults are inconsistent

While auditing the Futon3 pattern import we found the real graph lives under
`~/code/futon1/data/default`, but various Futon1 entry points default to their own
`apps/<component>/data/<profile>` trees when `BASIC_CHAT_DATA_DIR` isn't set.

* `scripts/futon3_ingest.clj` and `scripts/inspect_pattern.clj` respect `BASIC_CHAT_DATA_DIR`
  (and were run with `/home/joe/code/futon1/data/default`).
* `apps/api` / `apps/demo` defaulted to `apps/api/data/default`, `apps/demo/data/default`, so any
  tooling that skipped the env var silently read/wrote empty stores.

This divergence is confusing for `arxana-patterns-browse` + Futon1 server coordination. We should
harmonize defaults so every entry point shares the same canonical data root (via the env var or a
common fallback) to avoid future "patterns=0" surprises.

## TODO

- Consider renaming `apps/graph-memory/src/futon1` (and clients that refer to it) to a clearer,
  externally meaningful name. Track follow-up in issue work before changing public APIs.
