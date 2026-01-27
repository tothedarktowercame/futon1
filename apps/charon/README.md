# Charon (Ingest Gatekeeping)

Charon provides a shared, pure gatekeeping layer for Futon1 ingest. It defines
a standard rejection envelope and helpers that clients use to abort ingest
before writing invalid data. The package is deliberately envelope-only by
design; validation lives in the ingesting app.

## Contract

Charon returns a consistent map with an `:ok?` flag:

- `{:ok? true ...}` for acceptable ingest.
- `{:ok? false :error :charon/reject :surface ... :reason ... :details ...}`
  when ingest should be rejected.

Use `charon.core/ensure-ok` to throw on rejection.

## Integration

Callers are expected to:

1. Run payload validation/gating logic before persistence.
2. Return a Charon envelope on failure.
3. Abort ingest (throw or return HTTP 409) when `:ok?` is false.

For context on how this fits into the broader Futon1 workflow, see
`README-archivist.md`.

## Current integrations

- Open-world ingest
  - `apps/open-world-ingest/src/open_world_ingest/storage.clj`
  - `apps/open-world-ingest/src/open_world_ingest/adapters/interface.clj`
  - Uses `reject` + `ensure-ok` to gate writes.
- Docbook ingest
  - `apps/graph-memory/src/app/docbook.clj`
  - Returns Charon envelopes for invalid entries.
- Pattern ingest
  - `apps/graph-memory/src/scripts/futon3_ingest.clj`
  - `apps/graph-memory/src/scripts/ingest_futon3_batch.clj`
  - Rejects and aborts before persistence.
- API
  - `apps/api/src/api/handlers/turns.clj`
  - Returns HTTP 409 for open-world rejects.

## Penholder registration

Charon can be registered as a strict penholder for core descriptors:

```bash
cd /home/joe/code/futon1/apps/graph-memory
clojure -M -m scripts.charon-penholder-bootstrap
```
