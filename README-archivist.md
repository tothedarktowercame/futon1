# Archivist Guide (Using Futon1 Storage)

This guide explains how clients and tools should use Futon1 as a deterministic
storage substrate. For hydration details and internals, see `README-storage.md`.

## Quick start

```bash
cd /home/joe/code/futon1
export BASIC_CHAT_DATA_DIR=/home/joe/code/futon1/data
export ALPHA_PROFILE=default
clojure -M:server
```

## Common client workflows

### Ingest text (turns)

```bash
curl -s -X POST http://localhost:8080/api/alpha/turns \
  -H 'content-type: application/json' \
  -d '{"text":"Serena is presenting tomorrow.","protocol":"basic-chat/v5"}'
```

### Inspect focus/header state

```bash
curl -s http://localhost:8080/api/alpha/focus-header
curl -s http://localhost:8080/api/alpha/entities/latest?type=clock-out/summary
```

### Docbook entry ingest (single or batch)

```bash
curl -s -X POST http://localhost:8080/api/alpha/docs/futon4/entry \
  -H 'content-type: application/json' \
  -d '{"doc_id":"futon4-0cf1aad99fe8","entry_id":"futon4-0cf1aad99fe8::org","book_id":"futon4","timestamp":"2025-01-02T12:34:56Z","version":"v1","outline_path":["Quickstart"],"path_string":"Quickstart","context":"..."}'
```

```bash
curl -s -X POST http://localhost:8080/api/alpha/docs/futon4/entries \
  -H 'content-type: application/json' \
  -d '{"entries":[{"doc_id":"futon4-0cf1aad99fe8","entry_id":"futon4-0cf1aad99fe8::org","book_id":"futon4","timestamp":"2025-01-02T12:34:56Z","version":"v1","outline_path":["Quickstart"],"path_string":"Quickstart","context":"..."}]}'
```

## Model registry and queue

Use the registry to confirm what models are active and the queue to see pending
entity types that are not yet covered by a model descriptor.

```bash
curl -s http://localhost:8080/api/alpha/meta/model/registry
curl -s http://localhost:8080/api/alpha/meta/model/queue
```

## Requesting a new database type

Default to existing types and open-world labels unless the data requires new
invariants or lifecycle rules.

Include the following in a type request:
- Proposed type (namespaced, e.g. `media/asset`).
- Why existing types are insufficient.
- Required fields and invariants.
- Store role (xtdb canonical? filesystem working copy?).
- Example payloads and expected volume.

Once approved, add the type to a model descriptor, bootstrap it, and confirm it
appears in `/meta/model/registry`.
