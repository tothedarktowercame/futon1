# api

`api` exposes the deterministic Futon pipeline over HTTP without
LLM calls. It depends on the shared `app.*` namespaces housed in
`apps/graph-memory` (Datascript with XTDB salience) and provides JSON/text
endpoints that align with the focus-header UX from the demo client.

## Running the server

The app-local alias is `:server`:

```bash
cd apps/api
clojure -M:server
```

From the repository root you can use the `:api` alias:

```bash
clojure -M:api
```

Environment variables:

- `ALPHA_PORT` – listen port (default `8080`).
- `ALPHA_PROFILE` – default profile name when `X-Profile` header is absent.
- `BASIC_CHAT_DATA_DIR` – root data directory (set this explicitly; see below).
- `BASIC_CHAT_XTDB_RESOURCE` / `BASIC_CHAT_XTDB_ENABLED` – override XTDB config.

## Example workflow

```bash
# Start the service
clojure -M:api

# Send a turn and persist salience
curl -s -X POST localhost:8080/api/alpha/turns \
  -H 'Content-Type: application/json' \
  -d '{"text":"Serena is presenting tomorrow.","protocol":"basic-chat/v5"}' | jq .

# Inspect the focus header (all responses include X-API-Version: α)
curl -s -i localhost:8080/api/alpha/focus-header | grep X-API-Version

# Structured profile document
curl -s localhost:8080/api/alpha/me | jq .

# Distilled summary (defaults to 2000 chars)
curl -s localhost:8080/api/alpha/me/summary?limit_chars=2000
```

All routes are mounted twice: `/api/α/...` is canonical while `/api/alpha/...`
provides an ASCII alias for proxies or clients that cannot emit unicode paths.

> **Important:** The server must not write under `apps/api/data/` in production.
> Externalise the data root before you start the API:
>
> ```bash
> export BASIC_CHAT_DATA_DIR=$HOME/.local/share/futon1
> mkdir -p "$BASIC_CHAT_DATA_DIR"
> export BASIC_CHAT_XTDB_RESOURCE=resources/xtdb.edn
> ```
>
> The default in-repo `data/` folder exists only for tests and should be
> deleted if you accidentally spill real data there.

## Routes

| Method & Path | Description |
|---------------|-------------|
| `POST /api/α/turns` | Run the deterministic pipeline (`{text, ts?, source?, protocol?, actor}`) and return entities, relations, intent, context, and `focus_header` (`actor` or `X-Actor` required to map first-person pronouns). |
| `GET /api/α/focus-header` | Return the current focus header. Accepts `focus_days` and `allow_works` query parameters. |
| `GET /api/α/me` | Fetch the structured profile document for the active profile. |
| `POST /api/α/me` | Shallow merge the provided map into the profile (`Content-Type: application/json`). |
| `GET /api/α/me/summary` | Return a text summary (defaults to `limit_chars=2000`). |
| `POST /api/α/ingest` | Bulk ingest plain text or NDJSON turns. Optional header `X-Chunking: sentences`. |
| `POST /api/α/entity` | Programmatic entity ensure (`{name, type?}`). |
| `POST /api/α/relation` | Programmatic relation upsert (`{type, src, dst}`). |
| `POST /api/α/docs/:book/entry` | Upsert a single docbook entry (idempotent by `doc/entry-id`; heading is upserted too). |
| `POST /api/α/docs/:book/entries` | Upsert a batch of docbook entries (idempotent by `doc/entry-id`). |
| `GET /api/α/affect-labels` | Experimental label telemetry from open-world utterances (dictionary intents). Query: `since`/`until` (ISO or ms), `lookback_hours`, `limit`, `actor_id`. |
| `GET /api/α/affect-transitions` | Heuristic affect→consequence transitions using novel terms near affect utterances (requires `actor_id`). Query: `since`/`until`, `lookback_hours`, `actor_id`, `lookahead_minutes`, `lookahead_utterances`, `novelty_days`, `limit`, `max_transitions`, `max_terms`. |
| `GET /api/α/entities/latest?type=` | Return the most recently seen entities of the requested type (e.g. `clock-out/summary`). Optional `limit` query parameter controls the count. |
| `GET /api/α/types` | List registered entity/relation types, parents, and aliases. |
| `POST /api/α/types/parent` | Override or clear a type's parent (`{type, parent?, kind?}`). |
| `POST /api/α/types/merge` | Merge aliases into a canonical type (`{into,type?,aliases}` accepts strings or keywords). |
| `GET /api/α/patterns/registry` | Return a lightweight pattern registry for fast sync diffs (computed on request; candidate for persistence similar to docbook TOC). |
| `GET /api/α/meta/model` | Return the model registry (patterns, docbook, media, open-world ingest, meta-model). |
| `GET /api/α/meta/model/registry` | Alias of `/meta/model`. |
| `GET /api/α/meta/model/queue` | Summarize covered vs pending XTDB entity types based on the model registry. |
| `GET /api/α/meta/model/media` | Return the media model descriptor. |
| `GET /api/α/meta/model/media/verify` | Run media model invariants. |
| `GET /api/α/meta/model/meta` | Return the meta-model descriptor for `model/descriptor` entities. |
| `GET /api/α/meta/model/meta/verify` | Run meta-model invariants over stored model descriptors. |
| `DELETE /api/α/docs/:book/doc/:doc-id` | Delete a docbook heading plus its entries for the given book/doc-id. |
| `DELETE /api/α/docs/:book/toc/:doc-id` | Delete a docbook heading only; pass `?cascade=true` to also delete entries. |

Each endpoint also accepts `/api/alpha/...` as an alias.

Note: the API does not expose a global delete for arbitrary XTDB documents; deletion is scoped to docbook content only.

Relation payloads can omit `type` if you provide `props.relation/type`, `props.link/type`, `props.pattern/type`, or `props.label` (string/keyword); the handler normalizes these into `type` before upserting.

### Type registry

Entity and relation types automatically register in XTDB with lightweight parent
inference. Namespaced types such as `:work/project` inherit from `:work/*`, while
top-level keywords consult `resources/type_namespace_map.edn`. The registry keeps
aliases and ancestry in sync, so selectors like `:work/*` expand to the concrete
types when computing salience or filtering results.

Use the `/api/α/types` endpoints to inspect or adjust parents and aliases. For
example, to query all work-related entities you can request the focus header or
neighbors with an allowed-type selector of `:work/*`; the registry resolves
every descendant before the salience heuristics run.

## Testing

```bash
cd apps/api
clojure -M:test -m cognitect.test-runner
```
