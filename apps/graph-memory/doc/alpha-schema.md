# Alpha schema

This document captures the **alpha** graph schema that underpins every app in
`futon1`. The same attribute vocabulary is shared by the in-memory Datascript
store (`graph_memory.main/schema`) and the XTDB documents mirrored by
`app.store`. Slash commands and the `/api/α/*` endpoints are thin adapters over
this core definition.

The schema is intentionally compact: utterances describe what was said, mentions
link utterances to canonical entities, relations connect entities, and links
provide lighter-weight co-occurrence edges. Salience metadata (`last-seen`,
`seen-count`, `pinned?`) and the type registry round out the core state that
focus headers, memory retrieval, and API clients rely on.

## Storage layers at a glance

| Layer | Purpose | Source files |
| --- | --- | --- |
| Datascript | Low-latency runtime graph queried every turn; enforces the schema below. | `apps/graph-memory/src/graph_memory/main.clj` |
| XTDB | Durable, bitemporal mirror of entities/relations/type docs; feeds hydration and registry APIs. | `apps/graph-memory/src/app/store.clj`, `apps/graph-memory/src/app/xt.clj`, `apps/graph-memory/src/graph_memory/types_registry.clj` |

`app.store` persists Datascript mutations into an append-only event log plus
snapshots, and mirrors key documents into XTDB so the next session can hydrate
without replaying every event.

## Entity records (`:entity/*`)

| Attribute | Type | Notes |
| --- | --- | --- |
| `:entity/id` | UUID (identity) | Primary key; also used as `:xt/id` when mirrored to XTDB. |
| `:entity/name` | string (identity) | Canonical display name; unique within a profile. |
| `:entity/type` | keyword | Semantic type; must exist in the type registry (see below). |
| `:entity/last-seen` | ms epoch (indexed) | Updated whenever the entity participates in a relation. |
| `:entity/seen-count` | integer | Salience counter used by focus headers. |
| `:entity/pinned?` | boolean | Forces inclusion in focus headers / activation sets. |

These attributes live entirely inside Datascript but are mirrored to XTDB by
`xt-entity->tx`; hydration uses them to rebuild the in-memory graph after a
restart.

## Relation records (`:relation/*`)

| Attribute | Type | Notes |
| --- | --- | --- |
| `:relation/id` | UUID (identity) | Primary key / XT document id. |
| `:relation/type` | keyword | Directed edge label; registered under kind `:relation`. |
| `:relation/src` | ref → entity | Source entity reference. |
| `:relation/dst` | ref → entity | Destination entity reference. |
| `:relation/provenance` | map/string | Optional provenance payload recorded by ingestion. |
| `:relation/confidence` | double | Optional model confidence. |
| `:relation/last-seen` | ms epoch (indexed) | Used for salience decay and `/tail`. |

Relations are the only edges considered when building salience summaries and
responding to `/ego`, `/tail`, and `/relation` commands.

## Utterances, mentions, and intents

| Namespace | Key attributes | Purpose |
| --- | --- | --- |
| `:utterance/*` | `:utterance/id`, `:utterance/text`, `:utterance/ts`, optional `:utterance/intent`, `:utterance/intent-conf`, `:utterance/intent-source` | Captures the raw text and detected intent for each user or model turn. |
| `:intent/*` | `:intent/id`, `:intent/data` (original classifier payload), optional `:intent/type`, `:intent/confidence`, `:intent/source`, `:intent/candidates` | Stores structured intent outputs that may be reused across turns. |
| `:mention/*` | `:mention/id`, refs to `:utterance/id` and `:entity/id`, `:mention/span` | Anchors an entity mention back to the utterance text. |

Utterances and intents ensure their referenced `:intent/type` values exist in the
registry before persisting. Mentions allow the focus header builder to connect
the latest language back to canonical entities.

## Link records (`:link/*`)

| Attribute | Type | Notes |
| --- | --- | --- |
| `:link/id` | UUID (identity) | Primary key. |
| `:link/type` | keyword | Lightweight edge label (`:link/*` is not mirrored to XTDB today). |
| `:link/from` | ref | Arbitrary reference (often a Datascript tempid). |
| `:link/to` | ref | Arbitrary reference. |

Links are opportunistic co-occurrence edges that do not enforce entity refs; the
schema keeps them distinct from `:relation/*` to avoid confusing heuristics with
first-class relations.

## Type registry (entities, relations, intents)

Types are **data**, not schema. `graph_memory.types_registry` persists docs in
XTDB with the following fields:

| Field | Meaning |
| --- | --- |
| `:type/id` | Keyword name of the type (e.g. `:person`, `:project/chapter`). |
| `:type/kind` | One of `:entity`, `:relation`, `:intent`. |
| `:type/parent` | Optional parent keyword; defaults to namespace wildcards (`foo/*`). |
| `:type/inferred-parent?` | Boolean flag indicating whether the parent was inferred. |
| `:type/aliases` | Vector of keyword/string aliases that normalize to this type. |
| `:type/alias-of` | When present, marks this row as an alias of the canonical type. |

Whenever the runtime encounters a new type (entity kind, relation label, or
intent), it calls `types/ensure!` to create the doc, infer parents, and wire
alias scaffolding. XTDB stores the registry so `/types` requests, API callers,
and future migrations have a consistent vocabulary.

### `/types` command vs. alpha schema

The `/types` slash command (and `GET /api/α/types`) simply renders the current
registry snapshot for the active profile. Because users can add new entity or
relation types on the fly, the output is profile-specific and may include custom
entries. The **alpha schema** defined above remains the baseline: it describes
which attributes exist for entities, relations, utterances, mentions, intents,
and links, while `/types` shows the **instances** of types observed so far.

Sub-commands `/types parent` and `/types merge` mutate the registry by overriding
parents or marking aliases. These operations do not change the Datascript
schema—they only affect how type metadata is interpreted and displayed.

## XTDB documents

XTDB mirrors only the entity, relation, and type registry documents. The mapper
functions `xt-entity->tx` and `xt-relation->tx` convert XT docs into Datascript
transactions during hydration, ensuring the attribute-level schema above remains
consistent across restarts. Additional metadata (profiles, salience summaries)
resides alongside the data directory and is not part of the alpha schema.

## Future extensions

New modules should treat this document as the canonical “alpha” snapshot. When
adding attributes or new namespaces:

1. Update `graph_memory.main/schema` and its tests.
2. Mirror the change in `app.store` (hydration + XT mapping) if the data needs
   to persist across restarts.
3. Extend this document with the new attributes plus any relationship to the
   type registry.

Keeping the schema definition centralized ensures CLI, API, and demo apps evolve
in lockstep and the `/types` UX stays grounded in the same vocabulary.
