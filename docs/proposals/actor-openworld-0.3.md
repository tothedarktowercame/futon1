# Proposal 0.3 — Actor‑centric Open‑World Pipeline

Status: draft  
Scope: futon1 API + open‑world ingest + graph‑memory  
Owner: Joe

## Changelog
- 0.3: added context model (separate from locale), with spatialized context option.
- 0.2: added locale/addressable place model for utterances; updated invariants.
- 0.1: initial actor‑centric open‑world pipeline proposal.

## Goals
- Stable, explicit actor identities (one actor per turn).
- Open‑world ingest is the canonical store for utterances + entities.
- Graph‑memory focuses on relations and query convenience, derived from open‑world data.
- Summaries become actor‑scoped (not just `:me`), computed from canonical data.
- Utterances are addressable in a locale (chat thread, encyclopedia entry, forum thread).

## Non‑goals (v0.3)
- Multi‑actor turns.
- Full historical backfill or migration of legacy data.
- Perfect cross‑store normalization (aliases, dedupe) on day one.

## Current situation (high‑level)
- `/turns` uses open‑world NLP for extraction but persists **graph‑memory** entities/relations.
- Open‑world ingest storage is not treated as canonical for API traffic.
- “Me summary” pulls from graph‑memory salience and XTDB relations.

## Proposal

### 1) Actor model (required on turns)
- Each turn includes exactly one actor identity.
- Actor identities are **stable** and **explicit** (e.g., `:agent/joe`, `:agent/claude`).
- Actors are stored in the canonical open‑world dataset and referenced by utterances.

Example input:
```json
{
  "text": "I shipped the build.",
  "actor": {"id": ":agent/joe", "name": "Joe Corneli", "type": "person"}
}
```

### 2) Canonical store
- Open‑world ingest is the canonical record of:
  - Utterances
  - Extracted entities
  - Mentions
  - Relations
  - Actor references on utterances
- Graph‑memory is a **derived** view focused on relations and query ergonomics.

### 3) Graph‑memory derivation
- Derived from open‑world relations on ingest (or in batch).
- Entities are optional in graph‑memory except for:
  - Actor entity
  - Any entity needed for relation endpoints/queries
- Avoid dual‑write of entity payloads where possible.

### 4) Actor‑scoped summaries
- `/me/summary` becomes `/actor/summary` or `/me/summary?actor=...`.
- Summaries should use:
  - Open‑world utterances (canonical text + actor)
  - Graph‑memory relations (salience, co‑occurrence)
- Actor selection drives “first‑person” resolution and summary content.

### 5) Locale model
- Every utterance is anchored in an addressable locale.
- Locale carries at least a `:locale/type` and `:locale/id`.
- Locale topology can vary by type (linear chat vs nested forum threads).

Example locale fields:
```json
{
  "locale": {
    "type": "chat-thread",
    "id": "thread:alpha-2025-01",
    "path": ["root", "thread:alpha-2025-01"]
  }
}
```

### 6) Context model (separate from locale)
- Context is the active interpretive frame, not just the place where speech occurs.
- Context references prior utterances, segments, and artifacts to make inference possible.
- Context can be spatialized (graph geometry) to model proximity beyond linear order.

Example context fields:
```json
{
  "context": {
    "id": "ctx:proposal-0.2-thread",
    "refs": ["turn:1001", "turn:1002", "artifact:proposal-0.2"],
    "window": {"type": "sliding", "size": 12},
    "geometry": {"space": "embedding-2d", "coords": [0.42, -0.18]}
  }
}
```

## Invariants (v0.3)
- Every `/turns` request must include a valid actor.
- Every open‑world utterance must record `:utterance/actor-id` (and optional name/type).
- Actor IDs must be stable and resolvable (no empty/anonymous actors).
- Every utterance must record a locale reference (`:utterance/locale-type`, `:utterance/locale-id`).
- Context is optional at ingest, but when present it must reference addressable items.

## Migration / rollout
1) Enable required actor on `/turns`.
2) Write open‑world ingest docs for all new turns.
3) Start deriving graph‑memory relations from open‑world.
4) Introduce actor‑scoped summary endpoint.
5) Add locale annotations to new ingest paths.
6) Add context annotations to new ingest paths.

## Open questions
- Should actor IDs be validated against a registry endpoint?
- Do we need a dedicated actor type (`:agent/*`) or reuse `:person`?
- Should graph‑memory retain entities for performance, or treat it as “relations only”?
- How do we model nested locale paths (forum threads, replies, subthreads)?
- What is the minimal context reference set that still enables good inference?
