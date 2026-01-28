# Charon: Invariant Gatekeeping

Charon is the guardrail layer that makes model invariants *real* invariants in Futon1.
It wraps all write paths that matter (API ingest, internal store writes, and CLI repair
scripts) and rejects any change that would violate a registered model invariant or
violate penholder authorization.

This document explains the enforcement chain and why invariants should now hold in
steady state.

## What Charon is

- A lightweight gatekeeper (`apps/charon/src/charon/core.clj`) that:
  - normalizes success/failure envelopes (`ok`, `reject`)
  - enforces guard decisions (`guard`, `guard!`, `ensure-ok`)
- A guardian implementation (`apps/graph-memory/src/app/charon_guard.clj`) that:
  - runs model invariants before writes (`guard-event` / `guard-models`)
  - enforces penholder authorization

## Where invariants are enforced

### 1) Datascript event writes (core store)

`app.store/tx!` is the single point for Datascript writes. It uses a `:verify-fn`
that now points at `charon-guard/guard-event`.

This means **every** event write is checked against the relevant model invariants
before it is applied.

### 2) Open-world ingest (XTDB)

`open_world_ingest/storage.clj` now runs Charon guard for the
`:open-world-ingest` model when a Datascript connection is available.
The API passes this connection, so ingest is guarded in production.

### 3) Internal scripts and repair tools

All maintenance scripts that write to XTDB now call:

```
(charon-guard/guard-models! conn [<models>] env <surface>)
```

This includes open-world repair/backfill, docbook repair, descriptor cleanup,
XTDB pruning, and related tools. These scripts require a valid penholder.

### 4) API endpoints that bypass store

`/lab/session` ingest uses `app.xt/put-entity!` directly. It now performs a
Charon guard check before writing.

## Penholder enforcement (authorization)

Charon is **not** a penholder. The only standard penholders are:

- `api`
- `$USER` (the local Unix user, if present)
- `cli` (optional, for scripts)

Penholder registry entries are mandatory for every model. If a model has no
penholder entry, **writes are rejected** (except when bootstrapping the penholder
registry itself).

Default bootstrap script:

- `apps/graph-memory/src/scripts/charon_penholder_bootstrap.clj`
  - registers penholders for core model descriptors
  - strict by default

To run scripts, set one of:

```
MODEL_PENHOLDER=cli
# or
BASIC_CHAT_PENHOLDER=cli
```

## Why this makes invariants real

1) **All core write paths are guarded** (store, ingest, scripts, lab ingest).
2) **Invariants are checked before each write**; a violating write is rejected.
3) **Penholder authorization is required**; unauthorized writers are rejected.
4) **Missing penholder registry entries now fail**; silent bypass is gone.

As long as writes go through these guarded paths (which they do for the app and
official scripts), invariants should be upheld continuously.

## Remaining non-goals / escape hatches

- Manual XTDB writes outside the app can still bypass enforcement.
- Untracked scripts outside this repo can bypass enforcement.

Those are treated as out-of-band admin actions. If you want to eliminate those
as well, wrap all raw XTDB access in a guarded API and remove direct XTDB calls.

## Quick sanity checks

- Server startup logs should show: `[store] Invariants OK`.
- Ingest requests should return HTTP 409 with `Model invariants failed` when
  a violation would occur.
- Scripts should fail fast if `MODEL_PENHOLDER` is not authorized.
