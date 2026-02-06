# Monotonicity: Durable vs Volatile Type Counts

This document describes the **monotonicity guard** that detects cases where
writes hit the in-memory Datascript store but fail to persist to XTDB.

The goal is not to block startup, but to **surface drift early** and provide
enough signal to prevent silent data loss.

## Background

Futon1 maintains two storage layers:

- **XTDB** (durable, canonical)
- **Datascript** (volatile cache, hydrated from XTDB on boot)

If a codepath updates Datascript but does not mirror to XTDB, the system can
appear healthy until a restart, at which point data is missing. The
monotonicity guard detects this by comparing counts from both layers.

## What We Measure

We compute **type counts** in two ways:

- **Durable census**: derived directly from XTDB.
- **Volatile census**: derived from the live Datascript connection.

Counts include:

- `:total` entities
- `:by-type` counts (per `:entity/type`)
- `:untyped` (entities missing `:entity/type`)

## Invariant Rules

1. **Durable counts are non-decreasing**
   - `durable_now >= durable_baseline` for each type and total.
   - Baseline is stored per profile in `metadata-root/type-counts.edn`.

2. **Volatile must match durable**
   - If `volatile_now != durable_now`, we log a warning.
   - This is the direct signal for “writes hit Datascript but not XTDB.”

## Where It Runs

- **Startup** (after hydration + invariants)
  - Logs both censuses.
  - Warns if counts diverge.
  - Updates the durable baseline.

- **Shutdown**
  - Logs both censuses.
  - Warns if counts diverge.
  - Updates the durable baseline.

## What It Does Not Do

- It does **not** block startup.
- It does **not** fail writes immediately (warnings only).
- It does **not** attempt to reconcile or repair.

The intent is to make drift visible so we can fix the responsible codepath.

## Operational Notes

- If you intentionally delete entities, the baseline will regress.
  - Delete `metadata-root/type-counts.edn` to reset.
- If XTDB is unavailable at startup/shutdown, durable counts are skipped.
- If a profile’s data-dir changes, the baseline is skipped.

## Implementation Pointers

- XTDB census helper: `apps/graph-memory/src/app/xt.clj` (`entity-type-counts`)
- Census + baseline logic: `apps/graph-memory/src/app/type_counts.clj`
- Startup/shutdown logging: `apps/graph-memory/src/app/store_manager.clj`
- Meta-model invariant: `apps/graph-memory/src/app/model_meta.clj`
