# Protocol history

This document preserves the legacy notes for the earlier `basic-chat` protocols.
The primary README now focuses on v5 and the focus-header workflow.

## basic-chat/v1 – intent + utterance logging

- Minimal pipeline: intent classification and utterance capture.
- Scripted demo: `test/scripts/hello.edn` → golden output
  `test/golden/hello.out.edn`.
- Useful flags: `--script`, `--reset`, `--compact`, `--export edn`.

## basic-chat/v2 – entity aggregation + POS tagging

- Adds deterministic POS tagging and entity aggregation for labels.
- Golden script: `test/scripts/v2-basic.edn` with fixture
  `test/golden/v2-basic.out.edn`.
- Interactive extras: `/diff` (new labels), `/dump` (graph snapshot).

## basic-chat/v3 – entity + relation listing

- Integrates NER and relation extraction so the CLI can list entities/relations
  discovered so far.
- Flags:
  - `--list-entities` – print all known entities after the run.
  - `--links "Name"` – show direct neighbours for the given entity.

## basic-chat/v4 – deterministic NER stack with fallback

- Adds gazetteer + pattern recogniser + optional conservative fallback
  (`--ner-fallback`).
- Script: `test/scripts/basic-chat/v4/entities.edn` with fixture
  `test/golden/basic-chat/v4/entities.out.edn`.
- Continues to support `/links`, `!entity`, `!rel` for manual curation.

## XT + focus header transition (v5)

- XTDB mirroring introduced; Datascript hydrates from XT first, then legacy logs.
- Focus header rendering added to CLI (`--fh`, `--fh-only`).
- Policy controls surfaced: `--focus-days`, `--allow-works`.
- Emacs/HTTP clients consume the same focus header payload.

Refer back to `apps/basic-chat-demo/README.md` for the current v5 workflow.
