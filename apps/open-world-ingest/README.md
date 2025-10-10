# Open-World Ingest Module

The futon1 v5 command-line tool streams conversational text into XTDB using
Stanford CoreNLP for entity and relation extraction. Each input line creates a
new utterance record, merges noun-phrase entities using deterministic SHA-1
identifiers, and persists OpenIE triples as relations between the detected
entities.

## Usage

```bash
cd apps/open-world-ingest
clojure -M:run-m
```

### Options

- `--data-dir DIR` – override the XTDB RocksDB directory (defaults to
  `data/open-world`). Directories are created automatically when missing.
- `--config PATH` – provide an explicit XTDB configuration EDN. Paths are
  resolved before passing through to XT, so relative references inside the file
  continue to work when combined with `--data-dir`.

XTDB stores data under `data/open-world` by default. Override with
`--data-dir` or set a different RocksDB config via `--config`.

## Data model

Entities use deterministic IDs: `sha1(lowercase(label) ":" kind)`. Relations
store OpenIE verb phrases as lowercase lemmas and record negation. Every noun
phrase mention is persisted for co-occurrence analysis, while ego traversals
follow the relation graph in both directions.
