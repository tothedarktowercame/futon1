#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/archive_xt_snapshot.sh [--data-root DIR] [--dest DIR] [--label TAG]

Create a tarball containing the current XTDB store plus the Willie/Jane baseline fixture.
Defaults:
  --data-root   $SNAPSHOT_DATA_ROOT, $BASIC_CHAT_DATA_DIR, or data/default
  --dest        $SNAPSHOT_DEST or ~/code/storage/futon1-snapshots
  --label       Optional tag appended to the filename and metadata
USAGE
}

resolve_path() {
  python3 - "$1" <<'PY'
import os, sys
print(os.path.abspath(os.path.expanduser(sys.argv[1])))
PY
}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
DATA_ROOT=${SNAPSHOT_DATA_ROOT:-${BASIC_CHAT_DATA_DIR:-"$REPO_ROOT/data/default"}}
DEST=${SNAPSHOT_DEST:-"$HOME/code/storage/futon1-snapshots"}
LABEL=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --data-root)
      [[ $# -ge 2 ]] || { echo "--data-root requires a value" >&2; exit 1; }
      DATA_ROOT=$2
      shift 2
      ;;
    --dest)
      [[ $# -ge 2 ]] || { echo "--dest requires a value" >&2; exit 1; }
      DEST=$2
      shift 2
      ;;
    --label)
      [[ $# -ge 2 ]] || { echo "--label requires a value" >&2; exit 1; }
      LABEL=$2
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

DATA_ROOT=$(resolve_path "$DATA_ROOT")
DEST=$(resolve_path "$DEST")
XT_DIR="$DATA_ROOT/xtdb"
BASELINE_DIR="$REPO_ROOT/resources/baseline"
BASELINE_FILE="$BASELINE_DIR/demo_session.edn"

[[ -d "$DATA_ROOT" ]] || { echo "Data root not found: $DATA_ROOT" >&2; exit 1; }
[[ -d "$XT_DIR" ]] || { echo "XTDB directory not found: $XT_DIR" >&2; exit 1; }
[[ -f "$BASELINE_FILE" ]] || { echo "Baseline fixture missing: $BASELINE_FILE" >&2; exit 1; }

mkdir -p "$DEST"
STAMP=$(date -u +%Y%m%dT%H%M%SZ)
CREATED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
SAFE_LABEL=""
if [[ -n "$LABEL" ]]; then
  SAFE_LABEL=$(echo "$LABEL" | tr -cs 'A-Za-z0-9._-' -)
  SAFE_LABEL="-${SAFE_LABEL%-}"
fi
ARCHIVE_NAME="futon1-xtdb-${STAMP}${SAFE_LABEL}.tar.gz"
OUTPUT="$DEST/$ARCHIVE_NAME"
GIT_SHA=$(git -C "$REPO_ROOT" rev-parse --short HEAD 2>/dev/null || echo "unknown")
TMP_DIR=$(mktemp -d)
trap 'rm -rf -- "$TMP_DIR"' EXIT
cat > "$TMP_DIR/metadata.edn" <<META
{:created-at "$CREATED_AT"
 :git-sha "$GIT_SHA"
 :data-root "$DATA_ROOT"
 :label "${LABEL}"}
META

tar -czf "$OUTPUT" \
  -C "$DATA_ROOT" xtdb \
  -C "$BASELINE_DIR" demo_session.edn \
  -C "$TMP_DIR" metadata.edn

echo "Archived XT snapshot to $OUTPUT"
