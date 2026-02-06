#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/env_fingerprint.sh [--full]

Print a minimal environment fingerprint for Futon1 debugging.

Options:
  --full   include git status/details and clojure -Sdescribe output
USAGE
}

full=false
for arg in "$@"; do
  case "$arg" in
    --full) full=true ;;
    --help|-h) usage; exit 0 ;;
    *) echo "Unknown arg: $arg" >&2; usage; exit 1 ;;
  esac
  shift || true
  break
 done

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel 2>/dev/null || true)"

section() {
  printf "\n## %s\n" "$1"
}

kv() {
  printf "%-28s %s\n" "$1" "$2"
}

section "timestamp"
kv "date" "$(date -Iseconds)"

section "repo"
kv "root" "${repo_root:-<not a git repo>}"
if [[ -n "$repo_root" ]]; then
  kv "branch" "$(git -C "$repo_root" rev-parse --abbrev-ref HEAD)"
  kv "head" "$(git -C "$repo_root" rev-parse HEAD)"
  kv "dirty" "$(git -C "$repo_root" diff --quiet && echo no || echo yes)"
  kv "untracked" "$(git -C "$repo_root" status --porcelain | awk '/^\?\?/{c++} END{print c+0}')"
fi

section "runtime"
kv "uname" "$(uname -a)"
kv "java" "$(java -version 2>&1 | head -n 1 | sed 's/^[[:space:]]*//')"
if command -v clojure >/dev/null 2>&1; then
  kv "clojure" "$(clojure -e '(println (clojure-version))' 2>/dev/null | head -n 1)"
else
  kv "clojure" "<not found>"
fi

section "env"
for key in BASIC_CHAT_DATA_DIR ALPHA_PROFILE FUTON1_API XTDB_CONFIG FUTON3_ROOT JAVA_HOME; do
  val="${!key-<unset>}"
  kv "$key" "$val"
  if [[ "$key" == "BASIC_CHAT_DATA_DIR" && "$val" != "<unset>" ]]; then
    if [[ -d "$val" ]]; then
      kv "BASIC_CHAT_DATA_DIR.exists" "yes"
      kv "BASIC_CHAT_DATA_DIR.contents" "$(ls -1 "$val" 2>/dev/null | tr '\n' ' ' | sed 's/[[:space:]]*$//')"
    else
      kv "BASIC_CHAT_DATA_DIR.exists" "no"
    fi
  fi
  if [[ "$key" == "XTDB_CONFIG" && "$val" != "<unset>" ]]; then
    kv "XTDB_CONFIG.exists" "$(test -f "$val" && echo yes || echo no)"
  fi
 done

if [[ "$full" == true && -n "$repo_root" ]]; then
  section "git-status"
  git -C "$repo_root" status --short

  section "git-diff-stat"
  git -C "$repo_root" diff --stat || true

  section "git-remotes"
  git -C "$repo_root" remote -v

  if command -v clojure >/dev/null 2>&1; then
    section "clojure-sdescribe"
    clojure -Sdescribe || true
  fi
fi
