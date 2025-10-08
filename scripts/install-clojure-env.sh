#!/usr/bin/env bash
set -euo pipefail

CLJ_VERSION="1.11.1.1413"
CLJ_INSTALLER="linux-install-${CLJ_VERSION}.sh"
CLJ_DOWNLOAD_URL="https://download.clojure.org/install/${CLJ_INSTALLER}"

if [[ $EUID -ne 0 ]]; then
  echo "This script must be run with sudo/root privileges." >&2
  exit 1
fi

apt-get update
apt-get install -y curl git rlwrap openjdk-21-jre-headless

curl -fSL "$CLJ_DOWNLOAD_URL" -o "$CLJ_INSTALLER"
chmod +x "$CLJ_INSTALLER"
./"$CLJ_INSTALLER"
rm -f "$CLJ_INSTALLER"

echo "Clojure installation complete. Verify with:"
echo "  clojure -Sdescribe"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
DEPS_TARGETS=(
  "$ROOT_DIR/protocols"
  "$ROOT_DIR/apps/basic-chat-demo"
  "$ROOT_DIR/apps/graph-memory"
  "$ROOT_DIR/apps/nlp-interface"
)

for project_dir in "${DEPS_TARGETS[@]}"; do
  if [[ -f "$project_dir/deps.edn" ]]; then
    echo "Prefetching Clojure deps for $project_dir"
    (cd "$project_dir" && clojure -Sforce -P)
  fi
done

echo "Dependency prefetch complete."
