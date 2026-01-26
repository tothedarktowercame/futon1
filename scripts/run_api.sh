#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

DATA_DIR_DEFAULT="${ROOT}/data"
PROFILE_DEFAULT="default"
PORT_DEFAULT="8080"

FUTON3_ROOT=/home/joe/code/futon3

PENHOLDER="${MODEL_PENHOLDER:-${USER:-}}"
if [ -z "${PENHOLDER}" ]; then
  PENHOLDER="$(id -un 2>/dev/null || true)"
fi
if [ -z "${PENHOLDER}" ]; then
  echo "MODEL_PENHOLDER is required (set it or define USER)." >&2
  exit 1
fi

export BASIC_CHAT_DATA_DIR="${BASIC_CHAT_DATA_DIR:-${DATA_DIR_DEFAULT}}"
export ALPHA_PROFILE="${ALPHA_PROFILE:-${PROFILE_DEFAULT}}"
export ALPHA_PORT="${RUN_API_PORT:-${PORT_DEFAULT}}"
export APP_SERVER_PORT="${APP_SERVER_PORT:-${PORT_DEFAULT}}"
export MODEL_VERIFY_ON_WRITE="${MODEL_VERIFY_ON_WRITE:-true}"
export MODEL_PENHOLDER="${PENHOLDER}"

cd "${ROOT}/apps/api"
exec clojure -M:server
