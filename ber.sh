#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BER_DIR="$ROOT/ber"
WEB_DIR="$ROOT/web"
WEB_DIST="$WEB_DIR/dist"
WEB_PORT=8000
DO_WEB=false
DO_DEPLOY=false
DEPLOY_TARGET_DEFAULT="$ROOT/../julesjacobs.github.io/misc/ber"
DEPLOY_TARGET="$DEPLOY_TARGET_DEFAULT"
SERVER_PID=""

usage() {
  cat <<'EOF'
Usage: ./ber.sh [--web] [--web-port PORT] [--deploy [TARGET]]

Without flags, rewrites all .ber fixtures (default behavior).
With --web, also builds the wasm playground and serves web/dist on http://localhost:PORT/ (default 8000) then opens a browser.
With --deploy, builds the wasm playground, copies web/dist to the deploy target (default: ../julesjacobs.github.io/misc/ber), and pushes the deploy repo.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --web)
      DO_WEB=true
      shift
      ;;
    --web-port)
      shift
      if [[ $# -eq 0 ]]; then
        echo "--web-port requires a value" >&2
        usage
        exit 1
      fi
      WEB_PORT="$1"
      shift
      ;;
    --deploy)
      DO_DEPLOY=true
      if [[ $# -ge 2 && "$2" != --* ]]; then
        DEPLOY_TARGET="$2"
        shift 2
      else
        shift
      fi
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ ! -d "$BER_DIR" ]]; then
  echo "ber directory not found at $BER_DIR" >&2
  exit 1
fi

files=()
while IFS= read -r file; do
  files+=("$file")
done < <(find "$BER_DIR" -type f -name '*.ber' | sort)

if [[ ${#files[@]} -eq 0 ]]; then
  echo "No .ber files found in $BER_DIR"
else
  for file in "${files[@]}"; do
    dune exec ber-cli -- "$file" >/dev/null
  done
  echo "ber.sh finished. Please review git diffs for updated fixtures."
fi

ensure_web_dependencies() {
  if [[ ! -d "$WEB_DIR" ]]; then
    echo "web directory not found at $WEB_DIR" >&2
    exit 1
  fi
  if [[ ! -d "$WEB_DIR/node_modules" ]]; then
    echo "Installing npm dependencies in $WEB_DIR..."
    (cd "$WEB_DIR" && npm install)
  fi
}

build_web() {
  echo "Building wasm playground..."
  (cd "$WEB_DIR" && npm run build)
}

deploy_web() {
  local target="$1"
  mkdir -p "$target"
  rsync -a --delete "$WEB_DIST"/ "$target"/
  echo "Deployed web UI to $target"
}

push_deploy_repo() {
  local target="$1"

  if ! command -v git >/dev/null 2>&1; then
    echo "git not found; skipping deploy push." >&2
    return
  fi

  if ! git -C "$target" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "Deploy target $target is not a git repository; skipping push." >&2
    return
  fi

  local status_output
  status_output="$(git -C "$target" status --porcelain .)"
  if [[ -n "$status_output" ]]; then
    git -C "$target" add -A .
    git -C "$target" commit -m "Deploy ber web ($(date -u '+%Y-%m-%d %H:%M:%SZ'))" -- .
  else
    echo "No changes to commit in $target"
  fi

  git -C "$target" push
  echo "Pushed deploy repo at $target"
}

start_server() {
  local port="$1"
  local dist="$2"
  local py_cmd=""
  if command -v python3 >/dev/null 2>&1; then
    py_cmd="python3"
  elif command -v python >/dev/null 2>&1; then
    py_cmd="python"
  else
    echo "python3/python not found; cannot start web server" >&2
    exit 1
  fi
  echo "Starting local server on http://localhost:${port}/ (Ctrl+C to stop)..."
  "$py_cmd" -m http.server "$port" -d "$dist" >/tmp/ber_web_server.log 2>&1 &
  SERVER_PID=$!
  trap '[[ -n "${SERVER_PID:-}" ]] && kill "$SERVER_PID" 2>/dev/null || true' EXIT
}

open_browser() {
  local url="$1"
  if command -v open >/dev/null 2>&1; then
    open "$url" >/dev/null 2>&1 || true
  elif command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$url" >/dev/null 2>&1 || true
  else
    echo "Please open $url in your browser."
  fi
}

if [[ "$DO_WEB" == true || "$DO_DEPLOY" == true ]]; then
  ensure_web_dependencies
  build_web
fi

if [[ "$DO_DEPLOY" == true ]]; then
  deploy_web "$DEPLOY_TARGET"
  push_deploy_repo "$DEPLOY_TARGET"
fi

if [[ "$DO_WEB" == true ]]; then
  start_server "$WEB_PORT" "$WEB_DIST"
  open_browser "http://localhost:${WEB_PORT}/"
  wait "$SERVER_PID"
fi
