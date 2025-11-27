#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BER_DIR="$ROOT/ber"

if [[ ! -d "$BER_DIR" ]]; then
  echo "ber directory not found at $BER_DIR" >&2
  exit 1
fi

files=()
while IFS= read -r file; do
  files+=("$file")
done < <(find "$BER_DIR" -type f -name '*.ber' ! -path "*/challenges/*" | sort)

if [[ ${#files[@]} -eq 0 ]]; then
  echo "No .ber files found in $BER_DIR"
  exit 0
fi

for file in "${files[@]}"; do
  echo "Processing $file"
  dune exec ber-cli -- "$file"
  echo "----- $file -----"
  cat "$file"
  echo
done
