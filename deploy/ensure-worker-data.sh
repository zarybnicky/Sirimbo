#!/usr/bin/env bash
set -euo pipefail

directory=$1
mkdir -p "$directory"

if [[ ! -s "$directory/ruian.duckdb" ]]; then
  ruian=$(nix build .#ruian-address-cache --no-link --print-out-paths)
  cp "$ruian" "$directory/ruian.duckdb"
fi

if [[ ! -s "$directory/geonames.duckdb" ]]; then
  geonames=$(nix build .#geonames-city-cache --no-link --print-out-paths)
  cp "$geonames" "$directory/geonames.duckdb"
fi
