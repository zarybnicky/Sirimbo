#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

file=deploy/geonames-city-cache.nix
updated=$(mktemp)
trap 'rm -f "$updated"' EXIT
cp "$file" "$updated"

for source in cities countries regions; do
  url=$(nix eval --raw ".#geonames-city-cache.$source.url")
  hash=$(nix store prefetch-file --refresh --json "$url" | jq -er '.hash')
  sed -i "/^  $source = fetchurl {/,/^  };/ s|hash = \".*\";|hash = \"$hash\";|" "$updated"
done

version=$(date -u +%Y%m%d)
sed -i "s/version = \"[0-9]*\";/version = \"$version\";/" "$updated"
cat "$updated" > "$file"
