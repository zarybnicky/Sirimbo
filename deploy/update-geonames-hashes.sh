#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

file=deploy/geonames-city-cache.nix
updated=$(mktemp)
trap 'rm -f "$updated"' EXIT
cp "$file" "$updated"
version=$(date -u +%Y%m%d)

for source in cities countries regions; do
  url=$(nix eval --raw ".#geonames-city-cache.$source.url")
  current_hash=$(nix eval --raw ".#geonames-city-cache.$source.outputHash")
  hash=$(nix store prefetch-file --refresh --json "$url" | jq -er '.hash')
  if [ "$hash" != "$current_hash" ]; then
    sed -i "/^  $source = fetchurl {/,/^  };/ {
      s|\(name = \".*-\)[0-9]*\.|\1$version.|
      s|hash = \".*\";|hash = \"$hash\";|
    }" "$updated"
  fi
done

if ! cmp -s "$file" "$updated"; then
  sed -i "s/version = \"[0-9]*\";/version = \"$version\";/" "$updated"
  cat "$updated" > "$file"
fi
