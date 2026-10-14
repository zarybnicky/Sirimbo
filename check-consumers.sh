#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
PATH="$(pnpm bin):$PATH"

SCHEMA=$(realpath schema.graphql)
if [ "$#" -gt 0 ]; then
  SCHEMA=$1
  shift
fi
if [ "$#" -eq 0 ]; then
  set -- latest main
fi

REPO=toby-gamez/TKOlympApp
OPERATIONS='*/shared/src/commonMain/graphql/operations/*.graphql'

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

STATUS=0
for REF in "$@"; do
  if [[ "$REF" == latest ]]; then
    REF=$(gh release view --repo "$REPO" --json tagName --jq .tagName)
  fi
  SHA=$(gh api "repos/$REPO/commits/$REF" --jq .sha)
  echo "$REF ($SHA) against $SCHEMA"

  if [[ ! -d "$TMP/$SHA" ]]; then
    mkdir "$TMP/$SHA"
    gh api "repos/$REPO/tarball/$SHA" |
      tar -xz -C "$TMP/$SHA" --wildcards "$OPERATIONS"
  fi
  pushd "$TMP/$SHA"
  if ! graphql-inspector validate "**/*.graphql" "$SCHEMA"; then
    STATUS=1
  fi
  popd
done
exit "$STATUS"
