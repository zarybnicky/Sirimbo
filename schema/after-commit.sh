#!/usr/bin/env bash

set -eu
cd "$(dirname "${BASH_SOURCE[0]}")/.."

pg_dump --no-sync --schema-only --no-owner \
  --exclude-schema=postgraphile_watch \
  --exclude-schema=graphile_migrate \
  --exclude-schema=graphile_worker \
  --file=schema.sql "$1"
python schema/split.py schema.sql
node schema/compactor.ts < schema.sql > schema/compact.sql
