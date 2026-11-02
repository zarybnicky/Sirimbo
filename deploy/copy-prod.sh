#!/usr/bin/env bash

set -euo pipefail

ssh tango "rm -rf /tmp/olymp.dump.d && \
  pg_dump -Fd -j 4 --compress=zstd:3 --with-statistics \
    --exclude-schema=postgraphile_watch --exclude-schema=graphile_worker \
    -f /tmp/olymp.dump.d olymp"

rsync -a --delete tango:/tmp/olymp.dump.d/ ./olymp.dump.d/

dropdb --if-exists --force olymp_new
createdb olymp_new
pg_restore -j "$(nproc)" -d olymp_new ./olymp.dump.d

dropdb --if-exists --force olymp
psql -d postgres -c "ALTER DATABASE olymp_new RENAME TO olymp"
