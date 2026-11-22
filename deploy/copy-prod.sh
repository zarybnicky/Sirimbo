#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 --fetch|--apply

  --fetch  Dump production on tango and download it to ./olymp.dump.d/.
  --apply  Restore ./olymp.dump.d/ and replace the local olymp database.
EOF
}

if [[ $# -ne 1 ]]; then
  usage >&2
  exit 1
fi

step=$1
case "$step" in
  --fetch|--apply) ;;
  -h|--help) usage; exit 0 ;;
  *) usage >&2; exit 1 ;;
esac

if [[ "$step" == --fetch ]]; then
  ssh tango "rm -rf /tmp/olymp.dump.d && pg_dump -Fd -j 4 --compress=zstd:3 --statistics -f /tmp/olymp.dump.d olymp"
  rsync -a --delete tango:/tmp/olymp.dump.d/ ./olymp.dump.d/
  exit 0
fi

restore_list=$(mktemp)
trap 'rm -f "$restore_list"' EXIT
pg_restore --list ./olymp.dump.d |
  sed -E '/^[0-9]+; [0-9]+ [0-9]+ ACL public FUNCTION (plpgsql_check_function(_tb)?|plpgsql_make_pragma|postgres_fdw_connection)\(/d' > "$restore_list"

dropdb --if-exists --force olymp_new
createdb olymp_new
pg_restore -j "$(nproc)" --use-list="$restore_list" -d olymp_new ./olymp.dump.d

# pg_dump omits our addition to the built-in GiST operator family.
psql -X -v ON_ERROR_STOP=1 -d olymp_new -c \
  'ALTER OPERATOR FAMILY pg_catalog.range_ops USING gist ADD OPERATOR 3 app_private.&& (tstzrange, anyrange)'

dropdb --if-exists --force olymp
psql -d postgres -c "ALTER DATABASE olymp_new RENAME TO olymp"
