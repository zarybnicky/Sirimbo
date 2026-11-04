{
  fetchurl,
  runCommand,
  duckdb,
  iconv,
  unzip,
}:

let
  version = "20260430";
  sql = ./ruian-address-cache.sql;
  src = fetchurl {
    name = "ruian-address-csv-${version}.zip";
    url = "https://vdp.cuzk.gov.cz/vymenny_format/csv/${version}_OB_ADR_csv.zip";
    hash = "sha256-aRG51nWvtKz58bONI/TwaJgzIEDTDDUiELplOsHOJMc=";
  };
in
runCommand "ruian-address-cache-${version}.duckdb"
  {
    inherit src sql;

    nativeBuildInputs = [
      duckdb
      iconv
      unzip
    ];

    passthru = {
      inherit version src;
    };
  }
  ''
    set -euo pipefail
    mkdir -p csv
    members="$TMPDIR/adr-members.txt"
    unzip -Z -1 "$src" | grep -E '(^|/)CSV/.*_ADR[.]csv$' | LC_ALL=C sort > "$members"
    test -s "$members"
    {
      first=1

      while IFS= read -r member; do
        if [ "$first" = 1 ]; then
          unzip -p "$src" "$member"
          first=0
        else
          # Strip duplicate CSV header from every file after the first.
          unzip -p "$src" "$member" | tail -n +2
        fi
      done < "$members"
    } | iconv -f CP1250 -t UTF-8 > csv/all_ADR.csv

    export RUIAN_CSV_GLOB="$PWD/csv/all_ADR.csv"
    duckdb -bail "$out" -f "$sql"
  ''
