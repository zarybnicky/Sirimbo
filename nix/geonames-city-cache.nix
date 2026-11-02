{ fetchurl, runCommand, duckdb, unzip }:

let
  version = "20261015";
  cities = fetchurl {
    name = "geonames-cities500-${version}.zip";
    url = "https://download.geonames.org/export/dump/cities500.zip";
    hash = "sha256-dBV+ArGkXTvcn/dhSbzvHhUh219ZBUrVtt+F4/l9S2k=";
  };
  countries = fetchurl {
    name = "geonames-countryInfo-${version}.txt";
    url = "https://download.geonames.org/export/dump/countryInfo.txt";
    hash = "sha256-efj1nqAcu0BDi/GFbdbC/yAfA/j8ctBTCyZGqvYD7wQ=";
  };
  regions = fetchurl {
    name = "geonames-admin1-${version}.txt";
    url = "https://download.geonames.org/export/dump/admin1CodesASCII.txt";
    hash = "sha256-Lkhn9Tl6ywVuomN3YBtXrzfLTIE5l3AiUBjp5DnpGtM=";
  };
in
runCommand "geonames-city-cache-${version}.duckdb" {
  nativeBuildInputs = [ duckdb unzip ];
  passthru = { inherit version cities countries regions; };
} ''
  unzip ${cities} cities500.txt
  export GEONAMES_CITIES="$PWD/cities500.txt"
  export GEONAMES_COUNTRIES=${countries}
  export GEONAMES_REGIONS=${regions}
  duckdb -bail "$out" -f ${./geonames-city-cache.sql}
''
