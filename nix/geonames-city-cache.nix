{ fetchurl, runCommand, duckdb, unzip }:

let
  version = "20261015";
  cities = fetchurl {
    name = "geonames-cities500-${version}.zip";
    url = "https://download.geonames.org/export/dump/cities500.zip";
    hash = "sha256-Rv0ESn4NLQl9qpbZB1yMgY6LDjN79YckpPlGN901GtI=";
  };
  countries = fetchurl {
    name = "geonames-countryInfo-${version}.txt";
    url = "https://download.geonames.org/export/dump/countryInfo.txt";
    hash = "sha256-xV9jjPqvtP5FqaoVEgoPH3joo0qOGEfXuTIM90D7yas=";
  };
  regions = fetchurl {
    name = "geonames-admin1-${version}.txt";
    url = "https://download.geonames.org/export/dump/admin1CodesASCII.txt";
    hash = "sha256-tXe6o0drvpTwRUVF5Rfhg1QB15xbEQXI4Czx1KUF6Ow=";
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
