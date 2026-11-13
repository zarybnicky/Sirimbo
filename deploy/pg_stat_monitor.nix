{
  lib,
  stdenv,
  fetchFromGitHub,
  postgresql,
}:
let
  release =
    if lib.versionAtLeast postgresql.version "18" then
      {
        version = "2.4.0";
        sha256 = "1jy1jsya35fchxm2di8wwaff9lr5nnw7s3fankwis3bhxzwv22n3";
      }
    else
      {
        version = "2.1.0";
        sha256 = "sha256-STJVvvrLVLe1JevNu6u6EftzAWv+X+J8lu66su7Or2s=";
      };
in
stdenv.mkDerivation {
  pname = "pg_stat_monitor";
  inherit (release) version;

  src = fetchFromGitHub {
    owner = "percona";
    repo = "pg_stat_monitor";
    rev = "refs/tags/${release.version}";
    inherit (release) sha256;
  };

  buildInputs = [ postgresql ] ++ postgresql.buildInputs;
  nativeBuildInputs = [ postgresql.pg_config ];
  makeFlags = [ "USE_PGXS=1" ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/{lib,share/postgresql/extension}
    cp pg_stat_monitor${postgresql.dlSuffix} $out/lib/
    cp pg_stat_monitor.control *.sql $out/share/postgresql/extension/
    runHook postInstall
  '';

  meta = {
    description = "Query Performance Monitoring Tool for PostgreSQL";
    homepage = "https://github.com/percona/pg_stat_monitor";
    license = lib.licenses.postgresql;
    inherit (postgresql.meta) platforms;
  };
}
