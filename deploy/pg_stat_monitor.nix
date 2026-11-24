{
  lib,
  stdenv,
  fetchFromGitHub,
  postgresql,
}:
stdenv.mkDerivation rec {
  pname = "pg_stat_monitor";
  version = "2.4.1";

  src = fetchFromGitHub {
    owner = "percona";
    repo = "pg_stat_monitor";
    rev = "refs/tags/${version}";
    sha256 = "1xr0w5ikjqfdj0ywqljh230x7dbi5c9sh8fid3ip6bwqxjg3jqwq";
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
