{
  buildNpmPackage,
}:

buildNpmPackage {
  pname = "graphile-migrate";
  version = "2.0.0-rc2";
  src = ./.;
  npmDepsHash = "sha256-pzzLB0KC1ouq2aRWMSocCLS34DjgWoxmZ/LtoeZ+duQ=";
  dontNpmBuild = true;
}
