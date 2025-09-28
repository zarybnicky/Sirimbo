{
  buildNpmPackage,
}:

buildNpmPackage {
  pname = "graphile-migrate";
  version = "2.0.0-rc2";
  src = ./.;
  npmDepsHash = "sha256-qZRG2iHrPrAE8tgKnys6MDr1I6rkA4yg3b7f5Gcc84I=";
  dontNpmBuild = true;
}
