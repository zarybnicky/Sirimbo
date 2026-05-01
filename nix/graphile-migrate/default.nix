{
  buildNpmPackage,
}:

buildNpmPackage {
  pname = "graphile-migrate";
  version = "2.0.0-rc3";
  src = ./.;
  npmDepsHash = "sha256-rrzdhNhajuRnd8t+HD/gcaJEu1LbDq9yGNFvprbvQsU=";
  dontNpmBuild = true;
}
