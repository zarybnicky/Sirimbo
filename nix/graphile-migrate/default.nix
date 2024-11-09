{ lib
, buildNpmPackage
}:

buildNpmPackage {
  pname = "graphile-migrate";
  version = "1.4.1";
  src = ./.;
  npmDepsHash = "sha256-Zl/07LbhRESr6myorFXjasamgoEMazyjEM5TANbbEO8=";
  dontNpmBuild = true;
}
