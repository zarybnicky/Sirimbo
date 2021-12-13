{ yarn2nix-moretea
, nodejs
, pkg-config
, src
, packageJSON
, yarnLock
}:

yarn2nix-moretea.mkYarnPackage {
  inherit src packageJSON yarnLock;
  name = "sirimbo-backend";
  # doCheck = true;
  # checkPhase = "yarn test --coverage --ci";
}
