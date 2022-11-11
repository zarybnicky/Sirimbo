{ yarn2nix-moretea
, nix-gitignore
, libsass
, nodejs
, pkg-config
, python3
}:

yarn2nix-moretea.mkYarnPackage {
  src = nix-gitignore.gitignoreSourcePure [../.gitignore] ./.;
  packageJSON = ./package.json;
  yarnLock = ../yarn.lock;
  name = "sirimbo-frontend";
  # doCheck = true;
  # checkPhase = "yarn test --coverage --ci";
  buildPhase = ''
    shopt -s dotglob
    unlink deps/sirimbo-frontend/sirimbo-frontend
    unlink deps/sirimbo-frontend/node_modules
    mv deps/sirimbo-frontend/* .

    find -type f -not -path "./node_modules/*"
    NEXT_PUBLIC_BASE_URL="http://undefined" yarn --offline run build
  '';
  distPhase = "true";
  installPhase = ''
    shopt -s dotglob
    mkdir -p $out
    cp -r public package.json $out/
    cp -r .next/standalone/frontend/* $out/
    cp -r .next/static $out/.next/
  '';
  extraBuildInputs = [libsass];
  yarnPreBuild = "export npm_config_nodedir=${nodejs}";
  pkgConfig = {
    node-sass = {
      nativeBuildInputs = [];
      buildInputs = [ libsass pkg-config python3 ];
      postInstall = ''
        LIBSASS_EXT=auto yarn --offline run build
        rm build/config.gypi
      '';
    };
  };
}
