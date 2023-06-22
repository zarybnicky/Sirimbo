{ yarn2nix-moretea
, nix-gitignore
, libsass
, nodejs
, pkg-config
, python3
, vips
}:

yarn2nix-moretea.mkYarnPackage {
  src = nix-gitignore.gitignoreSourcePure [../.gitignore] ./.;
  packageJSON = ./package.json;
  yarnLock = ../yarn.lock;
  name = "sirimbo-frontend";
  # doCheck = true;
  # checkPhase = "yarn test --coverage --ci";
  distPhase = "true";
  buildPhase = ''
    shopt -s dotglob
    unlink deps/sirimbo-frontend/sirimbo-frontend
    unlink deps/sirimbo-frontend/node_modules
    mv deps/sirimbo-frontend/* .
    NEXT_PUBLIC_BASE_URL="http://undefined" yarn --offline run build
    shopt -s dotglob
    mkdir -p $out
    cp -r public package.json $out/
    cp -r .next/standalone/frontend/* $out/
    cp -r .next/static $out/.next/
    PATTERN=/nix/store/
    ln -s /tmp/''${out//$PATTERN/} $out/.next/cache
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
