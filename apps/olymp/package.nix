{ yarn2nix-moretea
, nix-gitignore
, libsass
, nodejs
, pkg-config
, python3
, vips
}:

yarn2nix-moretea.mkYarnPackage {
  src = nix-gitignore.gitignoreSourcePure [../../.gitignore ./.gitignore] ./.;
  packageJSON = ./package.json;
  yarnLock = ../../yarn.lock;
  name = "olymp";
  distPhase = "true";
  buildPhase = ''
    shopt -s dotglob
    unlink deps/olymp/olymp
    unlink deps/olymp/node_modules
    mv deps/olymp/* .
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