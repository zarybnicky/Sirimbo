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
  yarnLock = ./yarn.lock;
  name = "sirimbo-frontend";
  # doCheck = true;
  # checkPhase = "yarn test --coverage --ci";
  buildPhase = ''
    yarn --offline run build
    sed -i '1,3d' deps/Sirimbo/dist/main.css
  '';
  distPhase = "true";
  installPhase = ''
    mkdir -p $out/public
    cp -Lr deps/Sirimbo/dist/* $out/public/
    cp -Lr deps/Sirimbo/static/* $out/public/
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
