{ nix-gitignore
, stdenv
, yarnPackages
}:

stdenv.mkDerivation {
  name = "sirimbo-frontend-old";
  src = yarnPackages."sirimbo-frontend@workspace:apps/custom-elements";
  buildPhase = ''
    mkdir -p $out/public
    cp -Lr dist/* $out/public/
    cp -Lr static/* $out/public/
  '';
  # extraBuildInputs = [libsass];
  # yarnPreBuild = "export npm_config_nodedir=${nodejs}";
  # pkgConfig = {
  #   node-sass = {
  #     nativeBuildInputs = [];
  #     buildInputs = [ libsass pkg-config python3 ];
  #     postInstall = ''
  #       LIBSASS_EXT=auto yarn --offline run build
  #       rm build/config.gypi
  #     '';
  #   };
  # };
}
