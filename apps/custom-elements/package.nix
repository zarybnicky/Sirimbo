{ nix-gitignore
, stdenv
, yarnPackages
}:

stdenv.mkDerivation {
  name = "sirimbo-frontend-old";
  src = yarnPackages."sirimbo-frontend@workspace:apps/custom-elements".package;
  buildPhase = ''
    cd node_modules/sirimbo-frontend
    mkdir -p $out/public
    cp -Lr dist/* $out/public/
    cp -Lr static/* $out/public/
  '';
}
