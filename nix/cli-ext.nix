{ stdenv
, pkgs
, hasura-graphql-engine
, nodejs-14_x
, python2
}:

let
  graphql-engine-src = hasura-graphql-engine.src;

  pkgfetch = builtins.fetchurl {
    url = "https://github.com/vercel/pkg-fetch/releases/download/v3.1/node-v12.22.1-linuxstatic-x64";
    sha256 = "04dvvj8k2n254f3jfxnhzrbvr354vinfrbmagd3c5czkfd1c1gjg";
  };

  cli-ext-node-modules = (import ./cli-ext-node-composition.nix {
    inherit pkgs;
    nodejs = nodejs-14_x;
  }).package.override {
    src = "${graphql-engine-src}/cli-ext";
    postInstall = "mv $out/lib/node_modules/*/node_modules /tmp/_; rm -rf $out; mv /tmp/_ $out";
  };
in
stdenv.mkDerivation rec {
  name = "hasura-cli-ext-${graphql-engine-src.rev}";
  src = "${graphql-engine-src}/cli-ext";
  buildInputs = [ nodejs-14_x ];
  phases = ["unpackPhase" "buildPhase" "installPhase"];
  buildPhase = ''
    source $stdenv/setup;
    mkdir -p .pkg-cache/v3.1
    cp -a ${pkgfetch} .pkg-cache/v3.1/built-v12.22.1-linux-x64
    chmod u+x .pkg-cache/v3.1/built-v12.22.1-linux-x64
    cp -a ${cli-ext-node-modules} node_modules
    substituteInPlace package.json \
      --replace 'rm -rf src/shared && cp -r ../console/src/shared ./src/shared' \
                'rm -rf src/shared && cp -r ${graphql-engine-src}/console/src/shared ./src/shared' \
      --replace 'node12-linux-x64,node12-macos-x64,node12-win-x64,node12-linux-arm64,node16-macos-arm64' \
                'node12-linux-x64'
    HOME=$(pwd) npm run build
  '';
  installPhase = "mkdir $out; cp -a bin $out/bin";
}
