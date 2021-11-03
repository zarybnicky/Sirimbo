{ stdenv
, pkgs
, hasura-graphql-engine
, nodejs-14_x
, python2
}:

let
  graphql-engine-src = hasura-graphql-engine.src;

  console-assets-node-modules = (import ./console-assets-node-composition.nix {
    inherit pkgs;
  }).package.override {
    src = "${graphql-engine-src}/console";
    preRebuild = ''
      substituteInPlace node_modules/cypress/package.json \
        --replace '"postinstall": "node index.js --exec install",' ""
    '';
    postInstall = "mv $out/lib/node_modules/*/*/node_modules /tmp/_; rm -rf $out; mv /tmp/_ $out";
    buildInputs = [python2];
  };
in
stdenv.mkDerivation rec {
  name = "hasura-console-assets-${graphql-engine-src.rev}";
  src = "${graphql-engine-src}/console";
  buildPhase = ''
    source $stdenv/setup;
    cp -a ${console-assets-node-modules} node_modules
    substituteInPlace Makefile --replace \
      'gsutil -m cp -r gs://$(BUCKET_NAME)/console/assets/common "$(DIST_PATH)"' \
     'tar xzvf ${./console-assets-common.tar.gz}'
     HOME=$(pwd) make server-build
  '';
  installPhase = "mkdir -p $out; cp -a static/dist/common $out; cp -a static/dist/versioned $out";
  buildInputs = [ nodejs-14_x ];
}
