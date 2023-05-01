{ stdenv
, yarn2nix-moretea
, fetchurl
, nodejs
, ncc
, src
}:

yarn2nix-moretea.mkYarnPackage {
  inherit src;
  name = "graphile-migrate";
  packageJSON = "${src}/package.json";
  yarnLock = "${src}/yarn.lock";
  installPhase = ''
    ${ncc}/bin/ncc build ./deps/graphile-migrate/src/cli.ts
    mkdir -p $out/{bin,libexec}
    cp -r dist/* $out/libexec
    cat > $out/bin/graphile-migrate <<EOS
    #!/usr/bin/env bash
    exec ${nodejs}/bin/node $out/libexec/index.js "\$@"
    EOS
    chmod +x $out/bin/graphile-migrate
  '';
  distPhase = "true";
}
