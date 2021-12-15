{ stdenv
, fetchurl
, nodejs
}:

stdenv.mkDerivation {
  pname = "ncc";
  version = "0.33.0";
  src = fetchurl {
    url = "https://registry.npmjs.org/@vercel/ncc/-/ncc-0.33.0.tgz";
    sha1 = "U41zrUvhslmDDSSdg/hXQy7diWc=";
  };
  buildPhase = ''
    mkdir -p $out/{bin,libexec}
    cp -r ./dist/* $out/libexec
    cat > $out/bin/ncc <<EOS
    #!/usr/bin/env bash
    exec ${nodejs}/bin/node $out/libexec/ncc/cli.js "\$@"
    EOS
    chmod +x $out/bin/ncc
  '';
  installPhase = "true";
}
