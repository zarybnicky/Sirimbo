{ stdenv, lib
, fetchurl
, openssl
, zlib
, autoPatchelfHook
}:

stdenv.mkDerivation rec {
  pname = "squawk";
  version = "0.8.1";
  src = fetchurl {
    url = "https://github.com/sbdchd/squawk/releases/download/v${version}/squawk-linux-x86_64";
    sha256 = "sha256-LGvJ7mFMDwPolwJOYbRdIgN5rRCeX1HeANGmceX8kZg=";
  };
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ openssl zlib ];
  unpackPhase = "true";
  installPhase = "install -m755 -D $src $out/bin/squawk";
}
