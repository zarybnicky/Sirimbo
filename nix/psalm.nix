{lib, stdenv, makeWrapper, php74}:

stdenv.mkDerivation {
  pname = "psalm";
  version = "4.29.0";
  src = builtins.fetchurl {
    url = "https://github.com/vimeo/psalm/releases/download/4.29.0/psalm.phar";
    sha256 = "1s6h1q9y1fy3idf4vbqk69cbg73cvclhgpvnnsih8gs0yc8a7qxb";
  };
  phases = [ "installPhase" ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    install -D $src $out/libexec/psalm.phar
    makeWrapper ${php74}/bin/php $out/bin/psalm --add-flags "$out/libexec/psalm.phar"
  '';
}
