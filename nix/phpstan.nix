{lib, stdenv, makeWrapper, php82}:

stdenv.mkDerivation {
  pname = "phpstan";
  version = "0.12.67";
  src = builtins.fetchurl {
    url = "https://github.com/phpstan/phpstan/releases/download/0.12.67/phpstan.phar";
    sha256 = "/c+ci/Ok08Qr4bFRiE8e0wSPNo3/k7LbW/KJOkcUTDw=";
  };
  phases = [ "installPhase" ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    install -D $src $out/libexec/phpstan/phpstan.phar
    makeWrapper ${php82}/bin/php $out/bin/phpstan \
      --add-flags "$out/libexec/phpstan/phpstan.phar"
  '';
}
