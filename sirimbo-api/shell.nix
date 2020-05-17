{ pkgs ? import <nixpkgs> {} }:
(import ./. {}).shellFor {
  packages = p: [ p.olymp-api p.olymp-schema p.olymp-tournament ];
  buildInputs = [ pkgs.cabal-install ];
  withHoogle = true;
  exactDeps = true;
}
