{ pkgs ? import <nixpkgs> {} }:
(import ./. {}).shellFor {
  packages = p: [ p.olymp-api ];
  buildInputs = [ pkgs.cabal-install ];
  withHoogle = true;
  exactDeps = true;
}
