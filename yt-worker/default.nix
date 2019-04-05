{ pkgs ? import <nixpkgs> {} }:
let
  gogol = pkgs.fetchFromGitHub {
    owner = "brendanhay";
    repo = "gogol";
    rev = "dc4b2a58a827dc5680e2570ab2fce6e103711e79";
    sha256 = "1lp5qmpdndmrjdfc912rlhax5blf31g75malcq6zpm6k870hmp9x";
  };
in with pkgs.haskell.lib; pkgs.haskellPackages.extend (self: super: {
  yt-worker = overrideCabal (self.callCabal2nix "yt-worker" ./. {}) (drv: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
      "--ghc-option=-optl=-L${pkgs.ncurses.override { enableStatic = true; }}/lib"
      "--ghc-option=-optl=-L${(pkgs.pcre.overrideAttrs (old: { dontDisableStatic = true; })).out}/lib"
      "--ghc-option=-optl=-L${(pkgs.openssl.override { static = true; }).out}/lib"
    ];
  });
  binary-parsers = dontCheck super.binary-parsers;
  gogol = self.callCabal2nix "gogol" "${gogol}/gogol" {};
  gogol-core = self.callCabal2nix "gogol-core" "${gogol}/core" {};
  gogol-youtube = self.callCabal2nix "gogol-youtube" "${gogol}/gogol-youtube" {};
})
