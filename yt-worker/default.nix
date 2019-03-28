{ pkgs ? import <nixpkgs> {} }:
let
  gogol = pkgs.fetchFromGitHub {
    owner = "brendanhay";
    repo = "gogol";
    rev = "dc4b2a58a827dc5680e2570ab2fce6e103711e79";
    sha256 = "1lp5qmpdndmrjdfc912rlhax5blf31g75malcq6zpm6k870hmp9x";
  };
in pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  yt-worker = ./.;
  gogol = "${gogol}/gogol";
  gogol-core = "${gogol}/core";
  gogol-youtube = "${gogol}/gogol-youtube";
})
