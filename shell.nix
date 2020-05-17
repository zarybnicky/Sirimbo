let
  sources = import nix/sources.nix;
  overlay = self: super: {
    niv = import sources.niv {};
    yarn2nix = import sources.yarn2nix {};
  };
  pkgs = import sources.nixpkgs {
    overlays = [ overlay ];
    config = {};
  };
  buildTools = with pkgs; [
    yarn2nix.yarn
    niv.niv
    php73Packages.phpstan
  ];
in
pkgs.mkShell {
  buildInputs = buildTools ++ [];
}
