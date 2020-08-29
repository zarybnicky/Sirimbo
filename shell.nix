let
  sources = import nix/sources.nix;
  cleanSource = with pkgs.lib; name: type: let
    baseName = baseNameOf (toString name);
  in cleanSourceFilter name type && !(
    (type == "directory" && (elem baseName [ ".stack-work" "dist" ".git"])) ||
    any (flip hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ]
  );
  overlay = self: super: {
    niv = import sources.niv {};
    yarn2nix = import sources.yarn2nix {};
  };
  pkgs = import sources.nixpkgs {
    overlays = [ overlay ];
    config = {};
  };

  co-log = pkgs.runCommand "co-log-source" {} ''
    mkdir -p $out
    cd $out
    cp -rL ${sources.co-log}/{co-log,co-log-core} $out
  '';
  hsOverrides = self: with pkgs.haskell.lib; {
    typerep-map = doJailbreak (dontCheck (self.callCabal2nix "typerep-map" sources.typerep-map {}));
    co-log = doJailbreak (dontCheck (self.callCabal2nix "co-log" "${co-log}/co-log" {}));
    co-log-core = self.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
    servant-websockets = self.callCabal2nix "servant-websockets" sources.servant-websockets {};
  };
  hsPackagesSrc = [
    "sirimbo-api"
    "sirimbo-schema"
    "sirimbo-tournament"
    "sirimbo-yt-worker"
  ];
  hsPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (
      hself: hsuper: builtins.listToAttrs (map (x: {
        name = x;
        value = hself.callCabal2nix x (builtins.path {
          filter = cleanSource;
          path = ./. + "/${x}";
        }) {};
      }) hsPackagesSrc) // hsOverrides hself
    );
  });
  buildTools = with pkgs; [
    yarn2nix.yarn
    niv.niv
    php73Packages.phpstan
  ];
in
hsPackages.shellFor {
  packages = p: map (x: builtins.getAttr x p) hsPackagesSrc;
  buildInputs = buildTools;
}
