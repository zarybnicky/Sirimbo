{
  inputs.co-log-src = { flake = false; url = github:kowainik/co-log/main; };
  inputs.typerep-map = { flake = false; url = github:kowainik/typerep-map/main; };
  inputs.servant-websockets = { flake = false; url = github:moesenle/servant-websockets/master; };

  outputs = { self, nixpkgs, co-log-src, typerep-map, servant-websockets }: let
    lib = nixpkgs.lib;
    pkgs = import nixpkgs { system = "x86_64-linux"; };

    cleanSource = name: type: let
      baseName = baseNameOf (toString name);
    in lib.cleanSourceFilter name type && !(
      (type == "directory" && (lib.elem baseName [ ".stack-work" "dist" ".git"])) ||
      lib.any (lib.flip lib.hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ]
    );

    co-log = pkgs.runCommand "co-log-source" {} ''
      mkdir -p $out
      cd $out
      cp -rL ${co-log-src}/{co-log,co-log-core} $out
    '';

    hsOverrides = self: with pkgs.haskell.lib; {
      typerep-map = doJailbreak (dontCheck (self.callCabal2nix "typerep-map" typerep-map {}));
      co-log = doJailbreak (dontCheck (self.callCabal2nix "co-log" "${co-log}/co-log" {}));
      co-log-core = self.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
      servant-websockets = self.callCabal2nix "servant-websockets" servant-websockets {};
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

  in {
    packages.x86_64-linux = pkgs.lib.attrsets.genAttrs hsPackagesSrc (x: builtins.getAttr x hsPackages);
    devShell.x86_64-linux = hsPackages.shellFor {
      packages = p: map (x: builtins.getAttr x p) hsPackagesSrc;
      buildInputs = [
        pkgs.yarn
        pkgs.php73Packages.phpstan
      ];
    };
  };
}
