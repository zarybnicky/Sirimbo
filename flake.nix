{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
    yarnpnp2nix.url = "github:madjam002/yarnpnp2nix";
    yarnpnp2nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, yarnpnp2nix, ... } @ inputs: let
    allSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = fn: nixpkgs.lib.genAttrs allSystems (system: fn (import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        self.overlays.default
        (final: prev: {
          nix = final.nixVersions.git;
        })
      ];
    }));

  in {
    nixosModules.default = ./nix/module.nix;

    overlays.default = final: prev: let
      yarnPackages = yarnpnp2nix.lib.${final.system}.mkYarnPackagesFromManifest {
        pkgs = final;
        yarnManifest = import ./yarn-manifest.nix;
        packageOverrides = {
          "node-gyp@npm:9.4.0" = {
            outputHashByPlatform = {
              "x86_64-linux" = "sha512-3O3TyO9LdL7rDJHYwE5RCGJi2KuJSgniWztbX2qaO6dDmBXdmzPeUWzYirsuH7Rtakl9vlvkyU3KN9Eit2dtBA==";
              "aarch64-darwin" = "sha512-3O3TyO9LdL7rDJHYwE5RCGJi2KuJSgniWztbX2qaO6dDmBXdmzPeUWzYirsuH7Rtakl9vlvkyU3KN9Eit2dtBA==";
            };
          };
          "prettier@npm:3.3.3" = {
            outputHashByPlatform = {
              "x86_64-linux" = "sha512-29qUGIt4gye0OIrbU4JruJxrUUD4HEdiuLs7aWC6KPAOcccPB3/iFYy+BHo7SpT7MLah/Kn2TEg69IX/bdz7lw==";
              "aarch64-darwin" = "sha512-29qUGIt4gye0OIrbU4JruJxrUUD4HEdiuLs7aWC6KPAOcccPB3/iFYy+BHo7SpT7MLah/Kn2TEg69IX/bdz7lw==";
            };
          };
          "typescript@patch:typescript@npm%3A5.6.3#optional!builtin<compat/typescript>::version=5.6.3&hash=8c6c40" = {
            outputHash = "sha512-AFBMAe5C1HDCNJVCavB1EuJeZUa85+JFcucqnKLmsum+pj3kKGw8/qZEh02hRn3PyiP0+Y98ryD4sDwCE7toNw==";
          };
          "rozpisovnik-worker@workspace:worker" = {
            shouldBeUnplugged = true;
            build = "node build.cjs && rm .gitignore";
          };
          "rozpisovnik-api@workspace:backend" = {
            shouldBeUnplugged = true;
            build = "node build.cjs";
          };
        };
      };

    in {
      prettier = yarnPackages."prettier@npm:3.3.3";
      nodemon = yarnPackages."nodemon@npm:3.1.9";
      squawk = yarnPackages."squawk-cli@npm:1.4.0";

      graphile-migrate = final.callPackage ./nix/graphile-migrate {};
      tbls = final.callPackage ./nix/tbls {};

      rozpisovnik-api = yarnPackages."rozpisovnik-api@workspace:backend";
      rozpisovnik-worker = yarnPackages."rozpisovnik-worker@workspace:worker";
      rozpisovnik-migrations = final.runCommand "rozpisovnik-migrations" {} ''
        mkdir -p $out
        cp -r ${./migrations} $out/migrations
        cp -r ${./.gmrc} $out/.gmrc
      '';
    };

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        buildInputs = [
          pkgs.prettier
          pkgs.nodemon
          pkgs.graphile-migrate
          pkgs.yarn
          pkgs.nodejs
          pkgs.postgresql_17
          pkgs.sqlint
          pkgs.pgformatter
          pkgs.squawk
          pkgs.tbls
          pkgs.overmind
        ];
      };
    });

    packages = forAllSystems (pkgs: {
      inherit (pkgs)
        graphile-migrate
        tbls
        rozpisovnik-api
        rozpisovnik-worker
        rozpisovnik-migrations;
    });

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.default
        { nixpkgs.overlays = builtins.attrValues self.overlays; }
        ./nix/container.nix
      ];
    };
  };
}
