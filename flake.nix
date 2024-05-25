{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.11;
    devenv.url = github:cachix/devenv;
    utils.url = "github:numtide/flake-utils";
    graphile-migrate-flake.url = github:zarybnicky/graphile-migrate-flake;
    yarnpnp2nix = {
      url = "github:madjam002/yarnpnp2nix";
    };
  };

  outputs = { self, nixpkgs, devenv, yarnpnp2nix, graphile-migrate-flake, ... } @ inputs: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;

    forAllSystems = fn: nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system: fn (import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        graphile-migrate-flake.overlays.default
        self.overlays.default
      ];
    }));

  in {
    nixosModules.default = ./module.nix;

    overlays.graphile-migrate = graphile-migrate-flake.overlays.default;
    overlays.default = final: prev: let
      yarnPackages = yarnpnp2nix.lib.${final.system}.mkYarnPackagesFromManifest {
        pkgs = final;
        yarnManifest = import ./yarn-manifest.nix;
        packageOverrides = {
          "prettier@npm:3.2.5" = {
            outputHash = "sha512-oj5JphV7F6yZ+y1/6YJFSJ9qHeqUcGr3hMvxC3ayS3DaaL/N/TJ4Ojxb7va+vHbl9spJaY6kMdjEVc5hpjFMYQ==";
          };
          "mjml-core@patch:mjml-core@npm%3A4.14.1#./.yarn/patches/mjml-core-npm-4.14.1-e6ad05b5d7.patch::version=4.14.1&hash=89aa1f&locator=rozpisovnik%40workspace%3A." = {
            outputHash = "sha512-0Ovf7e1Ksrlwig48a0mmiv3XGkxGDrtYYX9I3bxiH6rW0fNKSj8dr4jf+p+D7OD/QQNCfz4jnJ6UUKjMhUjqCA==";
          };
          "typescript@patch:typescript@npm%3A5.1.6#optional!builtin<compat/typescript>::version=5.1.6&hash=5da071" = {
            outputHash = "sha512-Pu+UjhDHG5YXLrzkccAx07evsSpI/urLrawYdsC06bDA/BZ3vvU9QWZJcKF0qe4C+dCb4mAoFXHJDo704iV0zw==";
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
      prettier = yarnPackages."prettier@npm:3.2.5";
      squawk = yarnPackages."squawk-cli@npm:0.28.0";
      commitlint = yarnPackages."@commitlint/cli@npm:17.7.1";
      typescript = yarnPackages."typescript@patch:typescript@npm%3A5.1.6#optional!builtin<compat/typescript>::version=5.1.6&hash=5da071";

      graphile-worker = yarnPackages."graphile-worker@npm:0.16.4";
      rozpisovnik-api = yarnPackages."rozpisovnik-api@workspace:backend";
      rozpisovnik-worker = yarnPackages."rozpisovnik-worker@workspace:worker";
      rozpisovnik-migrations = final.runCommand "rozpisovnik-migrations" {} ''
        mkdir -p $out
        cp -r ${./migrations} $out/migrations
        cp -r ${./.gmrc} $out/.gmrc
      '';

    };

    devShells = forAllSystems (pkgs: {
      default = devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [
          ({ pkgs, ... }: {
            packages = [
              pkgs.commitizen
              pkgs.prettier
              pkgs.graphile-migrate
              pkgs.graphile-worker
              pkgs.typescript
              pkgs.yarn
              pkgs.nodejs
              pkgs.postgresql_15
              pkgs.sqlint
              pkgs.pgformatter
              pkgs.squawk
              pkgs.pgsync
            ];

            pre-commit.hooks.commitizen.enable = true;

            processes = {
              backend.exec = "yarn workspace rozpisovnik-api start";
              worker.exec = "yarn workspace rozpisovnik-worker start";
              frontend.exec = "yarn workspace rozpisovnik-web dev";
              migrate.exec = "graphile-migrate watch";
              schema.exec = "yarn schema";
            };
          })
        ];
      };
    });

    packages = forAllSystems (pkgs: {
      inherit (pkgs)
        graphile-migrate
        graphile-worker
        rozpisovnik-api
        rozpisovnik-worker
        rozpisovnik-migrations;
      devenv-up = self.devShells.${pkgs.system}.default.config.procfileScript;
    });

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.default
        { nixpkgs.overlays = builtins.attrValues self.overlays; }
        ./container.nix
      ];
    };
  };
}
