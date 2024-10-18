{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-24.05;
    devenv.url = github:cachix/devenv;
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    graphile-migrate-flake.url = github:zarybnicky/graphile-migrate-flake;
    graphile-migrate-flake.inputs.nixpkgs.follows = "nixpkgs";
    yarnpnp2nix.url = "github:madjam002/yarnpnp2nix";
    yarnpnp2nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, devenv, yarnpnp2nix, graphile-migrate-flake, ... } @ inputs: let
    allSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = fn: nixpkgs.lib.genAttrs allSystems (system: fn (import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        graphile-migrate-flake.overlays.default
        self.overlays.default
        (final: prev: {
          nix = final.nixVersions.git;
        })
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
          "node-gyp@npm:9.4.0" = {
            outputHashByPlatform = {
              "x86_64-linux" = "sha512-3O3TyO9LdL7rDJHYwE5RCGJi2KuJSgniWztbX2qaO6dDmBXdmzPeUWzYirsuH7Rtakl9vlvkyU3KN9Eit2dtBA==";
            };
          };
          "prettier@npm:3.3.3" = {
            outputHashByPlatform = {
              "x86_64-linux" = "sha512-29qUGIt4gye0OIrbU4JruJxrUUD4HEdiuLs7aWC6KPAOcccPB3/iFYy+BHo7SpT7MLah/Kn2TEg69IX/bdz7lw==";
            };
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
      prettier = yarnPackages."prettier@npm:3.3.3";
      nodemon = yarnPackages."nodemon@npm:3.1.7";
      squawk = yarnPackages."squawk-cli@npm:1.4.0";

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
              pkgs.nodemon
              pkgs.graphile-migrate
              pkgs.yarn
              pkgs.nodejs
              pkgs.postgresql_15
              pkgs.sqlint
              pkgs.pgformatter
              pkgs.squawk
              pkgs.pgsync
            ];

            devenv.tmpdir = ".devenv";
            pre-commit.hooks.commitizen.enable = true;
            process.implementation = "overmind";

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
