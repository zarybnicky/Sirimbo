{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";

  outputs = { self, nixpkgs, ... }: let
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

    overlays.default = final: prev: {
      graphile-migrate = final.callPackage ./nix/graphile-migrate {};
      tbls = final.callPackage ./nix/tbls {};

      rozpisovnik-worker = final.callPackage ./nix/build-pnpm-package.nix {
        packageJSON = final.lib.importJSON ./worker/package.json;
        workspaceFolder = "worker";
        pnpmDepsHash = "sha256-gLmrs/2AWgGrTP8ycq1ZOnxZJ83DbKYWCLoh4kOSMpc=";
        postInstall = "cp -s $out/share/worker/rozpisovnik-worker $out/bin/";
      };

      rozpisovnik-api = final.callPackage ./nix/build-pnpm-package.nix {
        packageJSON = final.lib.importJSON ./backend/package.json;
        workspaceFolder = "backend";
        pnpmDepsHash = "sha256-yw3akRsPe4JUKKAJJNZVZ1iAP9ncFFtNxYWseaUrPMs=";
        postInstall = "cp -s $out/share/backend/dist/index.cjs $out/bin/rozpisovnik-api";
      };

      rozpisovnik-migrations = final.runCommand "rozpisovnik-migrations" {} ''
        mkdir -p $out
        cp -r ${./migrations} $out/migrations
        cp -r ${./.gmrc} $out/.gmrc
      '';
    };

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        buildInputs = [
          pkgs.nodePackages.prettier
          pkgs.nodemon
          pkgs.graphile-migrate
          pkgs.pnpm_9
          pkgs.nodejs
          pkgs.postgresql_17
          pkgs.sqlint
          pkgs.pgformatter
          pkgs.tbls
          pkgs.overmind
          pkgs.prefetch-npm-deps
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
