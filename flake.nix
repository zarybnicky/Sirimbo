{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

  outputs = { self, nixpkgs, ... }: let
    allSystems = [ "x86_64-linux" "aarch64-darwin" ];
    forAllSystems = fn: nixpkgs.lib.genAttrs allSystems (system: fn (import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [ self.overlays.default ];
    }));

  in {
    nixosModules.default = ./nix/module.nix;

    overlays.default = final: prev: {
      graphile-migrate = final.callPackage ./nix/graphile-migrate {};
      ruian-address-cache = final.callPackage ./nix/ruian-address-cache.nix {};

      rozpisovnik-worker = final.callPackage ./nix/build-pnpm-package.nix {
        packageJSON = final.lib.importJSON ./worker/package.json;
        workspaceFolders = [ "worker" ];
        pnpmDepsHash = builtins.readFile ./worker/pnpm-deps-hash.txt;
        postInstall = "cp -s $out/share/worker/rozpisovnik-worker $out/bin/";
      };

      rozpisovnik-api = final.callPackage ./nix/build-pnpm-package.nix {
        packageJSON = final.lib.importJSON ./backend/package.json;
        workspaceFolders = [ "backend" ];
        pnpmDepsHash = builtins.readFile ./backend/pnpm-deps-hash.txt;
        postInstall = "cp -s $out/share/backend/src/index.ts $out/bin/rozpisovnik-api";
      };

      rozpisovnik-migrations = final.runCommand "rozpisovnik-migrations" {} ''
        mkdir -p $out
        cp -r ${./migrations} $out/migrations
        cp -r ${./.gmrc} $out/.gmrc
      '';
    };

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          nodePackages.prettier
          nodemon
          graphile-migrate
          pnpm_10
          nodejs_24
          postgresql_18
          sqlfluff
          pgformatter
          overmind
          postgrest
          prefetch-npm-deps
          playwright-driver
          playwright-driver.browsers
          perlPackages.TAPParserSourceHandlerpgTAP
        ];
        env = {
          PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
          PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
          RUIAN_ADDRESS_SOURCE = pkgs.ruian-address-cache;
        };
        shellHook = ''
          unset PYTHONPATH
          unset PYTHONHOME
        '';
      };
    });

    packages = forAllSystems (pkgs: {
      inherit (pkgs)
        graphile-migrate
        rozpisovnik-api
        rozpisovnik-worker
        rozpisovnik-migrations
        ruian-address-cache;
    });

    checks = forAllSystems (pkgs: {
      rozpisovnik-api = pkgs.rozpisovnik-api;
      rozpisovnik-worker = pkgs.rozpisovnik-worker;
      module-eval = pkgs.runCommand "my-module-eval-check" { }
        "echo ${builtins.deepSeq self.nixosConfigurations.container.config.system.build.toplevel.outPath "ok"} > $out";
    });

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ lib, ... }: {
          options.my.argocdApps = lib.mkOption {
            type = lib.types.listOf lib.types.attrs;
            default = [];
          };
          options.my.seaweedfs.buckets = lib.mkOption {
            default = {};
            type = lib.types.attrsOf (lib.types.submodule ({ ... }: {
              options = {
                ensure = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                };
                helmValues = lib.mkOption {
                  type = lib.types.listOf lib.types.anything;
                  default = [];
                };
              };
            }));
          };
        })
        self.nixosModules.default
        { nixpkgs.overlays = builtins.attrValues self.overlays; }
        ./nix/container.nix
      ];
    };
  };
}
