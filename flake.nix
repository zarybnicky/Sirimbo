{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";

  outputs = { self, nixpkgs, ... }: let
    allSystems = [ "x86_64-linux" "aarch64-darwin" ];
    forAllSystems = fn: nixpkgs.lib.genAttrs allSystems (system: fn (import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      config.permittedInsecurePackages = [
        "pnpm-10.34.0"
      ];
      overlays = [ self.overlays.default ];
    }));

  in {
    nixosModules.default = ./deploy/module.nix;

    overlays.default = final: prev: {
      graphile-migrate = final.callPackage ./deploy/graphile-migrate {};
      ruian-address-cache = final.callPackage ./deploy/ruian-address-cache.nix {};
      geonames-city-cache = final.callPackage ./deploy/geonames-city-cache.nix {};
      postgresql_18 = prev.postgresql_18.overrideAttrs (old: {
        passthru = old.passthru // {
          pkgs = old.passthru.pkgs // {
            pg_stat_monitor = final.postgresql_18.pkgs.callPackage ./deploy/pg_stat_monitor.nix {};
          };
        };
      });

      rozpisovnik-migrations = final.runCommandLocal "rozpisovnik-migrations" {} ''
        mkdir -p $out
        cp -r ${./migrations} $out/migrations
        cp -r ${./.gmrc} $out/.gmrc
      '';
    };

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShellNoCC {
        buildInputs = with pkgs; [
          seaweedfs
          mailpit
          prettier
          graphile-migrate
          pnpm_10
          nodejs_24
          postgresql_18
          sqlfluff
          overmind
          playwright-driver
          playwright-driver.browsers
          perlPackages.TAPParserSourceHandlerpgTAP
        ];
        env = {
          NIXPKGS_FLAKE = ".";
          PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
          PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
          CHROME_DEVTOOLS_MCP_BROWSERS = pkgs.playwright-driver.browsers;
        };
        shellHook = ''
          unset PYTHONPATH
          unset PYTHONHOME
        '';
      };
    });

    packages = forAllSystems (pkgs: {
      inherit (pkgs)
        nodejs
        ffmpeg
        playwright-driver
        graphile-migrate
        rozpisovnik-migrations
        ruian-address-cache
        geonames-city-cache;
    });

    checks = forAllSystems (pkgs: {
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
                environment = lib.mkOption {
                  type = lib.types.attrsOf lib.types.str;
                  default = {};
                };
              };
            }));
          };
        })
        self.nixosModules.default
        { nixpkgs.overlays = builtins.attrValues self.overlays; }
        { nixpkgs.config.permittedInsecurePackages = [ "pnpm-10.34.0" ]; }
        ./deploy/container.nix
      ];
    };
  };
}
