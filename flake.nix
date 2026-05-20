{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

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

      ruian-csv-adr-st = final.fetchzip {
        name = "ruian-csv-adr-st-20260430";
        url = "https://vdp.cuzk.gov.cz/vymenny_format/csv/20260430_OB_ADR_csv.zip";
        hash = "sha256-HO+N/A1gxibvS1je0lFu6UniH4Fm/gRUUAY7LRnp/aA=";
        stripRoot = false;
      };

      ruian-address-cache = final.runCommand "ruian-address-cache-20260430.duckdb" {
        nativeBuildInputs = [ final.duckdb final.iconv ];
      } ''
        mkdir csv
        for input in ${final.ruian-csv-adr-st}/CSV/*_ADR.csv; do
          iconv -f CP1250 -t UTF-8 "$input" > "csv/$(basename "$input")"
        done

        export RUIAN_CSV_GLOB="$PWD/csv/*_ADR.csv"
        duckdb -bail "$out" -f ${./nix/ruian-address-cache.sql}
      '';

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
          pnpm_9
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
        ruian-csv-adr-st
        ruian-address-cache;
    });

    checks = forAllSystems (pkgs: {
      rozpisovnik-api = pkgs.rozpisovnik-api;
      rozpisovnik-worker = pkgs.rozpisovnik-worker;
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
