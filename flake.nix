{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/release-22.05;
  inputs.migrate = { flake = false; url = github:graphile/migrate/main; };

  outputs = { self, nixpkgs, migrate }: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
  in {
    nixosModule = ./nix/module.nix;

    overlay = final: prev: {
      ncc = final.callPackage ./nix/ncc.nix {};
      squawk = final.callPackage ./nix/squawk.nix {};
      graphile-migrate = final.callPackage ./nix/graphile-migrate.nix { src = migrate; };
      sirimbo-backend = final.callPackage ./backend/package.nix {};
      sirimbo-frontend = final.callPackage ./frontend/package.nix {};
      sirimbo-migrations = final.callPackage ./migrations/package.nix {};
    };

    packages.x86_64-linux = {
      inherit (pkgs)
        squawk ncc sirimbo-frontend sirimbo-backend graphile-migrate sirimbo-migrations;
    };

    devShell.x86_64-linux = pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.graphile-migrate
        pkgs.yarn
        pkgs.nodePackages.typescript
        pkgs.yarn2nix
        pkgs.postgresql
        pkgs.ncc
        pkgs.squawk
      ];
    };

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModule
        { nixpkgs.overlays = [ self.overlay ]; }
        ({ pkgs, ... }: {
          boot.isContainer = true;
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 3000 3306 5432 8025 1025 9000 ];
          environment.systemPackages = [ pkgs.file ];

          services.postgresql = {
            enable = true;
            enableTCPIP = true;
            package = pkgs.postgresql_13;
            extraPlugins = with pkgs.postgresql_13.pkgs; [ plpgsql_check ];
            ensureDatabases = ["olymp" "olymp_shadow"];
            ensureUsers = [
              {
                name = "olymp";
                ensurePermissions = {
                  "DATABASE olymp" = "ALL PRIVILEGES";
                  "ALL TABLES IN SCHEMA public" = "ALL";
                };
              }
              { name = "olympuser"; }
            ];
            authentication = "host all all all trust";
          };

          services.mailhog.enable = true;
          services.olymp-beta = {
            enable = true;
            dbConnString = "dbname=olymp";
            stateDir = "/var/lib/olymp";
            domain = "olymp-test";
            ssl = false;
            jsPort = 4000;
            frontendPort = 3030;

            smtpAuth = false;
            smtpTLS = false;
            smtpHost = "127.0.0.1";
            smtpPort = 1025;
          };
        })
      ];
    };
  };
}
