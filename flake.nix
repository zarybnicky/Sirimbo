{
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
      phpstan = final.callPackage ./nix/phpstan.nix {};
      ncc = final.callPackage ./nix/ncc.nix {};
      squawk = final.callPackage ./nix/squawk.nix {};
      graphile-migrate = final.callPackage ./nix/graphile-migrate.nix { src = migrate; };
      sirimbo-backend = final.callPackage ./backend/package.nix {};
      sirimbo-frontend = final.callPackage ./frontend/package.nix {};
      sirimbo-migrations = final.callPackage ./migrations/package.nix {};

      sirimbo-php = (final.callPackage ./sirimbo-php/composer-project.nix {
        php = final.php82;
      } (
        pkgs.nix-gitignore.gitignoreSourcePure [./.gitignore] ./sirimbo-php
      )).overrideAttrs (oldAttrs: {
        name = "sirimbo-php";
        buildInputs = oldAttrs.buildInputs ++ [ final.imagemagick ];
        buildPhase = "composer validate";
        installPhase = ''
          runHook preInstall
          mkdir -p $out
          mv $PWD/* $out/
          runHook postInstall
        '';
        doCheck = true;
        checkPhase = ''
          ${pkgs.php}/bin/php -f vendor/bin/twig-linter -- lint files/Templates
          ${pkgs.phpstan}/bin/phpstan analyse --level 5 files/
        '';
      });
    };

    packages.x86_64-linux = {
      inherit (pkgs)
        squawk ncc sirimbo-php sirimbo-frontend sirimbo-backend graphile-migrate sirimbo-migrations;
    };

    devShell.x86_64-linux = pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.entr
        pkgs.graphile-migrate
        pkgs.yarn
        pkgs.phpstan
        pkgs.nodePackages.typescript
        pkgs.yarn2nix
        pkgs.postgresql
        pkgs.ncc
        # pkgs.squawk
      ];
      DATABASE_URL = "postgres://olymp@olymp-test/olymp";
      SHADOW_DATABASE_URL = "postgres://olymp@olymp-test/olymp_shadow";
      ROOT_DATABASE_URL = "postgres://postgres@olymp-test/postgres";
      PGHOST = "olymp-test";
      PGUSER = "postgres";
      PGDATABASE = "olymp";
      SMTP_HOST = "olymp-test";
      SMTP_PORT = "1025";
      MINIO_DOMAIN = "olymp-test";
      MINIO_PORT = "9000";
      MINIO_ACCESS_KEY = "00000000";
      MINIO_SECRET_KEY = "000000000000";
      DOMAIN = "olymp-test";
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
          services.olymp = {
            enable = true;
            dbConnString = "dbname=olymp";
            stateDir = "/var/lib/olymp";
            domain = "olymp-test";
            ssl = false;
            phpPort = 3010;
            jsPort = 3020;

            smtpAuth = false;
            smtpTLS = false;
            smtpHost = "127.0.0.1";
            smtpPort = 1025;

            minioDomain = "cdn.olymp-test"; # doesn't work in local container
            minioPort = 9000;
            minioAccessKey = "00000000";
            minioSecretKey = "000000000000";
          };
        })
      ];
    };
  };
}
