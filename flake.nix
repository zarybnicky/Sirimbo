{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
    devenv.url = github:cachix/devenv;
    utils.url = "github:numtide/flake-utils";
    graphile-migrate-flake.url = github:zarybnicky/graphile-migrate-flake;
    yarnpnp2nix = {
      url = "github:madjam002/yarnpnp2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "utils";
    };
  };

  outputs = { self, nixpkgs, devenv, yarnpnp2nix, graphile-migrate-flake, ... } @ inputs: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [
        graphile-migrate-flake.overlays.default
        self.overlays.default
      ];
    };
  in {
    nixosModules.default = ./module.nix;

    overlays.default = final: prev: let
      yarnPackages = yarnpnp2nix.lib.x86_64-linux.mkYarnPackagesFromManifest {
        inherit pkgs;
        yarnManifest = import ./yarn-manifest.nix;
        packageOverrides = {
          "prettier@npm:3.1.0" = {
            outputHash = "sha512-4ZWNMZniyTyR3gJpz47HZf0X3V5kRSmhRsTpZ6QVnczd90OqPgyfJYC3SN1CtMX3IzOuls1JH9UXL/ILy+/AJA==";
          };
          "mjml-core@patch:mjml-core@npm%3A4.14.1#./.yarn/patches/mjml-core-npm-4.14.1-e6ad05b5d7.patch::version=4.14.1&hash=89aa1f&locator=rozpisovnik%40workspace%3A." = {
            outputHash = "sha512-0Ovf7e1Ksrlwig48a0mmiv3XGkxGDrtYYX9I3bxiH6rW0fNKSj8dr4jf+p+D7OD/QQNCfz4jnJ6UUKjMhUjqCA==";
          };
          "typescript@patch:typescript@npm%3A5.1.6#optional!builtin<compat/typescript>::version=5.1.6&hash=5da071" = {
            outputHash = "sha512-Pu+UjhDHG5YXLrzkccAx07evsSpI/urLrawYdsC06bDA/BZ3vvU9QWZJcKF0qe4C+dCb4mAoFXHJDo704iV0zw==";
          };
          "rozpisovnik-api@workspace:backend" = {
            shouldBeUnplugged = true;
            build = "node build.cjs";
          };
        };
      };

    in {
      prettier = yarnPackages."prettier@npm:3.1.0";
      squawk = yarnPackages."squawk-cli@npm:0.24.2";
      commitlint = yarnPackages."@commitlint/cli@npm:17.7.1";
      typescript = yarnPackages."typescript@patch:typescript@npm%3A5.1.6#optional!builtin<compat/typescript>::version=5.1.6&hash=5da071";

      rozpisovnik-api = yarnPackages."rozpisovnik-api@workspace:backend";
      rozpisovnik-api-migrations = final.runCommand "rozpisovnik-api-migrations" {} ''
        mkdir -p $out
        cp -r ${./migrations} $out/migrations
        cp -r ${./.gmrc} $out/.gmrc
      '';
    };

    devenv-up = self.devShells.x86_64-linux.default.config.procfileScript;
    devShells.x86_64-linux.default = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        ({ pkgs, ... }: {
          packages = [
            pkgs.commitizen
            pkgs.prettier
            pkgs.graphile-migrate
            pkgs.typescript
            pkgs.yarn
            pkgs.nodejs
            pkgs.postgresql_15
            pkgs.sqlint
            pkgs.pgformatter
            pkgs.squawk
          ];

          pre-commit.hooks.commitizen.enable = true;

          processes = {
            backend.exec = "yarn workspace rozpisovnik-api start";
            frontend.exec = "yarn workspace rozpisovnik-web dev";
            migrate.exec = "graphile-migrate watch";
            schema.exec = "yarn schema";
          };
        })
      ];
    };

    packages.x86_64-linux = {
      inherit (pkgs)
        graphile-migrate
        rozpisovnik-api
        rozpisovnik-api-migrations;
    };

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.default
        { nixpkgs.overlays = [ self.overlays.default ]; }
        ({ config, pkgs, ... }: {
          boot.isContainer = true;
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          system.stateVersion = "23.05";
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 3000 3306 5432 8025 1025 9000 ];

          services.postgresql = {
            enable = true;
            enableTCPIP = true;
            package = pkgs.postgresql_15;
            extraPlugins = with pkgs.postgresql_15.pkgs; [ plpgsql_check pg_cron ];
            ensureDatabases = ["olymp" "olymp_shadow"];
            ensureUsers = [
              {
                name = "olymp";
                ensurePermissions = {
                  "DATABASE olymp" = "ALL PRIVILEGES";
                  "ALL TABLES IN SCHEMA public" = "ALL";
                };
              }
            ];
            authentication = "host all all all trust";
            settings = {
              shared_preload_libraries = "pg_stat_statements,pg_cron";
              "pg_stat_statements.track" = "all";
              "cron.database_name" = "olymp";
              "cron.use_background_workers" = "on";
              max_worker_processes = "20";
            };
          };

          services.mailhog.enable = true;
          services.olymp = {
            stateDir = "/var/lib/olymp";

            backend = {
              enable = true;
              domain = "olymp-test";
              # debug = true;
              ssl = false;
              port = 5000;
              database = "olymp";
              jwtSecret = "1111111111";
            };
            smtp = {
              auth = false;
              tls = false;
              host = "127.0.0.1";
              port = 1025;
            };
            s3 = {
              bucket = "public";
              region = "us-west-1";
              endpoint = "http://olymp-test:9000";
              accessKeyId = "00000000";
              secretAccessKey = "000000000000";
            };
          };

          services.minio = {
            enable = true;
            browser = false;
            listenAddress = ":9000";
            configDir = "/var/lib/olymp/minio-config";
            dataDir = ["/var/lib/olymp/minio-data"];
            accessKey = "00000000";
            secretKey = "000000000000";
          };

          systemd.services.minio = {
            serviceConfig = {
              ExecStartPost= ''
                ${pkgs.coreutils}/bin/timeout 30 ${pkgs.bash}/bin/bash -c \
                  'while ! ${pkgs.curl}/bin/curl --silent --fail http://localhost:9000/minio/health/cluster; do sleep 1; done'
              '';
            };
          };

          systemd.services.minio-config = {
            path = [pkgs.minio pkgs.minio-client];
            requiredBy = ["multi-user.target"];
            after = ["minio.service"];
            serviceConfig = {
              Type = "simple";
              User = "minio";
              Group = "minio";
              WorkingDirectory = "/var/lib/olymp/minio-config";
            };
            script = ''
              set -e
              mc --config-dir . config host add minio http://localhost:9000 "00000000" "000000000000"
              mc --config-dir . mb --ignore-existing minio/private
              mc --config-dir . mb --ignore-existing minio/public
              mc --config-dir . policy set download minio/public
            '';
          };
        })
      ];
    };
  };
}
