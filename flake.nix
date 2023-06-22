{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/release-22.11;
  inputs.devenv.url = github:cachix/devenv;
  inputs.migrate = { flake = false; url = github:graphile/migrate/main; };

  outputs = { self, nixpkgs, devenv, migrate } @ inputs: let
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
      sirimbo-backend-beta = final.callPackage ./backend/package.nix {};
      sirimbo-frontend-beta = final.callPackage ./frontend/package.nix {};
      sirimbo-migrations-beta = final.callPackage ./migrations/package.nix {};
    };

    devShell.x86_64-linux = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        ({ pkgs, ... }: {
          packages = [
            pkgs.graphile-migrate
            pkgs.yarn
            pkgs.nodePackages.typescript
            pkgs.postgresql_13
            pkgs.ncc
            pkgs.sqlint
            pkgs.pgformatter
            # pkgs.squawk
          ];

          enterShell = ''
          '';

          processes.run.exec = "hello";
        })
      ];
    };

    packages.x86_64-linux = {
      inherit (pkgs) sirimbo-frontend-beta;
      sirimbo-frontend = pkgs.sirimbo-frontend-beta;
      sirimbo-backend = pkgs.sirimbo-backend-beta;
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
            ];
            authentication = "host all all all trust";
          };

          services.mailhog.enable = true;
          services.olymp-beta = {
            stateDir = "/var/lib/olymp";

            backend = {
              enable = true;
              domain = "olymp-test";
              # debug = true;
              ssl = false;
              port = 4000;
              database = "olymp";
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
