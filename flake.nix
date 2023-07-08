{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
  inputs.devenv.url = github:cachix/devenv;
  inputs.migrate = { flake = false; url = github:graphile/migrate/main; };
  inputs.utils.url = "github:numtide/flake-utils";
  inputs.yarnpnp2nix.url = "github:madjam002/yarnpnp2nix";
  inputs.yarnpnp2nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.yarnpnp2nix.inputs.utils.follows = "utils";

  outputs = { self, nixpkgs, devenv, migrate, utils, yarnpnp2nix } @ inputs: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlays.default ];
    };
  in {
    nixosModules.default = ./module.nix;

    overlays.default = final: prev: let
      yarnPackages = yarnpnp2nix.lib.x86_64-linux.mkYarnPackagesFromManifest {
        inherit pkgs;
        yarnManifest = import ./yarn-manifest.nix;
        packageOverrides = {
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
          "sirimbo-frontend@workspace:apps/custom-elements" = {
            shouldBeUnplugged = true;
            build = ''
              mkdir ../../libs
              ln -s ${./libs/branding-olymp} ../../libs/branding-olymp
              ln -s ${./libs/ui} ../../libs/ui
              webpack
            '';
          };
        };
      };

    in {
      graphile-migrate = yarnPackages."graphile-migrate@npm:1.4.1";

      rozpisovnik-api = yarnPackages."rozpisovnik-api@workspace:backend";
      rozpisovnik-api-migrations = final.callPackage ./migrations/package.nix {};

      sirimbo-frontend-old = final.runCommand "sirimbo-frontend-old" {} ''
        cd ${yarnPackages."sirimbo-frontend@workspace:apps/custom-elements".package}/node_modules/sirimbo-frontend
        mkdir -p $out/public
        cp -r dist/* $out/public/
      '';

      sirimbo-php = (final.callPackage ./backend-php/composer-project.nix {
        php = final.php82;
      } (
        final.nix-gitignore.gitignoreSourcePure [./.gitignore] ./backend-php
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
      });
    };

    devShells.x86_64-linux.default = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        ({ pkgs, ... }: {
          packages = [
            pkgs.graphile-migrate
            pkgs.yarn
            pkgs.nodejs
            pkgs.postgresql_13
            pkgs.sqlint
            pkgs.pgformatter
          ];

          enterShell = ''
          '';

          processes.run.exec = "hello";
        })
      ];
    };

    packages.x86_64-linux = {
      inherit (pkgs)
        graphile-migrate
        rozpisovnik-api
        rozpisovnik-api-migrations
        sirimbo-frontend-old
        sirimbo-php;
    };

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.default
        { nixpkgs.overlays = [ self.overlays.default ]; }
        ({ pkgs, ... }: {
          boot.isContainer = true;
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          system.stateVersion = "23.05";
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
          services.olymp = {
            stateDir = "/var/lib/olymp";

            php = {
              enable = true;
              domain = "olymp-test";
              ssl = false;
            };
            backend = {
              enable = true;
              domain = "olymp-test";
              # debug = true;
              ssl = false;
              port = 4000;
              database = "olymp";
            };
            frontend = {
              enable = true;
              domain = "olymp-test";
              ssl = false;
              port = 3000;
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
