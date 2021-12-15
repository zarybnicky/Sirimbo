{
  inputs.gogol = { flake = false; url = github:brendanhay/gogol/develop; };
  inputs.migrate = { flake = false; url = github:graphile/migrate/main; };
  inputs.unstable.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs, unstable, gogol, migrate }: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;
    inherit (pkgs.nix-gitignore) gitignoreSourcePure gitignoreSource;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    unstablePkgs = import unstable {
      system = "x86_64-linux";
    };
    compiler = "ghc8104";
    hsPkgs = pkgs.haskell.packages.${compiler};
    getSrc = dir: gitignoreSourcePure [./.gitignore] dir;
  in {
    nixosModule = ./nix/module.nix;

    overlay = final: prev: {
      phpstan = final.callPackage ./nix/phpstan.nix {};

      haskell = prev.haskell // (let
        inherit (prev.haskell.lib) doJailbreak dontCheck justStaticExecutables
          generateOptparseApplicativeCompletion unmarkBroken;
      in {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: {
          higgledy = doJailbreak (unmarkBroken hsuper.higgledy);
          gogol-core = hself.callCabal2nix "gogol-core" "${gogol}/core" {};
          gogol = hself.callCabal2nix "gogol" "${gogol}/gogol" {};
          gogol-youtube = hself.callCabal2nix "gogol-youtube" "${gogol}/gogol-youtube" {};
          sirimbo-api = generateOptparseApplicativeCompletion "olymp" (
            justStaticExecutables (
              hself.callCabal2nix "sirimbo-api" (getSrc ./sirimbo-api) {}
            )
          );
        });
      });

      inherit (final.haskell.packages.${compiler}) sirimbo-api;

      graphile-migrate = final.mkYarnPackage {
        name = "graphile-migrate";
        src = migrate;
        packageJSON = "${migrate}/package.json";
        yarnLock = "${migrate}/yarn.lock";
        buildPhase = "yarn --offline run prepack";
      };

      sirimbo-migrations = final.linkFarm "sirimbo-migrations" [
        { name = "migrations"; path = ./migrations; }
        { name = ".gmrc"; path = ./.gmrc; }
      ];

      sirimbo-backend = final.mkYarnPackage {
        src = getSrc ./backend;
        packageJSON = ./backend/package.json;
        yarnLock = ./backend/yarn.lock;
        buildPhase = ''
          # Inline watch-fixtures.sql
          sed -i \
            -e '/readFile(WATCH_FIXTURES_PATH,/d' \
            -e 's|const WATCH_FIXTURES_PATH.*|var watchSqlInner = require("!!raw-loader!../../res/watch-fixtures.sql").default;|' \
            node_modules/graphile-build-pg/node8plus/plugins/PgIntrospectionPlugin.js
          cat node_modules/graphile-build-pg/node8plus/plugins/PgIntrospectionPlugin.js
          yarn --offline run build
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp deps/sirimbo-backend/dist/bundle.js $out/bin/sirimbo-backend
        '';
        distPhase = "true";
      };

      sirimbo-app = final.callPackage ./nix/sirimbo-app.nix {
        src = getSrc ./sirimbo-app;
        packageJSON = ./sirimbo-app/package.json;
        yarnLock = ./sirimbo-app/yarn.lock;
      };

      sirimbo-php = (final.callPackage ./sirimbo-php/composer-project.nix {
        php = final.php74;
      } (getSrc ./sirimbo-php)).overrideAttrs (oldAttrs: {
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
      inherit (pkgs) sirimbo-php sirimbo-app sirimbo-backend graphile-migrate sirimbo-migrations;
    };

    devShell.x86_64-linux = hsPkgs.shellFor {
      withHoogle = true;
      packages = p: [ p.sirimbo-api ];
      buildInputs = [
        hsPkgs.cabal-install
        hsPkgs.haskell-language-server
        hsPkgs.stan
        hsPkgs.prune-juice
        pkgs.entr
        pkgs.graphile-migrate
        pkgs.yarn
        pkgs.phpstan
        pkgs.nodePackages.typescript
        pkgs.sass
        pkgs.yarn2nix
        pkgs.postgresql
      ];
      DATABASE_URL = "postgres://olymp@olymp-test/olymp";
      SHADOW_DATABASE_URL = "postgres://olymp@olymp-test/olymp_shadow";
      ROOT_DATABASE_URL = "postgres://postgres@olymp-test/postgres";
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
          networking.firewall.allowedTCPPorts = [ 80 3000 3306 5432 ];
          environment.systemPackages = [ pkgs.file ];
          services.postgresql = {
            enable = true;
            enableTCPIP = true;
            ensureDatabases = ["olymp" "olymp_shadow"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = {
                "DATABASE olymp" = "ALL PRIVILEGES";
                "ALL TABLES IN SCHEMA public" = "ALL";
              };
            }];
            authentication = "host all all all trust";
          };
          services.olymp = {
            enable = true;
            dbConnString = "dbname=olymp";
            stateDir = "/var/lib/olymp";
            domain = "olymp-test";
            phpPort = 3010;
            jsPort = 3020;
          };
        })
      ];
    };
  };
}
