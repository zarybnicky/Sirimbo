{
  inputs.gogol = { flake = false; url = github:brendanhay/gogol/develop; };
  inputs.migrate = { flake = false; url = github:graphile/migrate/main; };
  inputs.unstable.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs, unstable, gogol, migrate }: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    unstablePkgs = import unstable {
      system = "x86_64-linux";
    };
    compiler = "ghc8104";
    hsPkgs = pkgs.haskell.packages.${compiler};
  in {
    nixosModule = ./nix/module.nix;

    overlay = final: prev: {
      phpstan = final.callPackage ./nix/phpstan.nix {};
      ncc = final.callPackage ./nix/ncc.nix {};
      graphile-migrate = final.callPackage ./nix/graphile-migrate.nix { src = migrate; };
      sirimbo-backend = final.callPackage ./backend/package.nix {};
      sirimbo-frontend = final.callPackage ./frontend/package.nix {};
      sirimbo-migrations = final.callPackage ./migrations/package.nix {};

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
              hself.callCabal2nix "sirimbo-api" (
                pkgs.nix-gitignore.gitignoreSourcePure [./.gitignore] ./sirimbo-api
              ) {}
            )
          );
        });
      });

      inherit (final.haskell.packages.${compiler}) sirimbo-api;

      sirimbo-php = (final.callPackage ./sirimbo-php/composer-project.nix {
        php = final.php74;
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
      inherit (pkgs) ncc sirimbo-php sirimbo-frontend sirimbo-backend graphile-migrate sirimbo-migrations;
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
        pkgs.ncc
      ];
      DATABASE_URL = "postgres://olymp@olymp-test/olymp";
      SHADOW_DATABASE_URL = "postgres://olymp@olymp-test/olymp_shadow";
      ROOT_DATABASE_URL = "postgres://postgres@olymp-test/postgres";
      DATABASE_VISITOR = "olympuser";
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
