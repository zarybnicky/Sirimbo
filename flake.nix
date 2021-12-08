{
  inputs.gogol = { flake = false; url = github:brendanhay/gogol/develop; };
  inputs.unstable.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs, unstable, gogol }: let
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
      hasura-graphql-engine = unstablePkgs.hasura-graphql-engine.overrideAttrs (oldAttrs: {
        VERSION = unstablePkgs.hasura-graphql-engine.version;
      });
      hasura-cli = unstablePkgs.hasura-cli;
      hasura-cli-ext = final.callPackage ./nix/cli-ext.nix {};
      hasura-cli-full = final.callPackage ./nix/cli-full.nix {};
      hasura-console-assets = final.callPackage ./nix/console-assets.nix {};
      phpstan = final.callPackage ./nix/phpstan.nix {};

      haskell = prev.haskell // (let
        inherit (prev.haskell.lib) doJailbreak dontCheck justStaticExecutables
          generateOptparseApplicativeCompletion unmarkBroken;
      in {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: {
          servant-JuicyPixels = doJailbreak (unmarkBroken hsuper.servant-JuicyPixels);
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
      inherit (pkgs) sirimbo-php sirimbo-app;
      inherit (hsPkgs) sirimbo-api;
    };

    devShell.x86_64-linux = hsPkgs.shellFor {
      withHoogle = true;
      packages = p: [ p.sirimbo-api ];
      buildInputs = [
        hsPkgs.cabal-install
        hsPkgs.haskell-language-server
        hsPkgs.stan
        pkgs.entr
        pkgs.yarn
        pkgs.phpstan
        pkgs.nodePackages.typescript
        pkgs.sass
        pkgs.yarn2nix
        pkgs.hasura-cli-full
        pkgs.hasura-graphql-engine
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
          networking.firewall.allowedTCPPorts = [ 80 3000 3306 ];
          environment.systemPackages = [ pkgs.file ];
          services.mysql = {
            enable = true;
            package = pkgs.mariadb;
            ensureDatabases = ["olymp"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = { "olymp.*" = "ALL PRIVILEGES"; };
            }];
          };
          services.postgresql = {
            enable = true;
            ensureDatabases = ["root" "olymp"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = {
                "DATABASE olymp" = "ALL PRIVILEGES";
                "ALL TABLES IN SCHEMA public" = "ALL";
              };
            }];
          };
          services.olymp = {
            enable = true;
            dbConnString = "dbname=olymp";
            stateDir = "/var/lib/olymp";
            domain = "olymp-test";
            internalPort = 3000;
            proxyPort = 3010;
          };
        })
      ];
    };
  };
}
