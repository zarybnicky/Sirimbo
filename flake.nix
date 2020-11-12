{
  inputs.co-log-src = { flake = false; url = github:kowainik/co-log/main; };
  inputs.typerep-map = { flake = false; url = github:kowainik/typerep-map/main; };
  inputs.servant-websockets = { flake = false; url = github:moesenle/servant-websockets/master; };
  inputs.gitignore = { url = github:hercules-ci/gitignore.nix/master; flake = false; };

  outputs = { self, nixpkgs, gitignore, co-log-src, typerep-map, servant-websockets }: let
    inherit (nixpkgs.lib) composeExtensions flip genAttrs;
    inherit (import gitignore { inherit (nixpkgs) lib; }) gitignoreSource;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    src = gitignoreSource ./.;

    co-log = pkgs.runCommand "co-log-source" {} ''
      mkdir -p $out
      cd $out
      cp -rL ${co-log-src}/{co-log,co-log-core} $out
    '';

    hsOverrides = self: with pkgs.haskell.lib; {
      typerep-map = doJailbreak (dontCheck (self.callCabal2nix "typerep-map" typerep-map {}));
      co-log = doJailbreak (dontCheck (self.callCabal2nix "co-log" "${co-log}/co-log" {}));
      co-log-core = self.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
      servant-websockets = self.callCabal2nix "servant-websockets" servant-websockets {};
    };

    hsPackagesSrc = [
      "sirimbo-api"
      "sirimbo-schema"
      "sirimbo-tournament"
      "sirimbo-yt-worker"
    ];

  in {
    overlay = final: prev: {
      sirimbo-tournament-frontend = src + "/sirimbo-tournament/public";
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = composeExtensions (old.overrides or (_: _: {})) (hself: hsuper:
          (genAttrs (x: hself.callCabal2nix x (src + "/${x}") {}) hsPackagesSrc)
          // hsOverrides hself
        );
      });
    };

    packages.x86_64-linux = genAttrs hsPackagesSrc (x: builtins.getAttr x pkgs.haskellPackages);
    devShell.x86_64-linux = pkgs.haskellPackages.shellFor {
      packages = p: map (flip builtins.getAttr p) hsPackagesSrc;
      buildInputs = [
        pkgs.yarn
        pkgs.php73Packages.phpstan
      ];
    };

    nixosModules = {
      olymp-api = { config, lib, pkgs, ... }: let
        cfg = config.services.olymp-api;
        pkgName = "api.tkolymp.cz";
      in {
        options.services.olymp-api = {
          enable = lib.mkEnableOption "${pkgName}";
          user = lib.mkOption {
            type = lib.types.str;
            default = "olymp";
            description = "${pkgName} user";
          };
          group = lib.mkOption {
            type = lib.types.str;
            default = "olymp";
            description = "${pkgName} group";
          };
          domain = lib.mkOption {
            type = lib.types.str;
            description = "${pkgName} Nginx vhost domain";
            example = "api.tkolymp.cz";
          };
          port = lib.mkOption {
            type = lib.types.int;
            description = "${pkgName} internal port";
            example = 3000;
          };
          dbHost = lib.mkOption {
            type = lib.types.string;
            description = "${pkgName} DB host";
          };
          dbUser = lib.mkOption {
            type = lib.types.string;
            description = "${pkgName} DB user";
          };
          dbPassword = lib.mkOption {
            type = lib.types.string;
            description = "${pkgName} DB password";
          };
          dbDatabase = lib.mkOption {
            type = lib.types.string;
            description = "${pkgName} DB database";
          };
        };

        config = lib.mkIf cfg.enable {
          users.users.${cfg.user} = {
            name = cfg.user;
            group = cfg.group;
            description = pkgName;
            isSystemUser = true;
          };
          users.groups.${cfg.user} = {
            name = cfg.group;
          };

          services.nginx = {
            enable = true;
            enableReload = true;
            recommendedGzipSettings = true;
            recommendedOptimisation = true;
            recommendedProxySettings = true;
            virtualHosts.${cfg.domain}.locations."/" = {
              proxyPass = "http://localhost:${toString cfg.port}";
              proxyWebsockets = true;
            };
          };

          systemd.services.olymp-api = {
            description = "${pkgName} Webserver";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" ];
            environment = {
              CONFIG = pkgs.writeText "config.yaml" ''
                dbHost: ${cfg.dbHost}
                dbUser: ${cfg.dbUser}
                dbPassword: ${cfg.dbPassword}
                dbDatabase: ${cfg.dbDatabase}
              '';
            };
            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              Restart = "always";
              ExecStart = "${self.packages.x86_64-linux.sirimbo-api}/bin/server";
            };
          };
        };
      };

    };
  };
}
