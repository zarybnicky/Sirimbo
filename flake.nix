{
  inputs.co-log-src = { flake = false; url = github:kowainik/co-log/main; };
  inputs.typerep-map = { flake = false; url = github:kowainik/typerep-map/main; };
  inputs.bootstrap = { flake = false; url = github:twbs/bootstrap/main; };

  outputs = { self, nixpkgs, co-log-src, typerep-map, bootstrap }: let
    inherit (nixpkgs.lib) composeExtensions flip mapAttrs mapAttrsToList;
    inherit (pkgs.nix-gitignore) gitignoreSourcePure gitignoreSource;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    hsPkgs = pkgs.haskell.packages.ghc884;
    getSrc = dir: gitignoreSourcePure [./.gitignore] dir;

    co-log = pkgs.runCommand "co-log-source" {} ''
      mkdir -p $out
      cd $out
      cp -rL ${co-log-src}/{co-log,co-log-core} $out
    '';

    hsOverrides = self: with pkgs.haskell.lib; {
      typerep-map = doJailbreak (dontCheck (self.callCabal2nix "typerep-map" typerep-map {}));
      co-log = doJailbreak (dontCheck (self.callCabal2nix "co-log" "${co-log}/co-log" {}));
      co-log-core = self.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
    };

    hsPackagesSrc = {
      "sirimbo-api" = getSrc ./sirimbo-api;
      "sirimbo-schema" = getSrc ./sirimbo-schema;
      "sirimbo-tournament" = getSrc ./sirimbo-tournament;
      "sirimbo-yt-worker" = getSrc ./sirimbo-yt-worker;
    };

  in {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper:
          (mapAttrs (name: src: hself.callCabal2nix name src {}) hsPackagesSrc) // hsOverrides hself
        );
      };
      sirimbo-tournament-frontend = final.stdenv.mkDerivation {
        name = "sirimbo-tournament-frontend";
        src = getSrc ./sirimbo-tournament-frontend;
        phases = "unpackPhase buildPhase";
        buildPhase = ''
          ${final.nodePackages.typescript}/bin/tsc
          ${final.sass}/bin/sass index.scss:index.css
          mkdir -p $out
          cp admin.html almond.js bundle.js{,.map} index.css{,.map} index.html react* $out/
        '';
      };
      sirimbo-php = final.stdenv.mkDerivation {
        name = "sirimbo-php";
        src = getSrc ./sirimbo-php;
        phases = "unpackPhase buildPhase";
        buildPhase = ''
          mkdir -p $out/public $out/bootstrap
          cp -r index.php files composer.json $out
          cp -r public/{favicon.ico,images,robots.txt,scripts,style{,.css},webfonts} $out/public
          cp -r ${bootstrap}/* $out/bootstrap/
          cd $out
          ${final.sass}/bin/sass -t compact public/style/main.scss:$out/public/style.css
          ${final.php73Packages.composer}/bin/composer install -o
        '';
      };
    };

    packages.x86_64-linux = {
      inherit (pkgs) sirimbo-tournament-frontend sirimbo-php;
    } // mapAttrs (x: _: builtins.getAttr x hsPkgs) hsPackagesSrc;

    devShell.x86_64-linux = hsPkgs.shellFor {
      packages = p: mapAttrsToList (name: _: builtins.getAttr name p) hsPackagesSrc;
      buildInputs = [
        hsPkgs.cabal-install
        hsPkgs.haskell-language-server
        pkgs.yarn
        pkgs.php73Packages.phpstan
        pkgs.nodePackages.typescript
        pkgs.sass
      ];
    };

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModules.olymp-php
        { nixpkgs.overlays = [ self.overlay ]; }
        ({ pkgs, ... }: {
          boot.isContainer = true;
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 ];
          services.mysql = {
            enable = true;
            package = pkgs.mariadb;
            ensureDatabases = ["olymp"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = { "olymp.*" = "ALL PRIVILEGES"; };
            }];
          };
          services.olymp-php = {
            enable = true;
            dbHost = "localhost";
            dbUser = "olymp";
            dbDatabase = "olymp";
            domain = "olymp-test";
            stateDir = "/var/lib/olymp";
          };
        })
      ];
    };

    nixosModules = let
      dbOpts = pkgName: lib: {
        dbHost = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} DB host";
        };
        dbUser = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} DB user";
        };
        dbPassword = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "${pkgName} DB password";
        };
        dbDatabase = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} DB database";
        };
      };
    in {

      olymp-php = { config, lib, pkgs, ... }: let
        cfg = config.services.olymp-php;
        pkgName = "tkolymp.cz";
      in {
        options.services.olymp-php = dbOpts pkgName lib // {
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
            example = "tkolymp.cz";
          };
          stateDir = lib.mkOption {
            type = lib.types.str;
            description = "${pkgName} state directory";
          };
        };

        config = lib.mkIf cfg.enable {
          users.users.${cfg.user} = {
            name = cfg.user;
            group = cfg.group;
            home = cfg.stateDir;
            description = pkgName;
            createHome = true;
            useDefaultShell = true;
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
            virtualHosts.${cfg.domain} = {
              serverAliases = ["www.${cfg.domain}"];
              locations."/galerie".root = "${cfg.stateDir}/gallery";
              locations."/".index = "index.php index.html index.htm";
              locations."/".extraConfig = "try_files /public/$uri /index.php?$args;";
              locations."~ \.php$".extraConfig = ''
                fastcgi_split_path_info ^(.+\.php)(/.+)$;
                fastcgi_pass unix:${config.services.phpfpm.pools.${cfg.domain}.socket};
                fastcgi_index index.php;
                include ${pkgs.nginx}/conf/fastcgi_params;
                include ${pkgs.nginx}/conf/fastcgi.conf;
              '';
              root = pkgs.symlinkJoin {
                name = "sirimbo-php-dist";
                paths = [
                  pkgs.sirimbo-php
                  (pkgs.runCommand "sirimbo-php-config" {} ''
                    mkdir -p $out
                    cat > $out/config.php <<EOS
                    <?php

                    openlog('${cfg.domain}', LOG_ODELAY, LOG_USER);

                    define('DB_SERVER', '${cfg.dbHost}');
                    define('DB_DATABASE', '${cfg.dbDatabase}');
                    define('DB_USER', '${cfg.dbUser}');
                    define('DB_PASS', ${if cfg.dbPassword == null then "NULL" else "'${cfg.dbPassword}'"});

                    define('LOGS', '${cfg.stateDir}/logs');
                    define('GALERIE', '${cfg.stateDir}/gallery');
                    define('GALERIE_THUMBS', '${cfg.stateDir}/gallery/thumbnails');
                    define('UPLOADS', '${cfg.stateDir}/uploads');
                    foreach ([LOGS, GALERIE, GALERIE_THUMBS, UPLOADS] as \$path) {
                      if (!is_readable(\$path)) {
                        mkdir(\$path, 0777, true);
                      }
                    }

                    define('NABOR', '0');

                    date_default_timezone_set('Europe/Paris');

                    define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');
                    define('DEFAULT_ADMIN_MAIL', 'tkolymp@tkolymp.cz');
                    EOS
                  '')
                ];
              };
            };
          };

          services.phpfpm.pools.${cfg.domain} = {
            user = cfg.user;
            settings = {
              "listen.owner" = config.services.nginx.user;
              "pm" = "dynamic";
              "pm.max_children" = 50;
              "pm.max_requests" = 500;
              "pm.start_servers" = 2;
              "pm.min_spare_servers" = 2;
              "pm.max_spare_servers" = 5;
              "php_admin_value[error_log]" = "syslog";
              "php_admin_flag[log_errors]" = true;
              "catch_workers_output" = true;
            };
            phpPackage = pkgs.php.withExtensions ({ all, ... }: with all; [
              curl imagick opcache pdo_mysql pdo mysqlnd mysqli openssl posix
              mbstring session json
            ]);
          };

        };
      };

      olymp-api = { config, lib, pkgs, ... }: let
        cfg = config.services.olymp-api;
        pkgName = "api.tkolymp.cz";
      in {
        options.services.olymp-api = dbOpts pkgName lib // {
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
              PORT = toString cfg.port;
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
