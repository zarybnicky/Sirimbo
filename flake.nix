{
  inputs.co-log-src = { flake = false; url = github:kowainik/co-log/main; };
  inputs.in-other-words = { flake = false; url = github:KingoftheHomeless/in-other-words/master; };
  inputs.typerep-map = { flake = false; url = github:kowainik/typerep-map/main; };
  inputs.higgledy = { flake = false; url = github:zarybnicky/higgledy/master; };
  inputs.bootstrap = { flake = false; url = github:twbs/bootstrap/v4.5.3; };

  outputs = { self, nixpkgs, co-log-src, in-other-words, typerep-map, higgledy, bootstrap }: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;
    inherit (pkgs.nix-gitignore) gitignoreSourcePure gitignoreSource;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    compiler = "ghc884";
    hsPkgs = pkgs.haskell.packages.${compiler};
    getSrc = dir: gitignoreSourcePure [./.gitignore] dir;

    co-log = pkgs.runCommand "co-log-source" {} ''
      mkdir -p $out
      cd $out
      cp -rL ${co-log-src}/{co-log,co-log-core} $out
    '';

  in {
    overlay = final: prev: let
      inherit (prev.haskell.lib) doJailbreak dontCheck justStaticExecutables
        generateOptparseApplicativeCompletion;
    in {
      haskell = prev.haskell // {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: {
          typerep-map = doJailbreak (dontCheck (hself.callCabal2nix "typerep-map" typerep-map {}));
          co-log = doJailbreak (dontCheck (hself.callCabal2nix "co-log" "${co-log}/co-log" {}));
          co-log-core = hself.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
          in-other-words = hself.callCabal2nix "in-other-words" in-other-words {};
          higgledy = hself.callCabal2nix "higgledy" higgledy {};

          sirimbo-schema = hself.callCabal2nix "sirimbo-schema" (getSrc ./sirimbo-schema) {};
          sirimbo-api = generateOptparseApplicativeCompletion "olymp" (
            justStaticExecutables (
              hself.callCabal2nix "sirimbo-api" (getSrc ./sirimbo-api) {}
            )
          );
        });
      };
      inherit (final.haskell.packages.${compiler}) sirimbo-api;
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
          cp -r public/{favicon.ico,images,robots.txt,scripts,style,webfonts} $out/public
          cp -r ${bootstrap}/* $out/bootstrap/
          cd $out
          ${final.sass}/bin/sass -t compact public/style/main.scss:$out/public/style.css
          ${final.php73Packages.composer}/bin/composer install -o
        '';
      };
    };

    packages.x86_64-linux = {
      inherit (pkgs) sirimbo-tournament-frontend sirimbo-php;
      inherit (hsPkgs) sirimbo-api sirimbo-schema;
    };

    devShell.x86_64-linux = hsPkgs.shellFor {
      withHoogle = true;
      packages = p: [ p.sirimbo-api p.sirimbo-schema ];
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
        self.nixosModule
        { nixpkgs.overlays = [ self.overlay ]; }
        ({ pkgs, ... }: {
          boot.isContainer = true;
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 3000 3306 ];
          services.mysql = {
            enable = true;
            package = pkgs.mariadb;
            ensureDatabases = ["olymp"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = { "olymp.*" = "ALL PRIVILEGES"; };
            }];
          };
          services.olymp = {
            dbHost = "localhost";
            dbUser = "olymp";
            dbDatabase = "olymp";
            dbPassword = null;
            php = {
              enable = true;
              domain = "olymp-test";
              stateDir = "/var/lib/olymp";
              withApi = "http://localhost:3000";
            };
            api = {
              enable = true;
              port = 3000;
            };
          };
        })
      ];
    };

    nixosModule = { config, lib, pkgs, ... }: let
      cfg = config.services.olymp;
      pkgName = "tkolymp.cz";
    in {
      options.services.olymp = {
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


        php = {
          enable = lib.mkEnableOption "${pkgName}";
          domain = lib.mkOption {
            type = lib.types.str;
            description = "${pkgName} Nginx vhost domain";
            example = "tkolymp.cz";
          };
          stateDir = lib.mkOption {
            type = lib.types.str;
            description = "${pkgName} state directory";
          };
          withApi = lib.mkOption {
            type = lib.types.str;
            description = "${pkgName} API address to passthru";
          };
        };

        api = {
          enable = lib.mkEnableOption "${pkgName}";
          vhost = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "${pkgName} Nginx vhost domain";
            example = "api.tkolymp.cz";
          };
          port = lib.mkOption {
            type = lib.types.int;
            description = "${pkgName} internal port";
            example = 3000;
          };
        };

        yt-worker = {
          enable = lib.mkEnableOption "${pkgName}";
        };
      };

      config = lib.mkMerge [
        (lib.mkIf (cfg.php.enable or cfg.api.enable) {
          users.users.${cfg.user} = {
            name = cfg.user;
            group = cfg.group;
            home = cfg.php.stateDir;
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
          };

          environment.systemPackages = let
            config = pkgs.writeText "config.yaml" ''
              dbHost: ${cfg.dbHost}
              dbUser: ${cfg.dbUser}
              dbDatabase: ${cfg.dbDatabase}
              ${if cfg.dbPassword != null then "dbPassword: ${cfg.dbPassword}" else ""}
            '';
          in [
            (pkgs.runCommand "olymp" { buildInputs = [ pkgs.makeWrapper ]; } ''
              mkdir $out
              ln -s ${pkgs.sirimbo-api}/* $out
              rm $out/bin
              mkdir $out/bin
              makeWrapper ${pkgs.sirimbo-api}/bin/olymp $out/bin/olymp --set CONFIG "${config}"
            '')
          ];
        })

        (lib.mkIf cfg.php.enable {
          services.nginx = {
            virtualHosts.${cfg.php.domain} = {
              serverAliases = ["www.${cfg.php.domain}"];
              locations."/galerie".root = "${cfg.php.stateDir}/gallery";
              locations."/".index = "index.php index.html index.htm";
              locations."/".extraConfig = "try_files /public/$uri /index.php?$args;";
              locations."/api" = {
                proxyPass = cfg.php.withApi;
                proxyWebsockets = true;
              };
              locations."~ \.php$".extraConfig = ''
                fastcgi_split_path_info ^(.+\.php)(/.+)$;
                fastcgi_pass unix:${config.services.phpfpm.pools.${cfg.php.domain}.socket};
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
                    openlog('${cfg.php.domain}', LOG_ODELAY, LOG_USER);

                    define('COOKIE_DOMAIN', '${cfg.php.domain}');
                    define('DB_SERVER', '${cfg.dbHost}');
                    define('DB_DATABASE', '${cfg.dbDatabase}');
                    define('DB_USER', '${cfg.dbUser}');
                    define('DB_PASS', ${if cfg.dbPassword == null then "NULL" else "'${cfg.dbPassword}'"});

                    define('LOGS', '${cfg.php.stateDir}/logs');
                    define('GALERIE', '${cfg.php.stateDir}/gallery');
                    define('GALERIE_THUMBS', '${cfg.php.stateDir}/gallery/thumbnails');
                    define('UPLOADS', '${cfg.php.stateDir}/uploads');
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

          services.phpfpm.pools.${cfg.php.domain} = {
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
        })

        (lib.mkIf (cfg.api.vhost != null) {
          services.nginx = {
            virtualHosts.${cfg.api.vhost}.locations."/" = {
              proxyPass = "http://localhost:${toString cfg.api.port}";
              proxyWebsockets = true;
            };
          };
        })

        (lib.mkIf cfg.api.enable {
          systemd.services.olymp-api-migrate = {
            description = "${pkgName} Migrations";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" "mysql.service" ];
            requires = [ "mysql.service" ];
            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              Type = "oneshot";
              ExecStart = "${pkgs.coreutils}/bin/true";
              RemainAfterExit = "true";
              # ExecStart = "${self.packages.x86_64-linux.sirimbo-api}/bin/olymp migrate --execute";
            };
          };

          systemd.services.olymp-api = {
            description = "${pkgName} Webserver";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" ];
            requires = [ "olymp-api-migrate.service" ];
            environment.DB_HOST = cfg.dbHost;
            environment.DB_USER = cfg.dbUser;
            environment.DB_PASSWORD = cfg.dbPassword;
            environment.DB_DATABASE = cfg.dbDatabase;
            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              Restart = "always";
              ExecStart = "${pkgs.sirimbo-api}/bin/olymp server --port ${toString cfg.api.port}";
            };
          };
        })

        (lib.mkIf cfg.yt-worker.enable {
          systemd.services.olymp-yt-worker = {
            description = "Olymp YouTube worker service";
            serviceConfig = {
              Type = "simple";
              ExecStart = "${pkgs.sirimbo-api}/bin/olymp check-youtube";
            };
          };
          systemd.timers.olymp-yt-worker = {
            description = "Olymp YouTube worker service timer";
            wantedBy = ["multi-user.service"];
            timerConfig = {
              Unit = "olymp-yt-worker.service";
              OnBootSec = "10min";
              OnUnitActiveSec = "70min";
            };
          };
        })
      ];
    };
  };
}
