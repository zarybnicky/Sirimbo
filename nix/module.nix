{ config, lib, pkgs, ... }: let
  cfg = config.services.olymp;
  pkgName = "tkolymp.cz";
in {
  options.services.olymp = {
    enable = lib.mkEnableOption "${pkgName}";
    yt-worker.enable = lib.mkEnableOption "${pkgName}";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} Nginx vhost domain";
      example = "tkolymp.cz";
    };

    haskellPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} internal Haskell port";
      example = 3000;
    };
    phpPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} internal PHP port";
      example = 3001;
    };
    jsPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} internal JS port";
      example = 3002;
    };
    hasuraPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} internal Hasura port";
      default = 8080;
    };

    dbConnString = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} DB connection string";
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
    stateDir = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} state directory";
    };
  };

  config = let
    cfgFile = pkgs.writeText "config.yaml" ''
      dbConnString: "${cfg.dbConnString}"
      proxyPort: ${toString cfg.phpPort}
    '';

    configPhp = pkgs.runCommand "sirimbo-php-config" {} ''
      mkdir -p $out
      cat > $out/config.php <<EOS
      <?php
      openlog('${cfg.domain}', LOG_ODELAY, LOG_USER);
      date_default_timezone_set('Europe/Paris');
      mb_internal_encoding('UTF-8');

      define('FRONTEND_HASH', '${builtins.substring 11 32 "${pkgs.sirimbo-app}"}');
      define('SENTRY_ENV', '${cfg.domain}');
      define('DB_CONN_STRING', 'pgsql:${cfg.dbConnString}');

      define('GALERIE', '${cfg.stateDir}/gallery');
      define('GALERIE_THUMBS', '${cfg.stateDir}/gallery/thumbnails');
      define('UPLOADS', '${cfg.stateDir}/uploads');
      define('CACHE', '${cfg.stateDir}/cache');
      foreach ([GALERIE, GALERIE_THUMBS, UPLOADS, CACHE] as \$path) {
        if (!is_readable(\$path)) {
          mkdir(\$path, 0777, true);
        }
      }
      define('NABOR', '0');
      define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');
      define('DEFAULT_ADMIN_MAIL', 'tkolymp@tkolymp.cz');
      EOS
    '';

    phpRoot = pkgs.symlinkJoin {
      name = "sirimbo-php-dist";
      paths = [pkgs.sirimbo-php pkgs.sirimbo-app configPhp];
    };
  in lib.mkMerge [
    (lib.mkIf cfg.enable {
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
      systemd.tmpfiles.rules = [ "d ${cfg.stateDir} 0755 ${cfg.user} ${cfg.user} -" ];

      environment.systemPackages = [
        (pkgs.runCommand "olymp" { buildInputs = [pkgs.makeWrapper]; } ''
          mkdir $out
          ln -s ${pkgs.sirimbo-api}/* $out
          rm $out/bin
          mkdir $out/bin
          makeWrapper ${pkgs.sirimbo-api}/bin/olymp $out/bin/olymp --set CONFIG "${cfgFile}"
        '')
      ];

      services.nginx = {
        enable = true;
        enableReload = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        virtualHosts.${"fpm." + cfg.domain} = {
          root = phpRoot;
          listen = [{ addr = "127.0.0.1"; port = cfg.phpPort; }];
          locations."/" = {
            index = "index.php";
            extraConfig = "try_files /public/$uri /index.php?$args;";
          };
          locations."~ \.php$".extraConfig = ''
            try_files $uri /index.php?$args;
            client_max_body_size 20M;
            fastcgi_split_path_info ^(.+\.php)(/.+)$;
            fastcgi_pass unix:${config.services.phpfpm.pools.${cfg.domain}.socket};
            fastcgi_index index.php;
            include ${pkgs.nginx}/conf/fastcgi_params;
            include ${pkgs.nginx}/conf/fastcgi.conf;
          '';
        };
        virtualHosts.${cfg.domain} = {
          root = phpRoot;
          serverAliases = ["www.${cfg.domain}"];
          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";
          locations."/".extraConfig = "try_files /public/$uri @proxy;";

          locations."/graphql/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.hasuraPort}/";
            proxyWebsockets = true;
          };
          locations."/backend/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}/";
            proxyWebsockets = true;
          };
          locations."@proxy" = {
            proxyPass = "http://127.0.0.1:${toString cfg.haskellPort}";
            proxyWebsockets = true;
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
          "pm.start_servers" = 3;
          "pm.min_spare_servers" = 2;
          "pm.max_spare_servers" = 4;
          "catch_workers_output" = true;
          "php_admin_flag[log_errors]" = true;
          "php_admin_value[memory_limit]" = "512M";
          "php_admin_value[upload_max_filesize]" = "40M";
          "php_admin_value[post_max_size]" = "40M";
        };
        phpPackage = pkgs.php.withExtensions ({ all, ... }: with all; [
          curl imagick opcache pdo openssl posix
          mbstring session json ctype exif gd zlib pdo_pgsql
        ]);
      };

      systemd.services.hasura = {
        description = "Hasura";
        wantedBy = ["multi-user.target"];
        after = ["network-online.target" "postgresql.service"];
        requires = ["postgresql.service"];
        path = [pkgs.postgresql];
        environment.HASURA_GRAPHQL_SERVER_PORT = toString cfg.hasuraPort;
        environment.HASURA_GRAPHQL_DATABASE_URL = "postgres:///olymp";
        environment.HASURA_GRAPHQL_ADMIN_SECRET = "superadmin";
        environment.HASURA_GRAPHQL_ENABLE_TELEMETRY = "false";
        environment.HASURA_GRAPHQL_AUTH_HOOK = "http://localhost:${toString cfg.haskellPort}/api/graphql-auth";
        environment.HASURA_GRAPHQL_ENABLED_LOG_TYPES = "startup, http-log, webhook-log, websocket-log, query-log";
        environment.HASURA_GRAPHQL_EXPERIMENTAL_FEATURES = "inherited_roles";
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          Type = "simple";
          ExecStart = "${pkgs.hasura-graphql-engine}/bin/graphql-engine serve";
        };
      };

      systemd.services.sirimbo-backend = {
        serviceConfig = {
          ExecStart = "${pkgs.nodejs}/bin/node ${pkgs.sirimbo-backend}/bin/sirimbo-backend";
          Restart = "always";
          RestartSec = "10s";
        };
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        environment.PORT = toString cfg.jsPort;
      };

      systemd.services.olymp-api-migrate = {
        description = "${pkgName} Migrations";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "postgresql.service" ];
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
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "olymp-api-migrate.service" "postgresql.service" ];
        environment.CONFIG = cfgFile;
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          Restart = "always";
          ExecStart = "${pkgs.sirimbo-api}/bin/olymp server --port ${toString cfg.haskellPort}";
        };
      };
    })

    (lib.mkIf cfg.yt-worker.enable {
      systemd.services.olymp-yt-worker = {
        description = "Olymp YouTube worker service";
        environment.CONFIG = cfgFile;
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
}
