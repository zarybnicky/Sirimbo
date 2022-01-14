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

    minioPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} internal Minio port";
      default = 9000;
    };
    minioAccessKey = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} Minio access key";
    };
    minioSecretKey = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} Minio secret key";
    };

    dbConnString = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} DB connection string";
    };

    smtpAuth = lib.mkEnableOption "${pkgName} SMTP auth";
    smtpTLS = lib.mkEnableOption "${pkgName} SMTP TLS";
    smtpHost = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} SMTP host";
    };
    smtpPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} SMTP port";
    };
    smtpUser = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "${pkgName} SMTP username";
    };
    smtpPass = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "${pkgName} SMTP password";
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

      define('FRONTEND_HASH', '${builtins.substring 11 32 "${pkgs.sirimbo-frontend}"}');
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

      define('SMTP_AUTH', ${if cfg.smtpAuth then "true" else "false"});
      define('SMTP_TLS', ${if cfg.smtpTLS then "true" else "false"});
      define('SMTP_HOST', '${cfg.smtpHost}');
      define('SMTP_PORT', ${toString cfg.smtpPort});
      define('SMTP_USER', '${cfg.smtpUser}');
      define('SMTP_PASS', '${cfg.smtpPass}');
      EOS
    '';

    phpRoot = pkgs.symlinkJoin {
      name = "sirimbo-php-dist";
      paths = [pkgs.sirimbo-php pkgs.sirimbo-frontend configPhp];
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

      services.nginx = {
        enable = true;
        enableReload = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        virtualHosts.${cfg.domain} = {
          root = phpRoot;
          serverAliases = ["www.${cfg.domain}"];
          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

          locations."/graphql" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
            proxyWebsockets = true;
          };
          locations."/graphiql" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
            proxyWebsockets = true;
          };
          locations."/logout" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
          };
          locations."/upload" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
          };

          locations."/files/images/" = {
            # TODO: resizer
            proxyPass = "http://127.0.0.1:${toString cfg.minioPort}/";
          };
          locations."/files/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.minioPort}/";
          };

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
          mbstring session ctype exif gd zlib pdo_pgsql
        ]);
      };

      services.minio = {
        enable = true;
        browser = false;
        configDir = "${cfg.stateDir}/minio-config";
        dataDir = ["${cfg.stateDir}/minio-data"];
        accessKey = cfg.minioAccessKey;
        secretKey = cfg.minioSecretKey;
      };
      systemd.services.minio = {
        serviceConfig = {
          ExecStartPost= ''
            ${pkgs.coreutils}/bin/timeout 30 ${pkgs.bash}/bin/bash -c \
              'while ! ${pkgs.curl}/bin/curl --silent --fail http://localhost:${toString cfg.minioPort}/minio/health/cluster; do sleep 1; done'
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
          WorkingDirectory = config.services.minio.configDir;
        };
        script = ''
          set -e
          mc --config-dir . config host add minio \
            http://localhost:${toString cfg.minioPort} "${cfg.minioAccessKey}" "${cfg.minioSecretKey}"
          mc --config-dir . mb minio/public
          mc --config-dir . mb minio/private
          mc --config-dir . policy set download minio/public
        '';
      };

      systemd.services.sirimbo-backend = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        environment.PGDATABASE = "olymp";
        environment.PGHOST = "/run/postgresql";
        environment.PORT = toString cfg.jsPort;
        environment.SMTP_AUTH = if cfg.smtpAuth then "1" else "";
        environment.SMTP_TLS = if cfg.smtpTLS then "1" else "";
        environment.SMTP_HOST = cfg.smtpHost;
        environment.SMTP_PORT = toString cfg.smtpPort;
        environment.SMTP_USER = cfg.smtpUser;
        environment.SMTP_PASS = cfg.smtpPass;
        environment.MINIO_ACCESS_KEY = cfg.minioAccessKey;
        environment.MINIO_SECRET_KEY = cfg.minioSecretKey;
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.nodejs}/bin/node ${pkgs.sirimbo-backend}/bin/sirimbo-backend";
          Restart = "always";
          RestartSec = "10s";
        };
      };

      systemd.services.sirimbo-migrate = {
        description = "${pkgName} Migrations";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "postgresql.service" ];
        environment.DATABASE_URL = "postgres://${cfg.user}@localhost/olymp";
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          Type = "oneshot";
          RemainAfterExit = "true";
          WorkingDirectory = pkgs.sirimbo-migrations;
          ExecStart = "${pkgs.graphile-migrate}/bin/graphile-migrate migrate";
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
