{ config, lib, pkgs, ... }: let
  cfg = config.services.olymp-beta;
  pkgName = "tkolymp.cz";
in {
  options.services.olymp-beta = {
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

    backend = {
      enable = lib.mkEnableOption "${pkgName}";
      port = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} GraphQL port";
        example = 3002;
      };

      domain = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Nginx vhost domain";
        example = "tkolymp.cz";
      };
      ssl = lib.mkEnableOption "${pkgName} enable ssl";
      debug = lib.mkEnableOption "${pkgName} enable debug mode";
      database = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Postgres DB";
      };
    };

    smtp = {
      auth = lib.mkEnableOption "${pkgName} SMTP auth";
      tls = lib.mkEnableOption "${pkgName} SMTP TLS";
      host = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} SMTP host";
      };
      port = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} SMTP port";
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "${pkgName} SMTP username";
      };
      pass = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "${pkgName} SMTP password";
      };
    };

    migrations = {
      enable = lib.mkEnableOption "${pkgName}";
    };

    s3 = {
      bucket = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} S3 bucket";
      };
      region = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} S3 region";
      };
      endpoint = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} S3 endpoint";
      };
      publicEndpoint = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "${pkgName} publicly accessible endpoint (Cloudflare URL)";
        default = null;
      };
      accessKeyId = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} AWS_ACCESS_KEY_ID";
      };
      secretAccessKey = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} AWS_SECRET_ACCESS_KEY";
      };
    };

    frontend = {
      enable = lib.mkEnableOption "${pkgName}";
      port = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} Next.js port";
        example = 3002;
      };
      backend = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "${pkgName} backend";
        default = null;
        example = "api.rozpisovnik.cz";
      };

      domain = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Nginx vhost domain";
        example = "tkolymp.cz";
      };
      ssl = lib.mkEnableOption "${pkgName} enable ssl";
    };

    php = {
      enable = lib.mkEnableOption "${pkgName}";
      port = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} PHP port";
        example = 3002;
      };
      domain = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Nginx vhost domain";
        example = "tkolymp.cz";
      };
      ssl = lib.mkEnableOption "${pkgName} enable ssl";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.backend.enable or cfg.frontend.enable) {
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
      systemd.tmpfiles.rules = [
        "d ${cfg.stateDir} 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/gallery 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/gallery/thumbnails 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/uploads 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/cache 0755 ${cfg.user} ${cfg.user} -"
      ];
    })

    (lib.mkIf cfg.frontend.enable {
      systemd.services.sirimbo-frontend-beta = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          PORT = toString cfg.frontend.port;
          GRAPHQL_BACKEND = if cfg.frontend.backend != null then cfg.frontend.backend else "http://localhost:${toString cfg.backend.port}";
          NEXT_PUBLIC_GRAPHQL_BACKEND = if cfg.frontend.backend != null then cfg.frontend.backend else cfg.backend.domain;
          NEXT_PUBLIC_SENTRY_ENVIRONMENT = cfg.frontend.domain;
        };

        preStart = ''
          mkdir -p $(readlink ${pkgs.sirimbo-frontend-beta}/.next/cache)
        '';

        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.nodejs}/bin/node ${pkgs.sirimbo-frontend-beta}/server.js";
          WorkingDirectory = cfg.stateDir;
          Restart = "always";
          RestartSec = "10s";
        };
      };

      services.nginx = {
        enable = true;
        enableReload = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;

        virtualHosts.${cfg.frontend.domain} = {
          enableACME = cfg.frontend.ssl;
          forceSSL = cfg.frontend.ssl;

          extraConfig = if cfg.frontend.domain != cfg.backend.domain then ''
            ignore_invalid_headers off;
            client_max_body_size 0;
            proxy_buffering off;
            if ($http_origin = '''){
              set $http_origin "*";
            }
            proxy_hide_header Access-Control-Allow-Origin;
            add_header Access-Control-Allow-Origin $http_origin always;
            add_header Access-Control-Allow-Credentials 'true' always;
            add_header Access-Control-Allow-Methods 'GET,OPTIONS,PATCH,DELETE,POST,PUT' always;
            add_header Access-Control-Allow-Headers 'X-Tenant-Id,X-CSRF-Token,X-Requested-With,Accept,Accept-Version,Content-Length,Content-MD5,Content-Type,Date,X-Api-Version' always;
          '' else "";

          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.frontend.port}";
            proxyWebsockets = true;
          };
        };
      };
    })

    (lib.mkIf cfg.php.enable (let
      configPhp = pkgs.runCommand "sirimbo-php-config" {} ''
        mkdir -p $out
        cat > $out/config.php <<EOS
        <?php
        openlog('${cfg.php.domain}', LOG_ODELAY, LOG_USER);
        date_default_timezone_set('Europe/Paris');
        mb_internal_encoding('UTF-8');

        define('FRONTEND_HASH', '${builtins.substring 11 32 "${pkgs.sirimbo-frontend-old}"}');
        define('SENTRY_ENV', '${cfg.php.domain}');
        define('DB_CONN_STRING', 'pgsql:host=localhost;port=5432;dbname=${cfg.backend.database};user=${cfg.user}');

        define('CACHE', '${cfg.stateDir}/cache');
        define('UPLOADS', '${cfg.stateDir}/uploads');
        define('GALERIE', '${cfg.stateDir}/gallery');
        define('GALERIE_THUMBS', '${cfg.stateDir}/gallery/thumbnails');

        define('DEFAULT_FROM_MAIL', 'root@tkolymp.cz');

        define('SMTP_AUTH', ${if cfg.smtp.auth then "true" else "false"});
        define('SMTP_TLS', ${if cfg.smtp.tls then "true" else "false"});
        define('SMTP_HOST', '${cfg.smtp.host}');
        define('SMTP_PORT', ${toString cfg.smtp.port});
        define('SMTP_USER', '${cfg.smtp.user}');
        define('SMTP_PASS', '${cfg.smtp.pass}');
        EOS
      '';
    in {
      services.nginx = {
        enable = true;
        enableReload = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;

        virtualHosts.${cfg.domain} = {
          root = pkgs.symlinkJoin {
            name = "sirimbo-php-dist";
            paths = [pkgs.sirimbo-php pkgs.sirimbo-frontend-old configPhp];
          };

          serverAliases = ["www.${cfg.domain}"];
          locations."/gallery".root = cfg.stateDir;
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
          curl imagick opcache pdo openssl posix filter
          mbstring session ctype exif gd zlib pdo_pgsql
        ]);
      };
    }))

    (lib.mkIf cfg.backend.enable {
      systemd.services.sirimbo-backend-beta = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          DEBUG = if cfg.backend.debug then "postgraphile:postgres,postgraphile:postgres:error" else "";
          PGDATABASE = cfg.backend.database;
          PGHOST = "/run/postgresql";
          PORT = toString cfg.backend.port;
          DOMAIN = cfg.backend.domain;
          SSL = if cfg.backend.ssl then "1" else "";
          STATE_DIR = cfg.stateDir;
          SMTP_AUTH = if cfg.smtp.auth then "1" else "";
          SMTP_TLS = if cfg.smtp.tls then "1" else "";
          SMTP_HOST = cfg.smtp.host;
          SMTP_PORT = toString cfg.smtp.port;
          SMTP_USER = cfg.smtp.user;
          SMTP_PASS = cfg.smtp.pass;
          AWS_ACCESS_KEY_ID = cfg.s3.accessKeyId;
          AWS_SECRET_ACCESS_KEY = cfg.s3.secretAccessKey;
          S3_BUCKET = cfg.s3.bucket;
          S3_REGION = cfg.s3.region;
          S3_ENDPOINT = cfg.s3.endpoint;
          S3_PUBLIC_ENDPOINT = if cfg.s3.publicEndpoint != null then cfg.s3.publicEndpoint else cfg.s3.endpoint;
        };

        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.nodejs}/bin/node ${pkgs.sirimbo-backend-beta}/bin/sirimbo-backend";
          WorkingDirectory = cfg.stateDir;
          Restart = "always";
          RestartSec = "10s";
        };
      };

      services.nginx = {
        enable = true;
        enableReload = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;

        virtualHosts.${cfg.backend.domain} = {
          enableACME = cfg.backend.ssl;
          forceSSL = cfg.backend.ssl;

          extraConfig = ''
            ignore_invalid_headers off;
            client_max_body_size 0;
            proxy_buffering off;
            if ($http_origin = '''){
              set $http_origin "*";
            }
            proxy_hide_header Access-Control-Allow-Origin;
            add_header Access-Control-Allow-Origin $http_origin always;
            add_header Access-Control-Allow-Credentials 'true' always;
            add_header Access-Control-Allow-Methods 'GET,OPTIONS,PATCH,DELETE,POST,PUT' always;
            add_header Access-Control-Allow-Headers 'X-Tenant-Id,X-CSRF-Token,X-Requested-With,Accept,Accept-Version,Content-Length,Content-MD5,Content-Type,Date,X-Api-Version' always;
          '';

          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

          locations."/member/download" = {
            proxyPass = "http://127.0.0.1:${toString cfg.backend.port}";
          };
          locations."/graphql" = {
            proxyPass = "http://127.0.0.1:${toString cfg.backend.port}";
            proxyWebsockets = true;
          };
          locations."/graphiql" = {
            proxyPass = "http://127.0.0.1:${toString cfg.backend.port}";
            proxyWebsockets = true;
          };
        };
      };
    })

    (lib.mkIf cfg.migrations.enable {
      systemd.services.sirimbo-migrate-beta = {
        description = "${pkgName} Migrations";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "postgresql.service" ];
        environment.DATABASE_URL = "postgres://${cfg.user}@localhost/${cfg.backend.database}";
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          Type = "oneshot";
          RemainAfterExit = "true";
          WorkingDirectory = pkgs.sirimbo-migrations-beta;
          ExecStart = "${pkgs.graphile-migrate}/bin/graphile-migrate migrate";
        };
      };
    })
  ];
}
