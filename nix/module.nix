{ config, lib, pkgs, ... }: let
  cfg = config.services.olymp;
  pkgName = "tkolymp.cz";
in {
  options.services.olymp = {
    enable = lib.mkEnableOption "${pkgName}";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} Nginx vhost domain";
      example = "tkolymp.cz";
    };
    ssl = lib.mkEnableOption "${pkgName} enable ssl";

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
    frontendPort = lib.mkOption {
      type = lib.types.int;
      description = "${pkgName} internal JS port";
      example = 3003;
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

  config = lib.mkMerge [
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
      systemd.tmpfiles.rules = [
        "d ${cfg.stateDir} 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/gallery 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/gallery/thumbnails 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/uploads 0755 ${cfg.user} ${cfg.user} -"
        "d ${cfg.stateDir}/cache 0755 ${cfg.user} ${cfg.user} -"
      ];

      services.nginx = {
        enable = true;
        enableReload = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;

        virtualHosts.${cfg.domain} = {
          root = pkgs.sirimbo-php;
          serverAliases = ["www.${cfg.domain}"];

          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

          locations."/old" = {
            index = "index.php";
            extraConfig = "try_files /undefined /index.php?$args;";
          };

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
            proxyPass = "http://127.0.0.1:${toString cfg.frontendPort}";
            proxyWebsockets = true;
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
        phpPackage = pkgs.php.withExtensions ({ all, enabled }: with all; [
          curl imagick opcache pdo openssl posix json
          mbstring session ctype exif gd zlib pdo_pgsql
        ] ++ enabled);

        phpEnv = lib.filterAttrs (n: v: v != "\"\"" && v != "") {
          DOMAIN = "${cfg.domain}";
          DATABASE_URL = "\"pgsql:${cfg.dbConnString}\"";
          STATE_DIR = "${cfg.stateDir}";
          SMTP_AUTH = if cfg.smtpAuth then "1" else "0";
          SMTP_TLS = if cfg.smtpTLS then "1" else "0";
          SMTP_HOST = "${cfg.smtpHost}";
          SMTP_PORT = "${toString cfg.smtpPort}";
          SMTP_USER = "${cfg.smtpUser}";
          SMTP_PASS = "${cfg.smtpPass}";
        };
      };

      systemd.services.sirimbo-frontend = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          PORT = toString cfg.frontendPort;
          GRAPHQL_BACKEND = "http://localhost:${toString cfg.jsPort}";
          NEXT_PUBLIC_SENTRY_ENVIRONMENT = cfg.domain;
          NEXT_PUBLIC_BASE_URL = "http${if cfg.ssl then "s" else ""}://${cfg.domain}";
        };

        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.nodejs}/bin/node ${pkgs.sirimbo-frontend}/server.js";
          Restart = "always";
          RestartSec = "10s";
        };
      };

      systemd.services.sirimbo-backend = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          PGDATABASE = "olymp";
          PGHOST = "/run/postgresql";
          PORT = toString cfg.jsPort;
          DOMAIN = cfg.domain;
          SSL = if cfg.ssl then "1" else "";
          STATE_DIR = cfg.stateDir;
          SMTP_AUTH = if cfg.smtpAuth then "1" else "";
          SMTP_TLS = if cfg.smtpTLS then "1" else "";
          SMTP_HOST = cfg.smtpHost;
          SMTP_PORT = toString cfg.smtpPort;
          SMTP_USER = cfg.smtpUser;
          SMTP_PASS = cfg.smtpPass;
        };

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
  ];
}
