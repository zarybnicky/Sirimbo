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

    minio = {
      enable = lib.mkEnableOption "${pkgName}";
      port = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} internal Minio port";
        default = 9000;
      };
      accessKey = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Minio access key";
      };
      secretKey = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Minio secret key";
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
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.frontend.enable or cfg.backend.enable) {
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
            add_header Access-Control-Allow-Headers 'X-CSRF-Token,X-Requested-With,Accept,Accept-Version,Content-Length,Content-MD5,Content-Type,Date,X-Api-Version' always;
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

    (lib.mkIf cfg.minio.enable {
      services.minio = {
        enable = true;
        browser = false;
        listenAddress = ":${toString cfg.minio.port}";
        configDir = lib.mkForce "${cfg.stateDir}/minio-config";
        dataDir = lib.mkForce ["${cfg.stateDir}/minio-data"];
        accessKey = cfg.minio.accessKey;
        secretKey = cfg.minio.secretKey;
      };
      networking.firewall.allowedTCPPorts = [cfg.minio.port];

      systemd.services.minio = {
        serviceConfig = {
          ExecStartPost= ''
            ${pkgs.coreutils}/bin/timeout 30 ${pkgs.bash}/bin/bash -c \
              'while ! ${pkgs.curl}/bin/curl --silent --fail http://localhost:${toString cfg.minio.port}/minio/health/cluster; do sleep 1; done'
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
            http://localhost:${toString cfg.minio.port} "${cfg.minio.accessKey}" "${cfg.minio.secretKey}"
          mc --config-dir . mb --ignore-existing minio/private
          mc --config-dir . mb --ignore-existing minio/public
          mc --config-dir . policy set download minio/public
        '';
      };
    })

    (lib.mkIf cfg.backend.enable {
      systemd.services.sirimbo-backend-beta = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
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
          MINIO_DOMAIN = cfg.backend.domain;
          MINIO_PORT = toString cfg.minio.port;
          MINIO_ACCESS_KEY = cfg.minio.accessKey;
          MINIO_SECRET_KEY = cfg.minio.secretKey;
          DEBUG = if cfg.backend.debug then "postgraphile:postgres,postgraphile:postgres:error" else "";
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
            add_header Access-Control-Allow-Headers 'X-CSRF-Token,X-Requested-With,Accept,Accept-Version,Content-Length,Content-MD5,Content-Type,Date,X-Api-Version' always;
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
