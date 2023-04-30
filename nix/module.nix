{ config, lib, pkgs, ... }: let
  cfg = config.services.olymp-beta;
  pkgName = "tkolymp.cz";
in {
  options.services.olymp-beta = {
    enable = lib.mkEnableOption "${pkgName}";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} Nginx vhost domain";
      example = "tkolymp.cz";
    };
    domainAliases = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "${pkgName} Nginx vhost domain aliases";
      default = [];
      example = "[www.tkolymp.cz]";
    };

    ssl = lib.mkEnableOption "${pkgName} enable ssl";
    debug = lib.mkEnableOption "${pkgName} enable debug mode";

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
          enableACME = cfg.ssl;
          forceSSL = cfg.ssl;
          serverAliases = cfg.domainAliases;

          extraConfig = ''
            # To allow special characters in headers
            ignore_invalid_headers off;
            # Allow any size file to be uploaded.
            client_max_body_size 0;
            # To disable buffering
            proxy_buffering off;
          '';

          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

          locations."/member/download" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
          };
          locations."/graphql" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
            proxyWebsockets = true;
          };
          locations."/graphiql" = {
            proxyPass = "http://127.0.0.1:${toString cfg.jsPort}";
            proxyWebsockets = true;
          };
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.frontendPort}";
            proxyWebsockets = true;
          };
        };
      };

      services.minio = {
        enable = true;
        browser = false;
        listenAddress = ":${toString cfg.minioPort}";
        configDir = lib.mkForce "${cfg.stateDir}/minio-config";
        dataDir = lib.mkForce ["${cfg.stateDir}/minio-data"];
        accessKey = cfg.minioAccessKey;
        secretKey = cfg.minioSecretKey;
      };
      networking.firewall.allowedTCPPorts = [cfg.minioPort];

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
          mc --config-dir . mb --ignore-existing minio/private
          mc --config-dir . mb --ignore-existing minio/public
          mc --config-dir . policy set download minio/public
        '';
      };

      systemd.services.sirimbo-frontend-beta = {
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          PGDATABASE = "olymp";
          PGHOST = "/run/postgresql";
          PORT = toString cfg.frontendPort;
          GRAPHQL_BACKEND = "http://localhost:${toString cfg.jsPort}";
          NEXT_PUBLIC_SENTRY_ENVIRONMENT = cfg.domain;
          NEXT_PUBLIC_BASE_URL = "http${if cfg.ssl then "s" else ""}://${cfg.domain}";
        };

        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.nodejs}/bin/node ${pkgs.sirimbo-frontend-beta}/server.js";
          WorkingDirectory = cfg.stateDir;
          Restart = "always";
          RestartSec = "10s";
        };
      };

      systemd.services.sirimbo-backend-beta = {
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
          MINIO_DOMAIN = cfg.domain;
          MINIO_PORT = toString cfg.minioPort;
          MINIO_ACCESS_KEY = cfg.minioAccessKey;
          MINIO_SECRET_KEY = cfg.minioSecretKey;
          DEBUG = if cfg.debug then "postgraphile:postgres,postgraphile:postgres:error" else "";
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

      systemd.services.sirimbo-migrate-beta = {
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
          WorkingDirectory = pkgs.sirimbo-migrations-beta;
          ExecStart = "${pkgs.graphile-migrate}/bin/graphile-migrate migrate";
        };
      };
    })
  ];
}
