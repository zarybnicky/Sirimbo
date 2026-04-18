{ config, lib, pkgs, ... }: let
  cfg = config.services.olymp;
  pkgName = "tkolymp.cz";
in {
  options.services.olymp = {
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

    worker = {
      enable = lib.mkEnableOption "${pkgName}";
    };
    migrations = {
      enable = lib.mkEnableOption "${pkgName}";
    };

    backend = {
      enable = lib.mkEnableOption "${pkgName}";
      port = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} GraphQL port";
        example = 3002;
      };
      postgrestPort = lib.mkOption {
        type = lib.types.int;
        description = "${pkgName} Postgrest port";
        example = 3003;
      };

      domain = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Nginx vhost domain";
        example = "tkolymp.cz";
      };
      ssl = lib.mkEnableOption "${pkgName} enable ssl";
      debug = lib.mkEnableOption "${pkgName} enable debug mode";
      jwtSecret = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Postgres DB";
      };
      database = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} Postgres DB";
      };
    };

    wdsfAuth = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} WDSF Auth, usually 'Basic xxx'";
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

    otel = {
      endpoint = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} OTEL_EXPORTER_OTLP_ENDPOINT";
      };
      apiKey = lib.mkOption {
        type = lib.types.str;
        description = "${pkgName} HYPERDX_API_KEY";
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.backend.enable {
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
      ];

      systemd.services.rozpisovnik-api = {
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "network-online.target" "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          DEBUG = if cfg.backend.debug then "postgraphile:postgres,postgraphile:postgres:error" else "";
          NODE_ENV = if cfg.backend.debug then "development" else "production";
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
          JWT_SECRET = cfg.backend.jwtSecret;
          S3_BUCKET = cfg.s3.bucket;
          S3_REGION = cfg.s3.region;
          S3_ENDPOINT = cfg.s3.endpoint;
          S3_PUBLIC_ENDPOINT = if cfg.s3.publicEndpoint != null then cfg.s3.publicEndpoint else cfg.s3.endpoint;

          OTEL_EXPORTER_OTLP_ENDPOINT = cfg.otel.endpoint;
          OTEL_EXPORTER_OTLP_HEADERS = "authorization=${cfg.otel.apiKey}";
          OTEL_SERVICE_NAME = "rozpisovnik-api";
          HDX_NODE_EXPERIMENTAL_EXCEPTION_CAPTURE = "1";
          HDX_NODE_BETA_MODE = "1";
          NODE_OPTIONS = "--require ${pkgs.rozpisovnik-api}/share/backend/node_modules/@hyperdx/node-opentelemetry/build/src/tracing.js";
        };

        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.rozpisovnik-api}/bin/rozpisovnik-api";
          WorkingDirectory = "${pkgs.rozpisovnik-api}/share";
          Restart = "always";
          RestartSec = "10s";
        };
      };

      services.postgrest = {
        enable = true;
        settings = {
          server-port = cfg.backend.postgrestPort;
          db-uri = {
            dbname = cfg.backend.database;
            host = "/run/postgresql";
          };
          db-schema = "federated";
          db-anon-role = "anonymous";
          db-config = false;
          jwt-secret = "";
        };
      };
      systemd.services.postgrest.serviceConfig = {
        DynamicUser = false;
        User = cfg.user;
        Group = cfg.group;
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
          '';

          locations."/gallery".root = cfg.stateDir;
          locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.backend.port}";
            proxyWebsockets = true;
          };

          locations."/api/federated" = {
            proxyPass = "http://127.0.0.1:${toString cfg.backend.postgrestPort}";
            extraConfig = ''
              limit_except GET HEAD {
                deny all;
              }
            '';
          };
        };
      };
    })

    (lib.mkIf cfg.migrations.enable {
      systemd.services.rozpisovnik-migrate = {
        description = "${pkgName} Migrations";
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "network-online.target" "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];

        environment.DATABASE_URL = "postgres://${cfg.user}@localhost/${cfg.backend.database}";
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          Type = "oneshot";
          RemainAfterExit = "true";
          WorkingDirectory = pkgs.rozpisovnik-migrations;
          ExecStart = "${pkgs.graphile-migrate}/bin/graphile-migrate migrate";
        };
      };
    })

    (lib.mkIf cfg.worker.enable {
      systemd.services.rozpisovnik-worker = {
        description = "${pkgName} Worker";
        after = [ "network-online.target" "postgresql.service" ];
        requires = [ "network-online.target" "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];

        environment = {
          DATABASE_URL = "postgres://${cfg.user}@localhost/${cfg.backend.database}";
          SMTP_AUTH = if cfg.smtp.auth then "1" else "";
          SMTP_TLS = if cfg.smtp.tls then "1" else "";
          SMTP_HOST = cfg.smtp.host;
          SMTP_PORT = toString cfg.smtp.port;
          SMTP_USER = cfg.smtp.user;
          SMTP_PASS = cfg.smtp.pass;
          WDSF_AUTH = cfg.wdsfAuth;

          OTEL_EXPORTER_OTLP_ENDPOINT = cfg.otel.endpoint;
          OTEL_EXPORTER_OTLP_HEADERS = "authorization=${cfg.otel.apiKey}";
          OTEL_SERVICE_NAME = "rozpisovnik-worker";
          HDX_NODE_EXPERIMENTAL_EXCEPTION_CAPTURE = "1";
          HDX_NODE_BETA_MODE = "1";
          NODE_OPTIONS = "--require ${pkgs.rozpisovnik-worker}/share/worker/node_modules/@hyperdx/node-opentelemetry/build/src/tracing.js";
        };

        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${pkgs.rozpisovnik-worker}/bin/rozpisovnik-worker -j 5";
          Restart = "always";
          RestartSec = "10s";
        };
      };
    })
  ];
}
