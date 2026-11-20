{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.olymp;
  pkgName = "Sirimbo";
in
{
  options.services.olymp = {
    enable = lib.mkEnableOption "${pkgName} via ArgoCD";

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

    jwtSecret = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} JWT secret";
    };
    database = lib.mkOption {
      type = lib.types.str;
      description = "${pkgName} Postgres DB";
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

    frontend = {
      ssl = lib.mkEnableOption "${pkgName} enable SSL";

      domains = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "${pkgName} domains";
      };
      nodePort = lib.mkOption {
        type = lib.types.port;
        default = 30100;
        description = "NodePort exposed by the frontend Kubernetes service";
      };
      staging = {
        nodePort = lib.mkOption {
          type = lib.types.port;
          default = 30101;
          description = "NodePort exposed by the frontend staging Kubernetes service";
        };
        domains = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "${pkgName} staging domains";
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.nginx = {
      enable = true;
      enableReload = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;

      virtualHosts = lib.genAttrs cfg.frontend.domains (domain: {
        enableACME = cfg.frontend.ssl;
        forceSSL = cfg.frontend.ssl;

        extraConfig = ''
          ignore_invalid_headers off;
          client_max_body_size 0;
          proxy_buffering off;
        '';

        locations."/gallery".root = cfg.stateDir;
        locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.frontend.nodePort}";
          proxyWebsockets = true;
        };
      }) // lib.genAttrs cfg.frontend.staging.domains (domain: {
        enableACME = cfg.frontend.ssl;
        forceSSL = cfg.frontend.ssl;

        extraConfig = ''
          ignore_invalid_headers off;
          client_max_body_size 0;
          proxy_buffering off;
        '';

        locations."/gallery".root = cfg.stateDir;
        locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.frontend.staging.nodePort}";
          proxyWebsockets = true;
        };
      });
    };

    users.users.${cfg.user} = {
      name = cfg.user;
      group = cfg.group;
      home = cfg.stateDir;
      description = pkgName;
      createHome = true;
      useDefaultShell = true;
      isSystemUser = true;
    };
    users.groups.${cfg.user}.name = cfg.group;
    systemd.tmpfiles.rules = [
      "d ${cfg.stateDir} 0755 ${cfg.user} ${cfg.user} -"
      "d ${cfg.stateDir}/gallery 0755 ${cfg.user} ${cfg.user} -"
      "d ${cfg.stateDir}/gallery/thumbnails 0755 ${cfg.user} ${cfg.user} -"
    ];

    systemd.services.rozpisovnik-migrate = {
      description = "${pkgName} Migrations";
      after = [ "network-online.target" "postgresql.service" ];
      requires = [ "network-online.target" "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];

      environment.DATABASE_URL = "postgres://${cfg.user}@localhost/${cfg.database}";
      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        Type = "oneshot";
        RemainAfterExit = "true";
        WorkingDirectory = pkgs.rozpisovnik-migrations;
        ExecStart = "${pkgs.graphile-migrate}/bin/graphile-migrate migrate";
      };
    };

    my.seaweedfs.buckets.olymp.ensure = true;
    my.argocdApps = [
      {
        apiVersion = "argoproj.io/v1alpha1";
        kind = "Application";
        metadata = {
          name = "sirimbo";
          namespace = "argocd";
        };
        spec = {
          project = "default";
          source = {
            repoURL = "https://github.com/zarybnicky/Sirimbo";
            targetRevision = "master";
            path = "deploy/chart";
            helm.parameters = [
              {
                name = "image.repository";
                value = "127.0.0.1:5000/sirimbo";
              }
              {
                name = "image.tag";
                value = "$ARGOCD_APP_REVISION";
              }
              {
                name = "service.nodePort";
                value = toString cfg.frontend.nodePort;
              }
              {
                name = "postgresql.user";
                value = cfg.user;
              }
              {
                name = "postgresql.database";
                value = cfg.database;
              }
              {
                name = "runtime.jwtSecret";
                value = cfg.jwtSecret;
              }
              {
                name = "runtime.sentryDsn";
                value = "https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825";
              }
              {
                name = "runtime.sentryEnvironment";
                value = "production";
              }
              {
                name = "worker.enabled";
                value = "true";
              }
              {
                name = "smtp.auth";
                value = lib.boolToString cfg.smtp.auth;
              }
              {
                name = "smtp.tls";
                value = lib.boolToString cfg.smtp.tls;
              }
              {
                name = "smtp.host";
                value = if builtins.elem cfg.smtp.host [ "localhost" "127.0.0.1" ]
                  then "$(PGHOST)" else cfg.smtp.host;
              }
              {
                name = "smtp.port";
                value = toString cfg.smtp.port;
              }
              {
                name = "smtp.user";
                value = cfg.smtp.user;
              }
              {
                name = "smtp.pass";
                value = cfg.smtp.pass;
                forceString = true;
              }
              {
                name = "runtime.wdsfAuth";
                value = cfg.wdsfAuth;
              }
              {
                name = "otel.endpoint";
                value = lib.replaceStrings
                  [ "://localhost:" "://127.0.0.1:" ]
                  [ "://$(PGHOST):" "://$(PGHOST):" ] cfg.otel.endpoint;
              }
              {
                name = "otel.headers";
                value = "authorization=${cfg.otel.apiKey}";
              }
            ] ++ config.my.seaweedfs.buckets.olymp.helmValues;
          };
          destination = {
            server = "https://kubernetes.default.svc";
            namespace = "sirimbo";
          };
          syncPolicy.syncOptions = [
            "CreateNamespace=true"
          ];
        };
      }
      {
        apiVersion = "argoproj.io/v1alpha1";
        kind = "Application";
        metadata = {
          name = "sirimbo-staging";
          namespace = "argocd";
        };
        spec = {
          project = "default";
          source = {
            repoURL = "https://github.com/zarybnicky/Sirimbo";
            targetRevision = "staging";
            path = "deploy/chart";
            helm.parameters = [
              {
                name = "image.repository";
                value = "127.0.0.1:5000/sirimbo";
              }
              {
                name = "image.tag";
                value = "$ARGOCD_APP_REVISION";
              }
              {
                name = "service.nodePort";
                value = toString cfg.frontend.staging.nodePort;
              }
              {
                name = "postgresql.user";
                value = cfg.user;
              }
              {
                name = "postgresql.database";
                value = cfg.database;
              }
              {
                name = "runtime.jwtSecret";
                value = cfg.jwtSecret;
              }
              {
                name = "runtime.sentryDsn";
                value = "https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825";
              }
              {
                name = "runtime.sentryEnvironment";
                value = "staging";
              }
            ] ++ config.my.seaweedfs.buckets.olymp.helmValues;
          };
          destination = {
            server = "https://kubernetes.default.svc";
            namespace = "sirimbo-staging";
          };
          syncPolicy.syncOptions = [
            "CreateNamespace=true"
          ];
        };
      }
    ];
  };
}
