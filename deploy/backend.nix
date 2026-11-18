{ config, lib, pkgs, ... }: let
  cfg = config.services.olymp;
  pkgName = "tkolymp.cz";
in {
  imports = [
    ./module.nix
  ];

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
    (lib.mkIf (cfg.backend.enable || cfg.frontend.enable || cfg.migrations.enable) {
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
      ];

    })

    (lib.mkIf cfg.backend.enable {
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
            proxyPass = "http://127.0.0.1:${toString cfg.frontend.nodePort}";
            proxyWebsockets = true;
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
  ];
}
