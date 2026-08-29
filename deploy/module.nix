{
  config,
  lib,
  ...
}:
let
  cfg = config.services.olymp;
in
{
  options.services.olymp.frontend = {
    enable = lib.mkEnableOption "Rozpisovnik frontend via ArgoCD";

    ssl = lib.mkEnableOption "Rozpisovnik frontend enable SSL";

    domains = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "Rozpisovnik frontend domain";
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
        description = "Rozpisovnik frontend staging domains";
      };
    };
  };

  config = lib.mkIf cfg.frontend.enable {
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

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.frontend.staging.nodePort}";
          proxyWebsockets = true;
        };
      });
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
                name = "runtime.graphqlBackend";
                value = "https://api.rozpisovnik.cz";
              }
              {
                name = "runtime.jwtSecret";
                value = cfg.backend.jwtSecret;
              }
              {
                name = "runtime.sentryDsn";
                value = "https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825";
              }
              {
                name = "runtime.sentryEnvironment";
                value = "production";
              }
            ];
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
                name = "runtime.graphqlBackend";
                value = "https://api.rozpisovnik.cz";
              }
              {
                name = "runtime.jwtSecret";
                value = cfg.backend.jwtSecret;
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
