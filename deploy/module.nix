{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.rozpisovnik-web;
in
{
  options.services.rozpisovnik-web = {
    enable = lib.mkEnableOption "Rozpisovnik frontend via ArgoCD";

    domain = lib.mkOption {
      type = lib.types.str;
      description = "Rozpisovnik frontend domain";
    };

    ssl = lib.mkEnableOption "Rozpisovnik frontend enable SSL";

    nodePort = lib.mkOption {
      type = lib.types.port;
      default = 30080;
      description = "NodePort exposed by the frontend Kubernetes service";
    };

    imageRepository = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1:5000/rozpisovnik/web";
      description = "Frontend image repository used by the Helm chart";
    };

    graphqlBackend = lib.mkOption {
      type = lib.types.str;
      default = "https://api.rozpisovnik.cz";
      description = "Server-side GraphQL backend URL used by the frontend container";
    };

    sentryDsn = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Sentry DSN exposed to the frontend container runtime";
    };

    sentryEnvironment = lib.mkOption {
      type = lib.types.str;
      default = "production";
      description = "Sentry environment exposed to the frontend container runtime";
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

      virtualHosts.${cfg.domain} = {
        enableACME = cfg.ssl;
        forceSSL = cfg.ssl;

        extraConfig = ''
          ignore_invalid_headers off;
          client_max_body_size 0;
          proxy_buffering off;
        '';

        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString cfg.nodePort}";
          proxyWebsockets = true;
        };
      };
    };

    my.argocdApps = [
      {
        apiVersion = "argoproj.io/v1alpha1";
        kind = "Application";
        metadata = {
          name = "rozpisovnik-web";
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
                value = cfg.imageRepository;
              }
              {
                name = "image.tag";
                value = "$ARGOCD_APP_REVISION";
              }
              {
                name = "service.nodePort";
                value = toString cfg.nodePort;
              }
              {
                name = "runtime.graphqlBackend";
                value = cfg.graphqlBackend;
              }
              {
                name = "runtime.sentryDsn";
                value = cfg.sentryDsn;
              }
              {
                name = "runtime.sentryEnvironment";
                value = cfg.sentryEnvironment;
              }
            ];
          };
          destination = {
            server = "https://kubernetes.default.svc";
            namespace = "rozpisovnik-web";
          };
          syncPolicy.syncOptions = [
            "CreateNamespace=true"
          ];
        };
      }
    ];

    services.github-runners."rozpisovnik-web" = {
      enable = true;
      name = "${config.networking.hostName}-rozpisovnik-web";
      url = "https://github.com/zarybnicky/Sirimbo";
      replace = true;
      extraLabels = ["nixos" "deploy" "rozpisovnik"];
      extraPackages = with pkgs; [
        argocd
        git
        jq
        kubectl
        skopeo
      ];
      extraEnvironment.KUBECONFIG = "/etc/rancher/k3s/argocd-ns.yaml:/etc/rancher/k3s/k3s.yaml";
    };
  };
}
