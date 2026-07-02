{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.olymp.frontend;
in
{
  options.services.olymp.frontend = {
    enable = lib.mkEnableOption "Rozpisovnik frontend via ArgoCD";

    domains = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "Rozpisovnik frontend domain";
    };

    ssl = lib.mkEnableOption "Rozpisovnik frontend enable SSL";

    nodePort = lib.mkOption {
      type = lib.types.port;
      default = 30080;
      description = "NodePort exposed by the frontend Kubernetes service";
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

      virtualHosts = lib.genAttrs cfg.domains (domain: {
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
      });
    };

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
                value = toString cfg.nodePort;
              }
              {
                name = "runtime.graphqlBackend";
                value = "https://api.rozpisovnik.cz";
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
    ];

    services.github-runners."sirimbo" = {
      enable = true;
      name = "${config.networking.hostName}-sirimbo";
      url = "https://github.com/zarybnicky/Sirimbo";
      replace = true;
      extraLabels = ["nixos" "deploy" "sirimbo"];
      extraPackages = with pkgs; [
        argocd
        git
        jq
        kubectl
        skopeo
        podman
      ];
      extraEnvironment.KUBECONFIG = "/etc/rancher/k3s/argocd-ns.yaml:/etc/rancher/k3s/k3s.yaml";
    };
  };
}
