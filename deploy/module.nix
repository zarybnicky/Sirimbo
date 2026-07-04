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

    users.users.github-runner-sirimbo = {
      uid = 1234;
      isSystemUser = true;
      group = "github-runner-sirimbo";
      home = "/var/lib/github-runner-sirimbo";
      createHome = true;
      linger = true;
      subUidRanges = [{ startUid = 100000; count = 65536; }];
      subGidRanges = [{ startGid = 100000; count = 65536; }];
    };
    users.groups.github-runner-sirimbo = {};

    virtualisation.docker.rootless = {
      enable = true;
      setSocketVariable = true;
      daemon.settings = {
        features.containerd-snapshotter = true;
      };
    };

    services.github-runners."sirimbo" = {
      enable = true;
      name = "${config.networking.hostName}-sirimbo";
      url = "https://github.com/zarybnicky/Sirimbo";
      replace = true;
      user = "github-runner-sirimbo";
      group = "github-runner-sirimbo";
      extraLabels = [ "deploy" "sirimbo" ];
      extraPackages = with pkgs; [
        argocd
        git
        jq
        kubectl
        skopeo
        docker
      ];
      serviceOverrides = {
        PrivateUsers = lib.mkForce false;
        BindReadOnlyPaths = [ "/run/user" ];
      };
      extraEnvironment = {
        KUBECONFIG = "/etc/rancher/k3s/argocd-ns.yaml:/etc/rancher/k3s/k3s.yaml";
        DOCKER_HOST = "unix:///run/user/1234/docker.sock";
        XDG_RUNTIME_DIR = "/run/user/1234";
      };
    };
  };
}
