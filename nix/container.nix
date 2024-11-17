{ pkgs, ... }: {
  boot.isContainer = true;
  system.stateVersion = "23.05";
  networking.useDHCP = false;
  networking.firewall.allowedTCPPorts = [ 80 3000 3306 5432 8025 1025 9000 ];

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    package = pkgs.postgresql_16;
    extraPlugins = with pkgs.postgresql_16.pkgs; [
      plpgsql_check
      pg_cron
      hypopg
      pgsql-http
    ];
    ensureDatabases = [ "olymp" "olymp_shadow" ];
    ensureUsers = [{
      name = "olymp";
    }];
    authentication = "host all all all trust";
    settings = {
      shared_preload_libraries = "pg_stat_statements,pg_cron";
      "pg_stat_statements.track" = "all";
      "cron.database_name" = "olymp";
      "cron.use_background_workers" = "on";
      max_worker_processes = "20";
    };
  };

  services.mailhog.enable = true;
  services.olymp = {
    stateDir = "/var/lib/olymp";

    migrations.enable = true;
    worker.enable = true;

    backend = {
      enable = true;
      domain = "olymp-test";
      debug = true;
      ssl = false;
      port = 5000;
      database = "olymp";
      jwtSecret = "1111111111";
    };
    smtp = {
      auth = false;
      tls = false;
      host = "127.0.0.1";
      port = 1025;
    };
    s3 = {
      bucket = "public";
      region = "us-west-1";
      endpoint = "http://olymp-test:9000";
      accessKeyId = "00000000";
      secretAccessKey = "000000000000";
    };
  };

  services.minio = {
    enable = true;
    browser = false;
    listenAddress = ":9000";
    configDir = "/var/lib/minio/config";
    dataDir = [ "/var/lib/minio/data" ];
    accessKey = "00000000";
    secretKey = "000000000000";
  };
  systemd.tmpfiles.rules = [
    "d /var/lib/minio 0755 minio minio -"
    "d /var/lib/minio/config 0755 minio minio -"
    "d /var/lib/minio/data 0755 minio minio -"
  ];

  systemd.services.minio = {
    serviceConfig = {
      ExecStartPost = ''
        ${pkgs.coreutils}/bin/timeout 30 ${pkgs.bash}/bin/bash -c \
          'while ! ${pkgs.curl}/bin/curl --silent --fail http://localhost:9000/minio/health/cluster; do sleep 1; done'
      '';
    };
  };

  systemd.services.minio-config = {
    path = [ pkgs.minio pkgs.minio-client ];
    requiredBy = [ "multi-user.target" ];
    after = [ "minio.service" ];
    serviceConfig = {
      Type = "simple";
      User = "minio";
      Group = "minio";
      WorkingDirectory = "/var/lib/minio/config";
    };
    script = ''
      set -e
      mc --config-dir . config host add minio http://localhost:9000 "00000000" "000000000000"
      mc --config-dir . mb --ignore-existing minio/private
      mc --config-dir . mb --ignore-existing minio/public
      mc --config-dir . policy set download minio/public
    '';
  };
}
