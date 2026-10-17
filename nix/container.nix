{ pkgs, ... }: {
  boot.isContainer = true;
  boot.isNspawnContainer = true;
  system.stateVersion = "23.05";
  networking.useDHCP = false;
  networking.firewall.allowedTCPPorts = [ 80 3000 3306 5432 8025 1025 9000 ];

  services.postgresql = {
    enable = true;
    enableTCPIP = true;
    package = pkgs.postgresql_18;
    extensions = with pkgs.postgresql_18.pkgs; [
      plpgsql_check
      hypopg
      pgsql-http
      pgtap
    ];
    ensureDatabases = [ "olymp" "olymp_shadow" ];
    ensureUsers = [{
      name = "olymp";
    }];
    authentication = "host all all all trust";
    settings = {
      shared_preload_libraries = "pg_stat_statements,auto_explain";
      "pg_stat_statements.track" = "all";
      max_worker_processes = "20";
    };
  };

  systemd.services.rozpisovnik-api.environment.POSTGRAPHILE_DONT_WATCH = "1";

  my.seaweedfs.buckets.olymp.environment = {
    S3_BUCKET = "public";
    AWS_REGION = "us-west-1";
    AWS_ENDPOINT_URL_S3 = "http://olymp-test:9000";
    AWS_ACCESS_KEY_ID = "00000000";
    AWS_SECRET_ACCESS_KEY = "000000000000";
    AWS_EC2_METADATA_DISABLED = "true";
  };

  services.mailhog.enable = true;
  services.olymp = {
    stateDir = "/var/lib/olymp";

    migrations.enable = true;
    worker.enable = false;

    backend = {
      enable = true;
      domain = "olymp-test";
      debug = true;
      ssl = false;
      port = 5200;
      database = "olymp";
      jwtSecret = "1111111111";
    };
    smtp = {
      auth = false;
      tls = false;
      host = "127.0.0.1";
      port = 1025;
    };
    otel = {
      endpoint = "http://localhost:4318";
      apiKey = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx";
    };
  };
}
