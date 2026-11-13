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
      pgtap
      pg_repack
      pg_stat_monitor
    ];
    ensureDatabases = [ "olymp" "olymp_shadow" ];
    ensureUsers = [{
      name = "olymp";
    }];
    authentication = "host all all all trust";
    settings = {
      max_connections = 200;
      shared_buffers = "8GB";
      effective_cache_size = "24GB";
      maintenance_work_mem = "2GB";
      random_page_cost = 1.1;
      work_mem = "26214kB";
      min_wal_size = "1GB";
      max_wal_size = "4GB";
      max_worker_processes = 16;
      max_parallel_workers_per_gather = 4;
      max_parallel_workers = 16;
      max_parallel_maintenance_workers = 4;

      shared_preload_libraries = [ "pg_stat_statements" "pg_stat_monitor" ];
      compute_query_id = "on";
      track_functions = "all";
      "pg_stat_statements.max" = "10000";
      "pg_stat_statements.track" = "all";

      "pg_stat_monitor.pgsm_bucket_time" = "300";
      "pg_stat_monitor.pgsm_max_buckets" = "288";
      "pg_stat_monitor.pgsm_query_max_len" = "16384";
      "pg_stat_monitor.pgsm_normalized_query" = "off";
      "pg_stat_monitor.pgsm_track_utility" = "off";
      "pg_stat_monitor.pgsm_track" = "all";
      "pg_stat_monitor.pgsm_track_planning" = "on";
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
