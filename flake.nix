{
  inputs.gogol = { flake = false; url = github:brendanhay/gogol/develop; };
  inputs.unstable.url = github:NixOS/nixpkgs/master;

  outputs = { self, nixpkgs, unstable, gogol }: let
    inherit (nixpkgs.lib) flip mapAttrs mapAttrsToList;
    inherit (pkgs.nix-gitignore) gitignoreSourcePure gitignoreSource;

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    unstablePkgs = import unstable {
      system = "x86_64-linux";
      overlays = [ self.overlay ];
    };
    compiler = "ghc8104";
    hsPkgs = pkgs.haskell.packages.${compiler};
    getSrc = dir: gitignoreSourcePure [./.gitignore] dir;
  in {
    overlay = final: prev: let
      inherit (prev.haskell.lib) doJailbreak dontCheck justStaticExecutables
        generateOptparseApplicativeCompletion unmarkBroken;
    in {
      hasura-graphql-engine = prev.hasura-graphql-engine.overrideAttrs (oldAttrs: {
        VERSION = prev.hasura-graphql-engine.version;
      });

      phpstan = final.stdenv.mkDerivation {
        pname = "phpstan";
        version = "0.12.67";
        src = final.fetchurl {
          url = "https://github.com/phpstan/phpstan/releases/download/0.12.67/phpstan.phar";
          sha256 = "/c+ci/Ok08Qr4bFRiE8e0wSPNo3/k7LbW/KJOkcUTDw=";
        };
        phases = [ "installPhase" ];
        nativeBuildInputs = [ final.makeWrapper ];
        installPhase = ''
          mkdir -p $out/bin
          install -D $src $out/libexec/phpstan/phpstan.phar
          makeWrapper ${final.php74}/bin/php $out/bin/phpstan \
          --add-flags "$out/libexec/phpstan/phpstan.phar"
        '';
      };

      mysql_fdw = final.stdenv.mkDerivation rec {
        name = "mysql_fdw-${version}";
        version = "2.4";
        buildInputs = [ final.postgresql final.libmysqlclient ];
        src = final.fetchFromGitHub {
          owner  = "EnterpriseDB";
          repo   = "mysql_fdw";
          rev = "c4677792b86e2944833282aa6e0463350a2638f8" ;
          sha256 = "R+mCnFjNgWRMWQ5Qqg4alkBThVtqmKLcVhxHk7vm2T4=";
        };
        buildPhase = ''
          sed -i 's,^PG_CPPFLAGS +=.*,PG_CPPFLAGS += -D _MYSQL_LIBNAME=\\"${final.libmysqlclient}/lib/mysql/libmysqlclient$(DLSUFFIX)\\",' Makefile
          make USE_PGXS=1
        '';
        installPhase = ''
          mkdir -p $out/{lib,share/postgresql/extension}
          ls
          cp *.so      $out/lib
          cp *.sql     $out/share/postgresql/extension
          cp *.control $out/share/postgresql/extension
        '';
      };

      haskell = prev.haskell // {
        packageOverrides = prev.lib.composeExtensions (prev.haskell.packageOverrides or (_: _: {})) (hself: hsuper: {
          servant-JuicyPixels = doJailbreak (unmarkBroken hsuper.servant-JuicyPixels);
          higgledy = doJailbreak (unmarkBroken hsuper.higgledy);
          gogol-core = hself.callCabal2nix "gogol-core" "${gogol}/core" {};
          gogol = hself.callCabal2nix "gogol" "${gogol}/gogol" {};
          gogol-youtube = hself.callCabal2nix "gogol-youtube" "${gogol}/gogol-youtube" {};

          sirimbo-schema = hself.callCabal2nix "sirimbo-schema" (getSrc ./sirimbo-schema) {};
          sirimbo-api = generateOptparseApplicativeCompletion "olymp" (
            justStaticExecutables (
              hself.callCabal2nix "sirimbo-api" (getSrc ./sirimbo-api) {}
            )
          );
        });
      };
      inherit (final.haskell.packages.${compiler}) sirimbo-api;
      sirimbo-tournament-frontend = final.stdenv.mkDerivation {
        name = "sirimbo-tournament-frontend";
        src = getSrc ./sirimbo-tournament-frontend;
        phases = "unpackPhase buildPhase";
        buildPhase = ''
          ${final.nodePackages.typescript}/bin/tsc
          ${final.sass}/bin/sass index.scss:index.css
          mkdir -p $out
          cp admin.html almond.js bundle.js{,.map} index.css{,.map} index.html react* $out/
        '';
      };
      sirimbo-php = (final.callPackage ./sirimbo-php/composer-project.nix {
        php = final.php74;
      } (getSrc ./sirimbo-php)).overrideAttrs (oldAttrs: {
        name = "sirimbo-php";
        buildInputs = oldAttrs.buildInputs ++ [ final.imagemagick ];
        buildPhase = "composer validate";
        installPhase = ''
          runHook preInstall
          mkdir -p $out
          mv $PWD/* $out/
          runHook postInstall
        '';
        doCheck = true;
        checkPhase = ''
          ! ${pkgs.ag}/bin/ag '\\[A-Z]|<\?|\$[^(]' files/Templates
          ${pkgs.php}/bin/php -f vendor/bin/twig-linter -- lint files/Templates
          ${pkgs.phpstan}/bin/phpstan analyse --level 5 files/
        '';
      });
      sirimbo-app = final.yarn2nix-moretea.mkYarnPackage {
        name = "sirimbo-app";
        src = getSrc ./sirimbo-app;
        packageJSON = ./sirimbo-app/package.json;
        yarnLock = ./sirimbo-app/yarn.lock;
        # yarnNix = ./sirimbo-php-scripts/yarn.nix;
        # doCheck = true;
        # checkPhase = "yarn test --coverage --ci";
        buildPhase = ''
          yarn --offline run build
          sed -i '1,3d' deps/Sirimbo/dist/main.css
        '';
        distPhase = "true";
        installPhase = ''
          mkdir -p $out/public
          cp -Lr deps/Sirimbo/dist/* $out/public/
        '';

        extraBuildInputs = [ final.libsass ];
        yarnPreBuild = "export npm_config_nodedir=${final.nodejs}";
        pkgConfig = {
          node-sass = {
            nativeBuildInputs = [ ];
            buildInputs = [ final.libsass final.pkg-config final.python3 ];
            postInstall = ''
              LIBSASS_EXT=auto yarn --offline run build
              rm build/config.gypi
            '';
          };
        };
      };
    };

    packages.x86_64-linux = {
      inherit (pkgs) sirimbo-tournament-frontend sirimbo-php sirimbo-app mysql_fdw;
      inherit (hsPkgs) sirimbo-api sirimbo-schema;
    };

    devShell.x86_64-linux = hsPkgs.shellFor {
      withHoogle = true;
      packages = p: [ p.sirimbo-api p.sirimbo-schema ];
      buildInputs = [
        hsPkgs.cabal-install
        hsPkgs.haskell-language-server
        hsPkgs.stan
        pkgs.yarn
        pkgs.phpstan
        pkgs.nodePackages.typescript
        pkgs.sass
        pkgs.yarn2nix
        unstablePkgs.hasura-cli
        unstablePkgs.hasura-graphql-engine
      ];
    };

    nixosConfigurations.container = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        self.nixosModule
        { nixpkgs.overlays = [ self.overlay ]; }
        ({ pkgs, ... }: {
          boot.isContainer = true;
          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
          networking.useDHCP = false;
          networking.firewall.allowedTCPPorts = [ 80 3000 3306 ];
          services.mysql = {
            enable = true;
            package = pkgs.mariadb;
            ensureDatabases = ["olymp"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = { "olymp.*" = "ALL PRIVILEGES"; };
            }];
          };
          services.postgresql = {
            enable = true;
            extraPlugins = [pkgs.mysql_fdw];
            ensureDatabases = ["root" "olymp"];
            ensureUsers = [{
              name = "olymp";
              ensurePermissions = {
                "DATABASE olymp" = "ALL PRIVILEGES";
                "ALL TABLES IN SCHEMA public" = "ALL";
              };
            }];
          };
          services.olymp = {
            enable = true;
            dbConnString = "dbname=olymp";
            stateDir = "/var/lib/olymp";
            domain = "olymp-test";
            internalPort = 3000;
            proxyPort = 3010;
          };
        })
      ];
    };

    nixosModule = { config, lib, pkgs, ... }: let
      cfg = config.services.olymp;
      pkgName = "tkolymp.cz";
    in {
      options.services.olymp = {
        enable = lib.mkEnableOption "${pkgName}";
        yt-worker.enable = lib.mkEnableOption "${pkgName}";

        domain = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} Nginx vhost domain";
          example = "tkolymp.cz";
        };
        internalPort = lib.mkOption {
          type = lib.types.int;
          description = "${pkgName} internal port";
          example = 3000;
        };
        proxyPort = lib.mkOption {
          type = lib.types.int;
          description = "${pkgName} internal PHP port";
          example = 3001;
        };

        dbConnString = lib.mkOption {
          type = lib.types.str;
          description = "${pkgName} DB connection string";
        };

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
      };

      config = let
        cfgFile = pkgs.writeText "config.yaml" ''
          dbConnString: "${cfg.dbConnString}"
          proxyPort: ${toString cfg.proxyPort}
        '';
        configPhp = pkgs.runCommand "sirimbo-php-config" {} ''
          mkdir -p $out
          cat > $out/config.php <<EOS
          <?php
          openlog('${cfg.domain}', LOG_ODELAY, LOG_USER);
          date_default_timezone_set('Europe/Paris');
          mb_internal_encoding('UTF-8');

          define('SENTRY_ENV', '${cfg.domain}');
          define('DB_CONN_STRING', 'pgsql:${cfg.dbConnString}');

          define('GALERIE', '${cfg.stateDir}/gallery');
          define('GALERIE_THUMBS', '${cfg.stateDir}/gallery/thumbnails');
          define('UPLOADS', '${cfg.stateDir}/uploads');
          define('CACHE', '${cfg.stateDir}/cache');
          foreach ([GALERIE, GALERIE_THUMBS, UPLOADS, CACHE] as \$path) {
            if (!is_readable(\$path)) {
              mkdir(\$path, 0777, true);
            }
          }
          define('NABOR', '0');
          define('DEFAULT_FROM_MAIL', 'TK Olymp.cz <noreply@tkolymp.cz>');
          define('DEFAULT_ADMIN_MAIL', 'tkolymp@tkolymp.cz');
          EOS
        '';
        phpRoot = pkgs.symlinkJoin {
          name = "sirimbo-php-dist";
          paths = [
            pkgs.sirimbo-php
            pkgs.sirimbo-app
            configPhp
          ];
        };
      in lib.mkMerge [
        (lib.mkIf cfg.enable {
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
          systemd.tmpfiles.rules = [ "d ${cfg.stateDir} 0755 ${cfg.user} ${cfg.user} -" ];

          environment.systemPackages = [
            (pkgs.runCommand "olymp" { buildInputs = [ pkgs.makeWrapper ]; } ''
              mkdir $out
              ln -s ${pkgs.sirimbo-api}/* $out
              rm $out/bin
              mkdir $out/bin
              makeWrapper ${pkgs.sirimbo-api}/bin/olymp $out/bin/olymp --set CONFIG "${cfgFile}"
            '')
          ];

          services.nginx = {
            enable = true;
            enableReload = true;
            recommendedTlsSettings = true;
            recommendedGzipSettings = true;
            recommendedOptimisation = true;
            recommendedProxySettings = true;
            virtualHosts.${"fpm." + cfg.domain} = {
              root = phpRoot;
              listen = [{ addr = "127.0.0.1"; port = cfg.proxyPort; }];
              locations."/" = {
                index = "index.php";
                extraConfig = "try_files /public/$uri /index.php?$args;";
              };
              locations."~ \.php$".extraConfig = ''
                try_files $uri /index.php?$args;
                client_max_body_size 20M;
                fastcgi_split_path_info ^(.+\.php)(/.+)$;
                fastcgi_pass unix:${config.services.phpfpm.pools.${cfg.domain}.socket};
                fastcgi_index index.php;
                include ${pkgs.nginx}/conf/fastcgi_params;
                include ${pkgs.nginx}/conf/fastcgi.conf;
              '';
            };
            virtualHosts.${cfg.domain} = {
              root = phpRoot;
              serverAliases = ["www.${cfg.domain}"];
              locations."/gallery".root = cfg.stateDir;
              locations."/galerie".root = cfg.stateDir;
              locations."/galerie".extraConfig = "rewrite ^/galerie(/.*)$ /gallery/$1 last;";
              locations."/".extraConfig = "try_files /public/$uri @proxy;";

              locations."/graphql/" = {
                proxyPass = "http://127.0.0.1:8080/";
                proxyWebsockets = true;
              };
              locations."@proxy" = {
                proxyPass = "http://127.0.0.1:${toString cfg.internalPort}";
                proxyWebsockets = true;
              };
            };
          };

          services.phpfpm.pools.${cfg.domain} = {
            user = cfg.user;
            settings = {
              "listen.owner" = config.services.nginx.user;
              "pm" = "dynamic";
              "pm.max_children" = 50;
              "pm.max_requests" = 500;
              "pm.start_servers" = 3;
              "pm.min_spare_servers" = 2;
              "pm.max_spare_servers" = 4;
              "catch_workers_output" = true;
              "php_admin_flag[log_errors]" = true;
              "php_admin_value[memory_limit]" = "512M";
              "php_admin_value[upload_max_filesize]" = "40M";
              "php_admin_value[post_max_size]" = "40M";
            };
            phpPackage = pkgs.php.withExtensions ({ all, ... }: with all; [
              curl imagick opcache pdo_mysql pdo mysqlnd mysqli openssl posix
              mbstring session json ctype exif gd zlib pdo_pgsql
            ]);
          };

          systemd.services.hasura = {
            description = "Hasura";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" "postgresql.service" ];
            requires = [ "postgresql.service" ];
            path = [ pkgs.postgresql ];
            environment.HASURA_GRAPHQL_DATABASE_URL = "postgres:///olymp";
            environment.HASURA_GRAPHQL_ADMIN_SECRET = "superadmin";
            environment.HASURA_GRAPHQL_ENABLE_TELEMETRY = "false";
            environment.HASURA_GRAPHQL_AUTH_HOOK = "http://localhost:${toString cfg.internalPort}/api/graphql-auth";
            environment.HASURA_GRAPHQL_ENABLED_LOG_TYPES = "startup, http-log, webhook-log, websocket-log, query-log";
            environment.HASURA_GRAPHQL_EXPERIMENTAL_FEATURES = "inherited_roles";
            environment.SERVER_VERSION = "${unstablePkgs.hasura-graphql-engine.version}";
            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              Type = "simple";
              ExecStart = "${unstablePkgs.hasura-graphql-engine}/bin/graphql-engine serve";
            };
          };

          systemd.services.olymp-api-migrate = {
            description = "${pkgName} Migrations";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" "mysql.service" ];
            requires = [ "mysql.service" ];
            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              Type = "oneshot";
              ExecStart = "${pkgs.coreutils}/bin/true";
              RemainAfterExit = "true";
              # ExecStart = "${self.packages.x86_64-linux.sirimbo-api}/bin/olymp migrate --execute";
            };
          };

          systemd.services.olymp-api = {
            description = "${pkgName} Webserver";
            wantedBy = [ "multi-user.target" ];
            after = [ "network-online.target" "mysql.service" ];
            requires = [ "olymp-api-migrate.service" "mysql.service" ];
            environment.CONFIG = cfgFile;
            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              Restart = "always";
              ExecStart = "${pkgs.sirimbo-api}/bin/olymp server --port ${toString cfg.internalPort}";
            };
          };
        })

        (lib.mkIf cfg.yt-worker.enable {
          systemd.services.olymp-yt-worker = {
            description = "Olymp YouTube worker service";
            environment.CONFIG = cfgFile;
            serviceConfig = {
              Type = "simple";
              ExecStart = "${pkgs.sirimbo-api}/bin/olymp check-youtube";
            };
          };
          systemd.timers.olymp-yt-worker = {
            description = "Olymp YouTube worker service timer";
            wantedBy = ["multi-user.service"];
            timerConfig = {
              Unit = "olymp-yt-worker.service";
              OnBootSec = "10min";
              OnUnitActiveSec = "70min";
            };
          };
        })

      ];
    };
  };
}
