{ yarn2nix-moretea
, nix-gitignore
, ncc
}:

yarn2nix-moretea.mkYarnPackage {
  src = nix-gitignore.gitignoreSourcePure [../.gitignore] ./.;
  packageJSON = ./package.json;
  yarnLock = ./yarn.lock;
  installPhase = ''
    # Inline watch-fixtures.sql
    sed -i \
      -e '/readFile(WATCH_FIXTURES_PATH,/d' \
      -e 's|const WATCH_FIXTURES_PATH.*|var watchSqlInner = require("!!raw-loader!../../res/watch-fixtures.sql").default;|' \
      node_modules/graphile-build-pg/node8plus/plugins/PgIntrospectionPlugin.js
    ${ncc}/bin/ncc build ./deps/sirimbo-backend/src/index.ts
    mkdir -p $out/bin
    cp dist/index.js $out/bin/sirimbo-backend
  '';
  distPhase = "true";
}
