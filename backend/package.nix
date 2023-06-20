{ yarn2nix-moretea
, nix-gitignore
, ncc
}:

yarn2nix-moretea.mkYarnPackage {
  src = nix-gitignore.gitignoreSourcePure [../.gitignore] ./.;
  packageJSON = ./package.json;
  yarnLock = ../yarn.lock;
  installPhase = ''
    ${ncc}/bin/ncc build -e pg-cloudflare ./deps/sirimbo-backend/src/index.ts
    sed -i '/\/nix\/store/d' dist/index.js
    mkdir -p $out/bin
    cp dist/index.js $out/bin/sirimbo-backend
    cp node_modules/graphile-build-pg/res/watch-fixtures.sql $out/bin/
    cp -r ./deps/sirimbo-backend/src/tasks/templates $out/bin/templates
    cp -r ./node_modules/graphile-worker/sql $out/bin/sql
  '';
  distPhase = "true";
}
