{
  stdenv,
  nodejs,
  pnpm_9,
  lib,
  packageJSON,
  workspaceFolder,
  postInstall,
  pnpmDepsHash,
  ...
}:
let
  workspaces = [
    "patches"
    workspaceFolder
  ];
  allowedFiles = [
    "package.json"
    "pnpm-lock.yaml"
    "pnpm-workspace.yaml"
  ];

  inWorkspace =
    path: type:
    let
      rel = lib.removePrefix (toString ./.. + "/") (toString path);

      # top-level files to keep
      isAllowedRootFile = lib.elem rel allowedFiles;

      # keep a workspace dir, anything inside it, and any of its parents
      withinWs = ws: rel == ws || lib.hasPrefix (ws + "/") rel;
      parentOfWs = ws: type == "directory" && lib.hasPrefix rel (ws + "/");

      inWorkspace = lib.any (ws: withinWs ws || parentOfWs ws) workspaces;

      # always keep the repo root directory itself
      isRepoRootDir = type == "directory" && rel == "";
    in
    isRepoRootDir || isAllowedRootFile || inWorkspace;
in
stdenv.mkDerivation (finalAttrs: {
  pname = packageJSON.name;
  version = packageJSON.version;
  src = lib.cleanSourceWith {
    src = lib.cleanSource ./..;
    filter = inWorkspace;
  };
  buildInputs = [
    nodejs
  ];
  nativeBuildInputs = [
    nodejs
    pnpm_9.configHook
  ];
  pnpmWorkspaces = [ packageJSON.name ];
  pnpmDeps = pnpm_9.fetchDeps {
    inherit (finalAttrs)
      pname
      version
      src
      pnpmWorkspaces
      ;
    fetcherVersion = 2;
    hash = pnpmDepsHash;
  };
  buildPhase = ''
    runHook preBuild
    pnpm --filter=${packageJSON.name} build
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share $out/bin

    pushd ${workspaceFolder}
    CI=true pnpm prune --prod
    popd
    find -xtype l -delete

    mv ./* $out/share
    ${postInstall}

    # When on pnpm 10, this might be a better way?
    # local -r packageOut="$out/share"
    # pnpm --filter ${packageJSON.name} deploy --prod --no-optional "$packageOut"

    runHook postInstall
  '';
})
