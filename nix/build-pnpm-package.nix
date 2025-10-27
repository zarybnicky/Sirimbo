{
  stdenv,
  nodejs_24,
  pnpm_9,
  lib,
  packageJSON,
  workspaceFolder,
  includeFolders ? [
    "patches"
    workspaceFolder
  ],
  includeFiles ? [
    "package.json"
    "pnpm-lock.yaml"
    "pnpm-workspace.yaml"
  ],
  postInstall,
  pnpmDepsHash,
  ...
}:
let
  inWorkspace =
    path: type:
    let
      rel = lib.removePrefix (toString ./.. + "/") (toString path);

      # top-level files to keep
      isAllowedRootFile = lib.elem rel includeFiles;

      # keep a workspace dir, anything inside it, and any of its parents
      withinWs = ws: rel == ws || lib.hasPrefix (ws + "/") rel;
      parentOfWs = ws: type == "directory" && lib.hasPrefix rel (ws + "/");

      inWorkspace = lib.any (ws: withinWs ws || parentOfWs ws) includeFolders;

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
    nodejs_24
  ];
  nativeBuildInputs = [
    nodejs_24
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
    pnpm --filter=${packageJSON.name} lint
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
