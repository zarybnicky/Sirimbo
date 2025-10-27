{
  stdenv,
  nodejs_24,
  pnpm_9,
  lib,
  packageJSON,
  workspaceFolders,
  includeFolders ? ["patches"] ++ workspaceFolders,
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
  pnpmWorkspaces = workspaceFolders;
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

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    for folder in ${lib.escapeShellArgs workspaceFolders}; do
      pnpm run -C $folder --if-present lint
    done
    runHook postCheck
  '';

  buildPhase = ''
    runHook preBuild
    for folder in ${lib.escapeShellArgs workspaceFolders}; do
      pnpm run -C $folder --if-present build
    done
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share $out/bin

    for folder in ${lib.escapeShellArgs workspaceFolders}; do
      CI=true pnpm prune -C $folder --prod
    done
    find -xtype l -delete

    mv ./* $out/share
    ${postInstall}

    # When on pnpm 10, this might be a better way?
    # local -r packageOut="$out/share"
    # pnpm --filter ${packageJSON.name} deploy --prod --no-optional "$packageOut"

    runHook postInstall
  '';
})
