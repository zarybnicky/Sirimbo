{ pkgs ? import <nixpkgs> {} }:
let
  cleanSource = with pkgs.lib; name: type: let
    baseName = baseNameOf (toString name);
  in cleanSourceFilter name type && !(
    (type == "directory" && (elem baseName [ ".stack-work" "dist" ".git"])) ||
    any (flip hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ]
  );
  polysemy = pkgs.fetchFromGitHub {
    owner = "polysemy-research";
    repo = "polysemy";
    rev = "8aa10efa8a4562eaa3e1dd35f6176eb51613e13d";
    sha256 = "0vaq8cdbxbbvakmhdkczni2j24a4fdg9lisjalmpirl5pywvgrni";
  };
  typerep-map = pkgs.fetchFromGitHub {
    owner = "kowainik";
    repo = "typerep-map";
    rev = "0ee27c8371e673e697d3c3a93bda152744bee414";
    sha256 = "0s2p6fc0ay4w0gwkd77a30m97zghbf4r9y7k2bbi5779jrwa245j";
  };
  servant-websockets = pkgs.fetchFromGitHub {
    owner = "moesenle";
    repo = "servant-websockets";
    rev = "a6b8b9f1d9d76e31b0f5de1ce6f79222fc1a7002";
    sha256 = "0mirn2z549f4kmqisidhpix4bh5dh9z01j2vnf3klbh057g9dcwk";
  };
  co-log-orig = pkgs.fetchFromGitHub {
    owner = "kowainik";
    repo = "co-log";
    rev = "13f6c31d4eb9290c5a5c1ea8de0b2f9f8d0957fa";
    sha256 = "1hkk8xdw9aw05vs76hj1fwwhk3k2c9b0pshmhfn7gxzydpf16vmr";
  };
  co-log = pkgs.runCommand "co-log-source" {} ''
    mkdir -p $out
    cd $out
    cp -rL ${co-log-orig}/{co-log,co-log-core} $out
    chmod +w $out/co-log
    sed -i -e '92,120d' $out/co-log/co-log.cabal
  '';
in with pkgs.haskell.lib; pkgs.haskellPackages.extend (self: super: {
  olymp-tournament = self.callCabal2nix "olymp-tournament" (builtins.filterSource cleanSource ./olymp-tournament) {};
  olymp-schema = self.callCabal2nix "olymp-schema" (builtins.filterSource cleanSource ./olymp-schema) {};
  olymp-api = dontHaddock (self.callCabal2nix "olymp-api" (builtins.filterSource cleanSource ./.) {});
  polysemy = self.callCabal2nix "polysemy" polysemy {};
  polysemy-plugin = self.callCabal2nix "polysemy" "${polysemy}/polysemy-plugin" {};
  typerep-map = doJailbreak (dontCheck (self.callCabal2nix "typerep-map" typerep-map {}));
  co-log = doJailbreak (dontCheck (self.callCabal2nix "co-log" "${co-log}/co-log" {}));
  co-log-core = self.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
  servant-websockets = self.callCabal2nix "servant-websockets" servant-websockets {};
})
