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
  type-errors = pkgs.fetchFromGitHub {
    owner = "isovector";
    repo = "type-errors";
    rev = "f171628f607d83de18bd96c1d4247674fece6b15";
    sha256 = "1pkrns559mh1psp2ic4dybk8x91vcx04xpjx7m7msqx7xypfz2il";
  };
  type-errors-pretty = pkgs.fetchFromGitHub {
    owner = "chshersh";
    repo = "type-errors-pretty";
    rev = "cf63c3363c6a681deae1843eca8524a74df53b12";
    sha256 = "0s12zydzrnyd9920s5vxkbv866gj8bjvqk5a9nzhg7yshiayccwq";
  };
  first-class-families = pkgs.fetchFromGitHub {
    owner = "Lysxia";
    repo = "first-class-families";
    rev = "2fb77468d3b0ce64dc5371bed1b636d60219975c";
    sha256 = "1jvn11nbb7271hhy7q21cyn4r238pfzj00aabd39gwhi0g2gzqbf";
  };
  inspection-testing = pkgs.fetchFromGitHub {
    owner = "nomeata";
    repo = "inspection-testing";
    rev = "cd0a728b5ca69dd7b6b6d73f036cf680341e4188";
    sha256 = "1rwzgyh9zp1754sb7zgqq560q4xw4f2k9a99cc8494ypfxhi7a3y";
  };
  th-abstraction = pkgs.fetchFromGitHub {
    owner = "glguy";
    repo = "th-abstraction";
    rev = "dfdaaa5cb32509774b513659d4dbc415957daf3e";
    sha256 = "1jfkyyz4f0d6b7vzk0j6gc5rs72j4vjwfqbn230nixkwfj0rjjzg";
  };
  th-lift = pkgs.fetchFromGitHub {
    owner = "mboes";
    repo = "th-lift";
    rev = "1f01ef970101e05c86b2ac8313cfce6fb5f30c11";
    sha256 = "03nwaz2azyyif7cbv7ybxlfkzs57lhj4dc7gykgkaqvyz78d3qjc";
  };
  generic-deriving = pkgs.fetchFromGitHub {
    owner = "dreixel";
    repo = "generic-deriving";
    rev = "4333e6902a239179a7755ee981dbee9ae3d1dcf1";
    sha256 = "1wc74jypcmx7fdb8rns3d9706gmgkrdrnqa39d2q8m0gh6dr2j28";
  };
  bifunctors = pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo = "bifunctors";
    rev = "acd1866f8ddad7991ae31d5c66740fc697fef92b";
    sha256 = "0yni4g2ywmj4iajzfw4dyv6jjs8h5n870pd1y7x9pahc75a441jg";
  };
  aeson = pkgs.fetchFromGitHub {
    owner = "bos";
    repo = "aeson";
    rev = "27119773ffeb20535f865ba554b929438cf97f7b";
    sha256 = "185w5r7fx6wpmbj1s7bhxa92gv1jamfh4yl2xy4aanaxfazx6r3g";
  };
  Diff = fetchTarball {
    url = https://hackage.haskell.org/package/Diff-0.4.0/Diff-0.4.0.tar.gz;
    sha256 = "1phz4cz7i53jx3d1bj0xnx8vpkk482g4ph044zv5c6ssirnzq3ng";
  };
  time-compat = pkgs.fetchFromGitHub {
    owner = "phadej";
    repo = "time-compat";
    rev = "89ef24ecf2b9a7f30bf91ec7cc82edc71c7b29d0";
    sha256 = "0ny2i1yv2pqgl6ksj8wg561hi6xdrlg5lp1z7qwrqdbjk9ymkcc0";
  };
  quickcheck-instances = pkgs.fetchFromGitHub {
    owner = "phadej";
    repo = "qc-instances";
    rev = "64c65460e68194781c3dc45861b89bc95366a652";
    sha256 = "126hncv0cwqn1gw2g8jkj4kjd1qcmh7s7724k9pvmqbm6bsd311j";
  };
  typerep-map = pkgs.fetchFromGitHub {
    owner = "kowainik";
    repo = "typerep-map";
    rev = "0ee27c8371e673e697d3c3a93bda152744bee414";
    sha256 = "0s2p6fc0ay4w0gwkd77a30m97zghbf4r9y7k2bbi5779jrwa245j";
  };
  http-api-data = pkgs.fetchFromGitHub {
    owner = "fizruk";
    repo = "http-api-data";
    rev = "432db612b076c23bb5a7af02e512d8020953710f";
    sha256 = "02wkm1b9h1mfxv664nl3qxwpyf3rsdvlispzfjmnpgf5ddiydj22";
  };
  singleton-bool = pkgs.fetchFromGitHub {
    owner = "phadej";
    repo = "singleton-bool";
    rev = "a40fff0530b748559540bec5d4d34e2c5c985909";
    sha256 = "0sgcss19yk5myllah8vkxkdn33kdzf1aq6w66arnpgi0j9slk75f";
  };
  vec = pkgs.fetchFromGitHub {
    owner = "phadej";
    repo = "vec";
    rev = "c069d76376f8ab32ea782694e0b7bdecbc971ab7";
    sha256 = "0gcyr535gd6vk14ma6v0448vhq72pvcky8ww7wxbffrj8g6knq4g";
  };
  invariant = pkgs.fetchFromGitHub {
    owner = "nfrisby";
    repo = "invariant-functors";
    rev = "28234d2797af24274f86e31f6320f9f523a43054";
    sha256 = "0i3wbpqvmnvwdj7d4s9cx6nkasfjbcrn7if6gc2w3a6dhz5wkf82";
  };
  servant = pkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "5285011233575354ded4df2a022af4b2ad24cf6b";
    sha256 = "0xk3czk3jhqjxhy0g8r2248m8yxgvmqhgn955k92z0h7p02lfs89";
  };
  servant-websockets = pkgs.fetchFromGitHub {
    owner = "moesenle";
    repo = "servant-websockets";
    rev = "a6b8b9f1d9d76e31b0f5de1ce6f79222fc1a7002";
    sha256 = "0mirn2z549f4kmqisidhpix4bh5dh9z01j2vnf3klbh057g9dcwk";
  };
  lens = pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo = "lens";
    rev = "eb255ec6c1e5c4f0012ab56268b43a3059d60a16";
    sha256 = "1h2kh9yx65n3vha05gp178ygzsb5ghi3smg91kzzhacf7l5s28i8";
  };
  recursion-schemes = pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo = "recursion-schemes";
    rev = "4e06bd8f4b93ec0e6f6bbfed20e018bdbbc4d78c";
    sha256 = "137qgfsc0h2y5jgwg068x6b19cp7jsxwix5ihb7vwicr6pfa633s";
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
  olymp-api = dontHaddock (self.callCabal2nix "olymp-api" (builtins.filterSource cleanSource ./.) {});
  polysemy = self.callCabal2nix "polysemy" polysemy {};
  polysemy-plugin = self.callCabal2nix "polysemy" "${polysemy}/polysemy-plugin" {};
  type-errors = self.callCabal2nix "type-errors" type-errors {};
  type-errors-pretty = self.callCabal2nix "type-errors-pretty" type-errors-pretty {};
  first-class-families = self.callCabal2nix "type-errors-pretty" first-class-families {};
  th-abstraction = self.callCabal2nix "th-abstraction" th-abstraction {};
  inspection-testing = self.callCabal2nix "inspection-testing" inspection-testing {};
  generic-deriving = self.callCabal2nix "generic-deriving" generic-deriving {};
  th-lift = self.callCabal2nix "th-lift" th-lift {};
  aeson = dontCheck (doJailbreak (self.callCabal2nix "aeson" aeson {}));
  Diff = self.callCabal2nix "Diff" Diff {};
  bifunctors = self.callCabal2nix "bifunctors" bifunctors {};
  time-compat = doJailbreak (self.callCabal2nix "time-compat" time-compat {});
  typerep-map = doJailbreak (dontCheck (self.callCabal2nix "typerep-map" typerep-map {}));
  hpack = dontCheck super.hpack;
  co-log = doJailbreak (dontCheck (self.callCabal2nix "co-log" "${co-log}/co-log" {}));
  co-log-core = self.callCabal2nix "co-log-core" "${co-log}/co-log-core" {};
  servant = self.callCabal2nix "servant" "${servant}/servant" {};
  servant-server = self.callCabal2nix "servant-server" "${servant}/servant-server" {};
  servant-websockets = self.callCabal2nix "servant-websockets" servant-websockets {};
  singleton-bool = overrideCabal (self.callCabal2nix "singleton-bool" singleton-bool {}) (drv: drv // {
    revision = null;
    editedCabalFile = null;
  });
  dec = self.callCabal2nix "dec" "${vec}/dec" {};
  lens = self.callCabal2nix "lens" lens {};
  recursion-schemes = self.callCabal2nix "recursion-schemes" recursion-schemes {};
  invariant = doJailbreak (self.callCabal2nix "invariant" invariant {});
  http-api-data = doJailbreak (dontCheck (self.callCabal2nix "http-api-data" http-api-data {}));
})
