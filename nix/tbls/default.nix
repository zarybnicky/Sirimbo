{ fetchFromGitHub
, buildGoModule
}:

buildGoModule rec {
  pname = "tbls";
  version = "1.77.0";

  src = fetchFromGitHub {
    owner = "k1LoW";
    repo = "tbls";
    rev = "v${version}";
    hash = "sha256-knYAwmxqeHv1XBi/zHf7cOkcLXITGnX0tXlT8/Zs2YQ=";
  };

  vendorHash = "sha256-m5G0knHmPCz1pZ7LZ4i6Tyq+xSEq32mQFbXEdOY+6ec=";

  doCheck = false;
  # ldflags = [
  #   "-s"
  #   "-w"
  #   "-X main.version=${version}"
  #   "-X main.currentSha=${src.rev}"
  # ];
}
