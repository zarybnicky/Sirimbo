{ stdenv
, hasura-cli
, makeWrapper
, hasura-cli-ext
, hasura-console-assets
}:
stdenv.mkDerivation rec {
  name = hasura-cli.name;
  buildInputs = [ makeWrapper hasura-cli ];
  phases = ["installPhase"];
  installPhase = ''
    mkdir -p $out/bin
    ln -s ${hasura-cli}/bin/hasura $out/bin/hasura
    wrapProgram $out/bin/hasura \
      --add-flags "--cli-ext-path" --add-flags "${hasura-cli-ext}/bin/cli-ext-hasura"
    ln -s ${hasura-cli}/bin/hasura $out/bin/hasura-console
    wrapProgram $out/bin/hasura-console \
      --add-flags "console" \
      --add-flags "--cli-ext-path" --add-flags "${hasura-cli-ext}/bin/cli-ext-hasura" \
      --add-flags "--static-dir" --add-flags "${hasura-console-assets}"
  '';
}

