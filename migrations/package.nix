{ runCommand
, makeWrapper
}:

runCommand "rozpisovnik-api-migrations" {
  buildInputs = [makeWrapper];
} ''
  mkdir -p $out
  cp -r ${./.} $out/migrations
  cp -r ${../.gmrc} $out/.gmrc
''
