{ runCommand
, makeWrapper
}:

runCommand "sirimbo-migrations" {
  buildInputs = [makeWrapper];
} ''
  mkdir -p $out/migrations
  cp -r ${./.} migrations/
  cp -r ${../.gmrc} .
''
