{ stdenv
, typescript
, sass
, src
}:

stdenv.mkDerivation {
  inherit src;
  name = "sirimbo-tournament-frontend";
  phases = "unpackPhase buildPhase";
  buildPhase = ''
    ${typescript}/bin/tsc
    ${sass}/bin/sass index.scss:index.css
    mkdir -p $out
    cp admin.html almond.js bundle.js{,.map} index.css{,.map} index.html react* $out/
  '';
}
