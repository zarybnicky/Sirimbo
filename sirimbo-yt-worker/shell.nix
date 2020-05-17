(import ./. {}).shellFor {
  packages = p: [ p.yt-worker ];
  withHoogle = true;
}
