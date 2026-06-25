{ inputs, pkgs, hsPkgs }:

let
  inherit (pkgs) lib;
in
hsPkgs.shellFor {
  nativeBuildInputs = [
    pkgs.cabal
    pkgs.cabal-docspec
    pkgs.fd
    pkgs.nixpkgs-fmt
    pkgs.dos2unix
    pkgs.cabal-gild
    pkgs.hlint
    pkgs.ghcid
    pkgs.fourmolu
    pkgs.cuddle
    pkgs.cddlc
    pkgs.pretty-simple
    pkgs.scriv
    pkgs.haskell-language-server
    (pkgs.python3.withPackages (p: [ p.beautifulsoup4 p.html5lib p.matplotlib p.pandas ]))
  ];

  shellHook = ''
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = true;
}
