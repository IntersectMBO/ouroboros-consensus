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
    pkgs.cabal-hoogle
    pkgs.ghcid
    pkgs.xrefcheck
    pkgs.fourmolu
    pkgs.cuddle
    pkgs.cddlc
    pkgs.pretty-simple

    # release management
    pkgs.scriv
    (pkgs.python3.withPackages (p: [ p.beautifulsoup4 p.html5lib p.matplotlib p.pandas ]))
  ];

  # This is the place for tools that are required to be built with the same GHC
  # version as used in hsPkgs.
  tools = {
    haskell-language-server = {
      src = inputs.hls;
      configureArgs = "--disable-benchmarks --disable-tests";
      cabalProjectLocal = ''
        allow-newer: haddock-library:base
      '';
    };
  };

  shellHook = ''
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = true;
}
