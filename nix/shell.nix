{ pkgs, hsPkgs }:

let
  inherit (pkgs) lib;
in
hsPkgs.shellFor {
  nativeBuildInputs = [
    pkgs.cabal
    pkgs.fd
    pkgs.nixpkgs-fmt
    pkgs.stylish-haskell
    pkgs.cabal-fmt
    pkgs.ghcid

    # release management
    pkgs.scriv
    (pkgs.python3.withPackages (p: [ p.beautifulsoup4 p.html5lib ]))
  ];

  # This is the place for tools that are required to be built with the same GHC
  # version as used in hsPkgs.
  tools = lib.mapAttrs (_: t: t // { index-state = pkgs.tool-index-state; }) {
    haskell-language-server = { version = "2.0.0.1"; };
  };

  shellHook = ''
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = true;
}
