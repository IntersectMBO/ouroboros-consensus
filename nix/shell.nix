pkgs:

let
  inherit (pkgs) lib;
in
pkgs.hsPkgs.shellFor {
  nativeBuildInputs = [
    pkgs.cabal
    pkgs.fd
    pkgs.nixpkgs-fmt
    pkgs.stylish-haskell
    pkgs.cabal-fmt
    pkgs.haskell-language-server
    pkgs.ghcid

    # release management
    pkgs.scriv
    (pkgs.python3.withPackages (p: [ p.beautifulsoup4 p.html5lib ]))
  ];

  shellHook = ''
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = false;
}
