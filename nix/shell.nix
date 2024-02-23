{ inputs, pkgs, hsPkgs }:

let
  inherit (pkgs) lib;
in
hsPkgs.shellFor {
  nativeBuildInputs = [
    pkgs.cabal
    pkgs.cabal-multi-repl
    pkgs.fd
    pkgs.nixpkgs-fmt
    pkgs.stylish-haskell
    pkgs.cabal-fmt
    pkgs.cabal-hoogle
    pkgs.ghcid

    # release management
    pkgs.scriv
    (pkgs.python3.withPackages (p: [ p.beautifulsoup4 p.html5lib p.matplotlib p.pandas ]))
  ];

  # This is the place for tools that are required to be built with the same GHC
  # version as used in hsPkgs.
  tools = {
    haskell-language-server = {
      src = inputs.haskellNix.inputs."hls-2.6";
      configureArgs = "--disable-benchmarks --disable-tests";
      modules = [{
        packages.ghcide.patches = [
          # https://github.com/haskell/haskell-language-server/issues/4046#issuecomment-1926242056
          ./ghcide-workaround.diff
        ];
      }];
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
