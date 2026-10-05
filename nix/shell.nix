{ inputs, pkgs, hsPkgs }:

let
  inherit (pkgs) lib;

  # Must be built with the same GHC as used in hsPkgs, hence built from the
  # project rather than taken from `nix/tools.nix`.
  haskell-language-server = hsPkgs.tool "haskell-language-server" {
    src = inputs.hls;
    configureArgs = "--disable-benchmarks --disable-tests";
    cabalProjectLocal = ''
      allow-newer: haddock-library:base
    '';
  };

  # Editors launch `haskell-language-server-wrapper`, whose sole job is to pick
  # the HLS binary matching the project's GHC. Here that choice is already made,
  # but the wrapper executable is not part of the tool output above -- so without
  # this shim the editor silently falls through to whatever HLS is installed
  # system-wide and drives the cradle with a foreign GHC and cabal.
  haskell-language-server-wrapper =
    pkgs.runCommand "haskell-language-server-wrapper" { } ''
      mkdir -p $out/bin
      ln -s ${haskell-language-server}/bin/haskell-language-server \
        $out/bin/haskell-language-server-wrapper
    '';
in
hsPkgs.shellFor {
  nativeBuildInputs = [
    haskell-language-server
    haskell-language-server-wrapper
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
    # WARNING: scriv tests are disabled in this Nix build.
    # Scriv's test suite is incompatible with Click 8.2+ due to removed `mix_stderr` parameter:
    # https://github.com/psf/black/pull/4577
    # https://github.com/pallets/click/pull/2844
    # This is a temporary workaround. TODO: Re-enable tests when scriv is updated.
    (pkgs.scriv.overridePythonAttrs (old: { doCheck = false; }))
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
