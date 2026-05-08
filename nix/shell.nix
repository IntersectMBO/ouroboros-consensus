{ inputs, pkgs, hsPkgs }:

let
  inherit (pkgs) lib;

  # lmt: Literate Markdown Tangle — extracts code blocks from markdown files.
  # Used for literate Quint specifications (docs/website/contents/references/specs/).
  # https://github.com/driusan/lmt
  lmt = pkgs.stdenv.mkDerivation {
    pname = "lmt";
    version = "unstable-2022-07-01";
    src = pkgs.fetchFromGitHub {
      owner = "driusan";
      repo = "lmt";
      rev = "62fe18f";
      hash = "sha256-6/jDh5dfpFAhOspKto2hB8TTSjjh++GkQWjRBaFrYZg=";
    };
    nativeBuildInputs = [ pkgs.go ];
    buildPhase = ''
      export HOME=$TMPDIR
      go build -o lmt main.go
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp lmt $out/bin/
    '';
  };
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
    pkgs.quint
    lmt

    # release management
    # WARNING: scriv tests are disabled in this Nix build.
    # Scriv's test suite is incompatible with Click 8.2+ due to removed `mix_stderr` parameter:
    # https://github.com/psf/black/pull/4577
    # https://github.com/pallets/click/pull/2844
    # This is a temporary workaround. TODO: Re-enable tests when scriv is updated.
    (pkgs.scriv.overridePythonAttrs (old: { doCheck = false; }))
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
    hoogle.cabalProjectLocal = ''
      if impl(ghc <9.7)
        constraints: alfred-margaret <2.1.1.0 || >2.1.1.0
    '';
  };

  shellHook = ''
    export LANG="en_US.UTF-8"
  '' + lib.optionalString
    (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '';

  withHoogle = true;
}
