inputs: final: prev:

let
  tool-index-state = "2023-10-06T00:00:00Z";
  tool = name: version: other:
    final.haskell-nix.tool final.hsPkgs.args.compiler-nix-name name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;

  cabal = tool "cabal" "latest" { };

  cabal-multi-repl = (final.haskell-nix.cabalProject {
    # cabal master commit containing https://github.com/haskell/cabal/pull/8726
    src = final.fetchFromGitHub {
      owner = "haskell";
      repo = "cabal";
      rev = "249374d16b328736a01a4c7e84fa42fbad7422e7";
      hash = "sha256-KQm3NwQAvsii+6o7MRRL4emCEBUT77foywTBHfq1pxg=";
    };
    index-state = tool-index-state;
    inherit (final.hsPkgs.args) compiler-nix-name;
    cabalProject = ''
      packages: Cabal-syntax Cabal cabal-install-solver cabal-install
    '';
    configureArgs = "--disable-benchmarks --disable-tests";
    modules = [{
      packages.cabal-install.components.exes.cabal.postInstall = ''
        mv $out/bin/cabal $out/bin/cabal-multi-repl
        wrapProgram $out/bin/cabal-multi-repl --add-flags --enable-multi-repl
      '';
    }];
  }).cabal-install.components.exes.cabal;

  cabal-hoogle = tool "cabal-hoogle" "git" {
    src = final.fetchFromGitHub {
      owner = "kokobd";
      repo = "cabal-hoogle";
      rev = "7452c2b1dbdae4eb675d280ed99ec135293adc13";
      hash = "sha256-w7PkNZfHJw1291c2nfviENSXykYpNV+4i3FmbMJqSMs=";
    };
    index-state = null; # use upstream cabal.project
  };

  stylish-haskell = tool "stylish-haskell" "0.14.4.0" { };

  cabal-fmt = tool "cabal-fmt" "0.1.7" { };

  scriv = prev.scriv.overrideAttrs (_: {
    version = "1.2.0-custom-iog";
    src = final.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "scriv";
      rev = "567a1aa3f6df77d1a531290f10a261ec6a49c75a";
      hash = "sha256-wpWDuZ3c8JJKVWPw9PHgcpneRWYjd/0z4oAIirPa0/E=";
    };
  });
}
