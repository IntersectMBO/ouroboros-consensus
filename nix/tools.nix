inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2024-01-22T00:00:00Z";
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
      rev = "adc283a0f06c7d24aeed67e69aca3d71c04010b3";
      hash = "sha256-3K9WVR/tINK3PyGlXpypSpp1pguHTnolDruHNE+VvE4=";
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
    cabalProjectLocal = ''
      allow-newer: cabal-hoogle:*
    '';
  };

  stylish-haskell = tool "stylish-haskell" "0.14.6.0" { };

  cabal-fmt = tool "cabal-fmt" "0.1.10" { };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit (final.hsPkgs.args) compiler-nix-name;
    index-state = tool-index-state;
  };
  set-git-rev = drv:
    let
      patched-drv = final.applyPatches {
        name = "${drv.name}-with-git-rev";
        src = drv;
        postPatch = ''
          ${final.haskellBuildUtils}/bin/set-git-rev \
            ${lib.escapeShellArg inputs.self.rev} bin/*
        '';
      };
    in
    if inputs.self ? rev then patched-drv else drv;
}
