inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2024-07-04T00:00:00Z";
  tool = name: version: other:
    final.haskell-nix.tool final.hsPkgs.args.compiler-nix-name name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;

  cabal = tool "cabal" "3.12.1.0" { };

  cabal-docspec = tool "cabal-docspec" "git" {
    compiler-nix-name = "ghc98";
    src = inputs.cabal-extras;
    cabalProject = ''
      packages: peura cabal-docspec ${inputs.gentle-introduction} paths-0.2.0.0
    '';
  };

  cabal-hoogle = tool "cabal-hoogle" "git" {
    src = final.fetchFromGitHub {
      owner = "kokobd";
      repo = "cabal-hoogle";
      rev = "f3a230de36a08920f8ad47766b0528b9229b3ce6";
      hash = "sha256-WiSq1uBjuSCEW7vp/81a1PVdo/7pf86dqy+R7lDCOdY=";
    };
  };

  stylish-haskell = tool "stylish-haskell" "0.14.6.0" { };

  cabal-gild = tool "cabal-gild" "1.5.0.1" { };

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
