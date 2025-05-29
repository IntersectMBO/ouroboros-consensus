inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2025-05-25T20:50:09Z";
  tool = name: version: other:
    final.haskell-nix.tool "ghc98" name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;

  cabal = tool "cabal" "3.14.2.0" { };

  cabal-docspec = tool "cabal-docspec" "git" {
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

  cabal-gild = tool "cabal-gild" "1.6.0.2" { };

  hlint = tool "hlint" "3.10" { };

  xrefcheck = tool "xrefcheck" "0.3.1" { compiler-nix-name = "ghc96"; };

  fourmolu = tool "fourmolu" "0.18.0.0" { };

  cuddle = tool "cuddle" "git" {
    src = final.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "cuddle";
      rev = "43050522b2c3326dc2bcb95a3fde852bce5bc729";
      hash = "sha256-S3GJBmvBmnbdb7tD2Fq9FNr9Z8iuT/eWwRpRxq9is10=";
    };
  };

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
