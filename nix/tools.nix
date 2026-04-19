inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2026-02-23T15:03:19Z";
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

  cuddle = tool "cuddle" "1.2.0.0" { };

  # remove once our nixpkgs contains https://github.com/NixOS/nixpkgs/pull/394873
  cddlc = final.callPackage ./cddlc/package.nix { };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit (final.hsPkgs.args) compiler-nix-name;
    index-state = tool-index-state;
  };
  set-git-rev = drv:
    let
      patched-drv = final.buildPackages.runCommand "${drv.name}-with-git-rev"
        {
          inherit (drv) meta passthru;
          nativeBuildInputs = lib.optionals final.stdenv.hostPlatform.isDarwin
            [ final.darwin.signingUtils ];
        }
        (''
          mkdir -p $out
          cp --no-preserve=timestamps --recursive ${drv}/* $out/
          chmod -R +w $out/bin
          ${final.pkgsBuildBuild.haskellBuildUtils}/bin/set-git-rev ${lib.escapeShellArg inputs.self.rev} $out/bin/*
        '' + lib.optionalString final.stdenv.hostPlatform.isDarwin ''
          for exe in $out/bin/*; do
            signIfRequired "$exe"
          done
        '');
    in
    if inputs.self ? rev then patched-drv else drv;
}
