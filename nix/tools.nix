inputs: final: prev:

let
  inherit (final) lib;
  tool-index-state = "2026-06-18T04:19:00Z";
  tool = name: version: other:
    final.haskell-nix.tool "ghc912" name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;

  cabal = tool "cabal" "3.16.1.0" { };

  cabal-docspec = tool "cabal-docspec" "git" {
    compiler-nix-name = "ghc98";
    src = inputs.cabal-extras;
    cabalProject = ''
      packages: peura cabal-docspec ${inputs.gentle-introduction} paths-0.2.0.0
    '';
  };

  cabal-gild = tool "cabal-gild" "1.8.4.1" { };

  hlint = tool "hlint" "3.10" { };

  fourmolu = tool "fourmolu" "0.20.0.0" { };

  cuddle = tool "cuddle" "1.8.0.0" { };

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
