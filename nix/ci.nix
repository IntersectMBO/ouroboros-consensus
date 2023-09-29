pkgs:

let
  inherit (pkgs) lib haskell-nix;
  inherit (haskell-nix) haskellLib;
  buildSystem = pkgs.buildPlatform.system;

  mkHaskellJobsFor = hsPkgs:
    let
      projectHsPkgs =
        haskellLib.selectProjectPackages hsPkgs.hsPkgs;
      projectHsPkgsNoAsserts =
        haskellLib.selectProjectPackages hsPkgs.projectVariants.noAsserts.hsPkgs;
      noCross = buildSystem == hsPkgs.pkgs.stdenv.hostPlatform.system;
    in
    {
      libs =
        haskellLib.collectComponents' "library" projectHsPkgs;
      exes =
        haskellLib.collectComponents' "exes" projectHsPkgs;
      exesNoAsserts =
        haskellLib.collectComponents' "exes" projectHsPkgsNoAsserts;
      benchmarks =
        haskellLib.collectComponents' "benchmarks" projectHsPkgs;
      tests =
        haskellLib.collectComponents' "tests" projectHsPkgs;
      checks =
        haskellLib.collectChecks' projectHsPkgs;
    } // lib.optionalAttrs noCross {
      devShell =
        import ./shell.nix { inherit pkgs hsPkgs; };
    };

  jobs = lib.filterAttrsRecursive (n: v: n != "recurseForDerivations") ({
    native = {
      haskell92 = mkHaskellJobsFor pkgs.hsPkgs;
    } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
      formatting = import ./formatting.nix pkgs;
      inherit (pkgs) consensus-pdfs;

      # ensure we can still build on 8.10, can be removed soon
      haskell810 = builtins.removeAttrs
        (mkHaskellJobsFor pkgs.hsPkgs.projectVariants.ghc810)
        [ "checks" "devShell" ];

      # ensure we can already build with 9.6, but do not yet run tests to reduce CI load
      haskell96 = builtins.removeAttrs
        (mkHaskellJobsFor pkgs.hsPkgs.projectVariants.ghc96)
        [ "checks" ];
    };
  } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
    windows = {
      haskell92 = mkHaskellJobsFor pkgs.hsPkgs.projectCross.mingwW64;
    };
  });

  require = jobs: pkgs.releaseTools.aggregate {
    name = "required-consensus";
    constituents = lib.collect lib.isDerivation jobs;
  };
in
jobs // {
  required = lib.mapAttrs (_: require) jobs;
}
