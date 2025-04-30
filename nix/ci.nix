{ inputs, pkgs }:

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
      set-git-revs =
        lib.mapAttrsRecursiveCond (as: !lib.isDerivation as) (pa: pkgs.set-git-rev);
    in
    {
      libs =
        haskellLib.collectComponents' "library" projectHsPkgs;
      exes =
        set-git-revs (haskellLib.collectComponents' "exes" projectHsPkgs);
      exesNoAsserts =
        set-git-revs (haskellLib.collectComponents' "exes" projectHsPkgsNoAsserts);
      benchmarks =
        haskellLib.collectComponents' "benchmarks" projectHsPkgs;
      tests =
        haskellLib.collectComponents' "tests" projectHsPkgs;
      checks =
        haskellLib.collectChecks' projectHsPkgs;
    } // lib.optionalAttrs noCross {
      devShell =
        import ./shell.nix { inherit inputs pkgs hsPkgs; };
      devShellProfiled =
        import ./shell.nix { inherit inputs pkgs; hsPkgs = hsPkgs.projectVariants.profiled; };
    };

  jobs = lib.filterAttrsRecursive (n: v: n != "recurseForDerivations") ({
    native = {
      haskell96 = mkHaskellJobsFor pkgs.hsPkgs;
    } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
      formattingLinting = import ./formatting-linting.nix pkgs;
      inherit (pkgs) consensus-pdfs agda-spec;

      # also already test GHC 9.10, but only on Linux to reduce CI load
      haskell910 = mkHaskellJobsFor pkgs.hsPkgs.projectVariants.ghc910;
    };
  } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
    windows = {
      haskell96 = mkHaskellJobsFor pkgs.hsPkgs.projectCross.mingwW64;
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
