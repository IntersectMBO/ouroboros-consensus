{ inputs, pkgs, devShell }:

let
  inherit (pkgs) lib haskell-nix;
  inherit (haskell-nix) haskellLib;
  buildSystem = pkgs.buildPlatform.system;

  mkHaskellJobsFor = hsPkgs:
    let projectHsPkgs = haskellLib.selectProjectPackages hsPkgs; in
    {
      libs =
        haskellLib.collectComponents' "library" projectHsPkgs;
      exes =
        haskellLib.collectComponents' "exes" projectHsPkgs;
      benchmarks =
        haskellLib.collectComponents' "benchmarks" projectHsPkgs;
      tests =
        haskellLib.collectComponents' "tests" projectHsPkgs;
      checks =
        haskellLib.collectChecks' projectHsPkgs;
    };

  jobs = lib.filterAttrsRecursive (n: v: n != "recurseForDerivations") ({
    native = {
      haskell = mkHaskellJobsFor pkgs.hsPkgs;
      formatting = import ./formatting.nix pkgs;
      inherit devShell;
    } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
      inherit (pkgs) consensus-pdfs;
      # ensure we can still build on 8.10, can be removed soon
      haskell8107 = builtins.removeAttrs
        (mkHaskellJobsFor (
          (pkgs.hsPkgs.appendModule {
            compiler-nix-name = lib.mkForce "ghc8107";
          }).hsPkgs
        )) [ "checks" ];
    };
  } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
    windows = {
      haskell = mkHaskellJobsFor pkgs.hsPkgs.projectCross.mingwW64.hsPkgs;
    };
  });

  require = jobs: pkgs.releaseTools.aggregate {
    name = "required-consensus";
    constituents = lib.collect lib.isDerivation jobs
      # force rebuild on every commit
      ++ [ (pkgs.writeText "rebuild-trigger" (inputs.self.rev or "dirty")) ];
  };
in
jobs // {
  required = lib.mapAttrs (_: require) jobs;
}
