{ pkgs, devShell }:

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

  jobs = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations") {
    native = mkHaskellJobsFor pkgs.hsPkgs;
    windows = lib.optionalAttrs (buildSystem == "x86_64-linux")
      (mkHaskellJobsFor pkgs.hsPkgs.projectCross.mingwW64.hsPkgs);
    formatting = import ./formatting.nix pkgs;
    inherit devShell;
  };
  required = pkgs.releaseTools.aggregate {
    name = "required-consensus";
    constituents = lib.collect lib.isDerivation jobs;
  };
in
jobs // {
  required =
    if builtins.elem buildSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
    then required
    else pkgs.emptyDirectory;
}
