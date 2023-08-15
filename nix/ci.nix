pkgs:

let
  inherit (pkgs) lib haskell-nix;
  inherit (haskell-nix) haskellLib;
  buildSystem = pkgs.buildPlatform.system;

  mkHaskellJobsFor = hsPkgs:
    let
      projectHsPkgs = haskellLib.selectProjectPackages hsPkgs.hsPkgs;
      noCross = buildSystem == hsPkgs.pkgs.stdenv.hostPlatform.system;
    in
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
    } // lib.optionalAttrs noCross {
      devShell =
        import ./shell.nix { inherit pkgs hsPkgs; };
    };

  hsPkgsForGhc = ghcVer:
    pkgs.hsPkgs.appendModule { compiler-nix-name = lib.mkForce ghcVer; };

  jobs = lib.filterAttrsRecursive (n: v: n != "recurseForDerivations") ({
    native = {
      haskell = mkHaskellJobsFor pkgs.hsPkgs;
      haskellNoAsserts = (mkHaskellJobsFor pkgs.hsPkgsNoAsserts).exes;
    } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
      formatting = import ./formatting.nix pkgs;
      inherit (pkgs) consensus-pdfs;

      # ensure we can still build on 8.10, can be removed soon
      haskell810 = builtins.removeAttrs
        (mkHaskellJobsFor (hsPkgsForGhc "ghc8107"))
        [ "checks" "devShell" ];

      # ensure we can already build with 9.6, but do not yet run tests to reduce CI load
      haskell96 = builtins.removeAttrs
        (mkHaskellJobsFor (hsPkgsForGhc "ghc962"))
        [ "checks" ];
    };
  } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
    windows = {
      haskell = mkHaskellJobsFor pkgs.hsPkgs.projectCross.mingwW64;
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
