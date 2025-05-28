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

      isCardanoExe = p:
        let i = p.identifier;
        in i.name == "ouroboros-consensus-cardano" && i.component-type == "exe";
      setGitRevs =
        lib.mapAttrsRecursiveCond (as: !lib.isDerivation as)
          (_: p: if isCardanoExe p then pkgs.set-git-rev p else p);
    in
    {
      build =
        setGitRevs (haskellLib.mkFlakePackages projectHsPkgs);
      checks =
        haskellLib.mkFlakeChecks (haskellLib.collectChecks' projectHsPkgs);
      exesNoAsserts =
        setGitRevs
          (lib.mapAttrs' (_: p: lib.nameValuePair p.identifier.component-name p)
            (lib.filterAttrs (_: isCardanoExe)
              (haskellLib.mkFlakePackages projectHsPkgsNoAsserts)));
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

      # also test newer GHCs, but only on Linux to reduce CI load
      haskell910 = mkHaskellJobsFor pkgs.hsPkgs.projectVariants.ghc910;
      haskell912 = mkHaskellJobsFor pkgs.hsPkgs.projectVariants.ghc912;
    };
  } // lib.optionalAttrs (buildSystem == "x86_64-linux") {
    windows = {
      # https://github.com/input-output-hk/haskell.nix/issues/2361
      haskell912 = mkHaskellJobsFor pkgs.hsPkgs.projectCross.ucrt64.projectVariants.ghc912;
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
