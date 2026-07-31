{ inputs, pkgs }:

let
  inherit (pkgs) lib haskell-nix;
  inherit (haskell-nix) haskellLib;
  buildSystem = pkgs.stdenv.buildPlatform.system;

  # When `exesOnly` is set, only the (distributed) executables are built. This
  # is used for non-Linux platforms (macOS and the Windows cross), where we only
  # want to ensure that the executables we distribute are buildable, and skip
  # the libraries, tests and benchmarks to reduce CI load. The dev shell is
  # still made available here (for `nix develop` on those platforms), but is
  # excluded from `required` below so Hydra doesn't build it there.
  mkHaskellJobsFor = { exesOnly ? false }: hsPkgs:
    let
      projectHsPkgs =
        haskellLib.selectProjectPackages hsPkgs.hsPkgs;
      projectHsPkgsNoAsserts =
        haskellLib.selectProjectPackages hsPkgs.projectVariants.noAsserts.hsPkgs;
      noCross = buildSystem == hsPkgs.pkgs.stdenv.hostPlatform.system;

      isCardanoExe = p:
        let i = p.identifier;
        in i.name == "ouroboros-consensus" && i.component-type == "exe";
      setGitRevs =
        lib.mapAttrsRecursiveCond (as: !lib.isDerivation as)
          (_: p: if isCardanoExe p then pkgs.set-git-rev p else p);

      exesNoAsserts =
        setGitRevs
          (lib.mapAttrs' (_: p: lib.nameValuePair p.identifier.component-name p)
            (lib.filterAttrs (_: isCardanoExe)
              (haskellLib.mkFlakePackages projectHsPkgsNoAsserts)));
    in
    (if exesOnly then {
      inherit exesNoAsserts;
    } else {
      build =
        setGitRevs (haskellLib.mkFlakePackages projectHsPkgs);
      checks =
        haskellLib.mkFlakeChecks (haskellLib.collectChecks' projectHsPkgs);
      inherit exesNoAsserts;
    }) // lib.optionalAttrs noCross {
      devShell =
        import ./shell.nix { inherit inputs pkgs hsPkgs; };
    };

  isLinux = buildSystem == "x86_64-linux";

  jobs = lib.filterAttrsRecursive (n: v: n != "recurseForDerivations") ({
    native = {
      # On Linux we build everything; on other platforms (i.e. macOS) we only
      # build the distributed executables (the dev shell is still exposed for
      # local use, see `mkHaskellJobsFor` above and `jobsForRequired` below).
      haskell96 = mkHaskellJobsFor { exesOnly = !isLinux; } pkgs.hsPkgs;
    } // lib.optionalAttrs isLinux {
      formattingLinting = import ./formatting-linting.nix pkgs;
      inherit (pkgs) cabal-docspec-check consensus-pdfs agda-spec;

      # also test latest GHC, but only on Linux to reduce CI load
      haskell914 = mkHaskellJobsFor { } pkgs.hsPkgs.projectVariants.ghc914 // {
        devShellIPE =
          import ./shell.nix { inherit inputs pkgs; hsPkgs = pkgs.hsPkgs.projectVariants.ghc914.projectVariants.ipe; };
      };
    } // lib.optionalAttrs isLinux {
      windows = {
        # On the Windows cross we only build the distributed executables.
        haskell914 = mkHaskellJobsFor { exesOnly = true; } pkgs.hsPkgs.projectVariants.ghc914.projectCross.ucrt64;
      };
    };
  });

  # `required` (below) drives what Hydra actually forces the build of. Where
  # `exesOnly` is set we don't want that to include the dev shell: it's
  # exposed in `jobs` purely so `nix develop` resolves on those platforms
  # (e.g. macOS), not so CI builds it too — that's exactly the cost `exesOnly`
  # exists to avoid. So strip it back out for the purposes of `required` only.
  jobsForRequired =
    jobs // lib.optionalAttrs (!isLinux) {
      native = jobs.native // {
        haskell96 = removeAttrs jobs.native.haskell96 [ "devShell" ];
      };
    };

  require = jobs: pkgs.releaseTools.aggregate {
    name = "required-consensus";
    constituents = lib.collect lib.isDerivation jobs;
  };
in
jobs // {
  required = lib.mapAttrs (_: require) jobsForRequired;
}
