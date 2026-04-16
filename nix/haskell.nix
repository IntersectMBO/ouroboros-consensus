inputs: final: prev:

let
  inherit (prev) lib;
  fs = lib.fileset;
  inherit (final) haskell-nix;
  # We're restricting the source closure for the Haskell project to avoid unnecessary reruns of builds and tests.
  # NOTE(bladyjoker): Add any files needed for the Haskell compilation here.
  ouroborosConsensusSrc = fs.toSource {
    root = ./..;
    fileset = fs.unions [
      ../LICENSE
      ../NOTICE
      ../cabal
      ../cabal.project
      ../ouroboros-consensus
      ../ouroboros-consensus-cardano
      ../ouroboros-consensus-diffusion
      ../ouroboros-consensus-protocol
      ../ouroboros-consensus.cabal
    ];
  };
  forAllProjectPackages = cfg: args@{ config, lib, ... }: {
    options.packages = lib.genAttrs config.package-keys (_:
      lib.mkOption {
        type = lib.types.submodule ({ config, lib, ... }:
          lib.mkIf config.package.isProject (cfg args)
        );
      });
  };
  # Pass the git revision as a compile-time GHC preprocessor flag, so that
  # executables embed the commit hash without binary patching (set-git-rev).
  # Binary patching breaks Mach-O code signatures on aarch64-darwin.
  gitRevFlag =
    if inputs.self ? rev
    then [("--ghc-option=-D__GIT_REV__=\\\"" + inputs.self.rev + "\\\"")]
    else [];

  hsPkgs = haskell-nix.cabalProject {
    src = ouroborosConsensusSrc;
    compiler-nix-name = "ghc967";
    flake.variants = {
      ghc910 = { compiler-nix-name = lib.mkForce "ghc9103"; };
      ghc912 = { compiler-nix-name = lib.mkForce "ghc9122"; };
    };
    inputMap = {
      "https://chap.intersectmbo.org/" = inputs.CHaP;
    };
    modules = [
      (forAllProjectPackages ({ ... }: {
        ghcOptions = [ "-Werror" ];
      }))
      {
        # Embed the git revision at compile time via a CPP flag, replacing the
        # old set-git-rev binary patching approach which broke Mach-O code
        # signatures on aarch64-darwin.
        packages.ouroboros-consensus.configureFlags = gitRevFlag;
      }
      {
        # disable haddocks for cardano-diffusion due to https://gitlab.haskell.org/ghc/ghc/-/issues/25739,
        # and cardano-diffusion uses `type data` which triggers the bug.
        # FIXME: we can remove that once ghc9123 is available
        packages.cardano-diffusion.doHaddock = false;
        packages.ouroboros-network.doHaddock = false;
        # disable haddocks some ledger packages as well because of https://gitlab.haskell.org/ghc/ghc/-/issues/25739.
        # FIXME: we can remove that once ghc9123 is available
        packages.cardano-ledger-allegra.components.library.doHaddock = false;
        packages.cardano-ledger-alonzo.components.library.doHaddock = false;
        packages.cardano-ledger-babbage.components.library.doHaddock = false;
        packages.cardano-ledger-binary.components.library.doHaddock = false;
        packages.cardano-ledger-conway.components.library.doHaddock = false;
        packages.cardano-ledger-core.components.library.doHaddock = false;
        packages.cardano-ledger-dijkstra.components.library.doHaddock = false;
        packages.cardano-ledger-mary.components.library.doHaddock = false;
        packages.cardano-ledger-shelley.components.library.doHaddock = false;
        # Options related to tasty and tasty-golden:
        packages.ouroboros-consensus.components.tests =
          lib.listToAttrs (map
            (n: lib.nameValuePair "${n}-test" {
              testFlags = lib.mkForce [ "--no-create --hide-successes" ];
              extraSrcFiles = [ "ouroboros-consensus-cardano/golden/${n}/**/*" ];
            }) [ "byron" "shelley" "cardano" ]);
      }
      ({ pkgs, lib, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        # https://github.com/input-output-hk/haskell.nix/issues/2332
        packages.basement.configureFlags = [ "--hsc2hs-option=--cflag=-Wno-int-conversion" ];
        # We can't cross-compile the ruby gem `cddlc` so we decided to skip this
        # test on Windows in Hydra.
        packages.ouroboros-consensus.components.tests.cardano-test.preCheck = ''
          export DISABLE_CDDLC=1
        '';
      })
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
        # Tools for CBOR/CDDL tests:
        packages.ouroboros-consensus.components.tests.cardano-test = {
          build-tools =
            [ pkgs.cddlc pkgs.cuddle ];
          extraSrcFiles = [ "ouroboros-consensus-cardano/cddl/**/*" ];
        };
      })
    ];
    flake.variants = {
      noAsserts = {
        src = lib.mkForce (final.applyPatches {
          name = "consensus-src-no-asserts";
          src = ouroborosConsensusSrc;
          postPatch = ''echo > cabal/asserts.cabal'';
        });
      };
      profiled = {
        modules = [{
          enableLibraryProfiling = true;
          enableProfiling = true;
          # https://well-typed.com/blog/2023/03/prof-late/
          profilingDetail = "late";
        }];
      };
    };
  };
in
{
  inherit hsPkgs;

  cabal-docspec-check = final.stdenv.mkDerivation {
    name = "cabal-docspec-check";

    src = fs.toSource {
      root = ./..;
      fileset = fs.unions [
        (fs.fileFilter (f: f.hasExt "cabal" || f.hasExt "hs") ./..)
      ];
    };

    nativeBuildInputs = [
      final.fd
      final.cabal-docspec
      (hsPkgs.shellFor {
        withHoogle = false;
        exactDeps = true;
        packages = _: [ ];
        additional = (ps: [ ps.latex-svg-image ] ++ lib.filter (p: p ? components.library)
          (lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps)));
      }).ghc
      final.texliveFull
    ];

    buildPhase = ''
      export CABAL_DIR=$(mktemp -d)
      touch $CABAL_DIR/config $out
      cabal-docspec --no-cabal-plan ouroboros-consensus.cabal
    '';
  };
}
