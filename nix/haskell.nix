inputs: final: prev:

let
  inherit (prev) lib;
  inherit (final) haskell-nix;

  forAllProjectPackages = cfg: args@{ config, lib, ... }: {
    options.packages = lib.genAttrs config.package-keys (_:
      lib.mkOption {
        type = lib.types.submodule ({ config, lib, ... }:
          lib.mkIf config.package.isProject (cfg args)
        );
      });
  };
  hsPkgs = haskell-nix.cabalProject {
    src = ./..;
    compiler-nix-name = "ghc967";
    flake.variants = {
      ghc910 = { compiler-nix-name = lib.mkForce "ghc9102"; };
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
        # Options related to tasty-golden:
        packages.ouroboros-consensus-cardano.components.tests =
          lib.listToAttrs (builtins.map
            (n: lib.nameValuePair "${n}-test" {
              testFlags = lib.mkForce [ "--no-create" ];
              extraSrcFiles = [ "golden/${n}/**/*" ];
            }) [ "byron" "shelley" "cardano" ]);
      }
      ({ pkgs, lib, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        # https://github.com/input-output-hk/haskell.nix/issues/2332
        packages.basement.configureFlags = [ "--hsc2hs-option=--cflag=-Wno-int-conversion" ];
        # We can't cross-compile the ruby gem `cddlc` so we decided to skip this
        # test on Windows in Hydra.
        packages.ouroboros-consensus-cardano.components.tests.cardano-test.preCheck = ''
          export DISABLE_CDDLC=1
        '';
      })
      ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
        # Tools for CBOR/CDDL tests:
        packages.ouroboros-consensus-cardano.components.tests.cardano-test = {
          build-tools =
            [ pkgs.cddlc pkgs.cuddle ];
          extraSrcFiles = [ "cddl/**/*" ];
        };
      })
    ];
    flake.variants = {
      noAsserts = {
        src = lib.mkForce (final.applyPatches {
          name = "consensus-src-no-asserts";
          src = ./..;
          postPatch = ''echo > asserts.cabal'';
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
}
