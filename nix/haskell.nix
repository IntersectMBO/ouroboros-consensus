inputs: final: prev:

let
  inherit (prev) lib;
  inherit (final) haskell-nix;

  # from https://github.com/input-output-hk/haskell.nix/issues/298#issuecomment-767936405
  forAllProjectPackages = cfg: args@{ lib, ... }: {
    options.packages = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
        config = lib.mkIf config.package.isProject (cfg args);
      }));
    };
  };
  hsPkgs = haskell-nix.cabalProject {
    src = ./..;
    compiler-nix-name = "ghc963";
    flake.variants = {
      ghc810 = { compiler-nix-name = lib.mkForce "ghc8107"; };
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
      ({ lib, pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        reinstallableLibGhc = false;
      })
    ];
    flake.variants.noAsserts = {
      src = lib.mkForce (final.applyPatches {
        name = "consensus-src-no-asserts";
        src = ./..;
        postPatch = ''echo > asserts.cabal'';
      });
    };
  };
in
{
  inherit hsPkgs;
}
