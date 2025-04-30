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
    compiler-nix-name = "ghc966";
    flake.variants = {
      ghc910 = { compiler-nix-name = lib.mkForce "ghc9101"; };
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
