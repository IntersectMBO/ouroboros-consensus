inputs: final: prev:

let
  inherit (prev) lib;
  inherit (final) haskell-nix;

  # from https://github.com/input-output-hk/haskell.nix/issues/298#issuecomment-767936405
  forAllProjectPackages = cfg: ctx@{ lib, ... }: {
    options.packages = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
        config = lib.mkIf config.package.isProject (cfg ctx);
      }));
    };
  };
in
{
  hsPkgs = haskell-nix.cabalProject {
    src = ./..;
    compiler-nix-name = "ghc928";
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
    };
    modules = [
      (forAllProjectPackages ({ config, lib, ... }: {
        ghcOptions = [ "-Werror" ] ++
          lib.optional (lib.hasPrefix "ghc96" config.compiler-nix-name) "-Wno-error=redundant-constraints";
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
  };
}
