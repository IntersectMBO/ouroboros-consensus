{
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.follows = "haskellNix/flake-utils";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    cardano-node.url =
      "github:input-output-hk/cardano-node/8063de9354de737788e038a56946c74d5227b05d";
  };
  outputs = inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        #"aarch64-linux"
        "aarch64-darwin"
      ];
    in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          inherit (inputs.haskellNix) config;
          overlays = [
            inputs.iohkNix.overlays.crypto
            inputs.haskellNix.overlay
            inputs.iohkNix.overlays.haskell-nix-crypto
            (import ./nix/tools.nix inputs)
            (import ./nix/haskell.nix inputs)
            (import ./nix/pdfs.nix)
            (_: _: {
              long-range-attack = {
                inherit (inputs.cardano-node.packages.${system}) cardano-cli;
                baseline = inputs.cardano-node.packages.${system}.cardano-node;
                genesis-poc = (inputs.cardano-node.project.${system}.appendModule {
                  cabalProjectLocal = ''
                    packages:
                      ${./ouroboros-consensus}
                      ${./ouroboros-consensus-diffusion}
                      ${./ouroboros-consensus-protocol}
                      ${./ouroboros-consensus-cardano}
                  '';
                }).hsPkgs.cardano-node.components.exes.cardano-node;
              };
            })
          ];
        };
        hydraJobs = import ./nix/ci.nix pkgs;
        poc = import ./nix/genesis-poc.nix { inherit pkgs; inherit (inputs) self; };
      in
      {
        devShells = {
          default = hydraJobs.native.haskell.devShell;
          ghc96 = hydraJobs.native.haskell96.devShell;
          website = pkgs.mkShell {
            packages = [ pkgs.nodejs pkgs.yarn ];
          };
          genesis-poc = hydraJobs.native.haskell.devShell.overrideAttrs (old: {
            shellHook = ''
            ${old.shellHook}
            ${poc.bootstrap}
            '';
          });
        };
        inherit hydraJobs;
        legacyPackages = pkgs;
        apps.genesis-poc = {
          type = "app";
          program = "${poc.run}";
        };
      }
    );
}
