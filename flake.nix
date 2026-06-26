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
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:nix-community/flake-compat";
      flake = false;
    };
    hls = {
      url = "github:haskell/haskell-language-server/2.14.0.0";
      flake = false;
    };
    agda-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # for cabal-docspec
    cabal-extras = {
      url = "github:phadej/cabal-extras/cabal-docspec-0.0.0.20250606";
      flake = false;
    };
    gentle-introduction = {
      url = "github:phadej/gentle-introduction";
      flake = false;
    };
  };
  outputs = inputs:
    let
      supportedSystems = [
        "x86_64-linux"
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
            inputs.iohkNix.overlays.haskell-nix-extra
            (import ./nix/tools.nix inputs)
            (import ./nix/haskell.nix inputs)
            (import ./nix/agda.nix inputs)
            (import ./nix/pdfs.nix inputs)
          ];
        };
        hydraJobs = import ./nix/ci.nix { inherit inputs pkgs; };
      in
      {
        devShells = rec {
          default = ghc96;
          ghc96 = hydraJobs.native.haskell96.devShell;
          ghc96-ipe = hydraJobs.native.haskell96.devShellIPE;
          ghc914 = hydraJobs.native.haskell914.devShell;
          ghc914-ipe = hydraJobs.native.haskell914.devShellIPE;

          agda-spec = pkgs.agda-spec.shell;

          website = pkgs.mkShell {
            packages = [ pkgs.nodejs pkgs.yarn ];
          };
        };
        inherit hydraJobs;
        legacyPackages = pkgs;
        packages = hydraJobs.native.haskell96.exesNoAsserts;
      }
    );
}
