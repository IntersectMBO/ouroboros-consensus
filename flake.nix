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
    tullia.url = "github:input-output-hk/tullia";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskellNix) config;
        overlays = [
          inputs.haskellNix.overlay
          inputs.iohkNix.overlays.crypto
          (import ./nix/tools.nix inputs)
          (import ./nix/haskell.nix inputs)
          (import ./nix/pdfs.nix)
        ];
      };
      inherit (pkgs) lib haskell-nix;
      inherit (haskell-nix) haskellLib;
      devShell = import ./nix/shell.nix pkgs;
    in
    {
      devShells = {
        default = devShell;
        website = pkgs.mkShell {
          packages = [ pkgs.nodejs pkgs.yarn ];
        };
      };
      hydraJobs = import ./nix/ci.nix { inherit inputs pkgs devShell; };
      legacyPackages = pkgs;
    } // inputs.tullia.fromSimple system (import ./nix/tullia.nix)
  );
}
