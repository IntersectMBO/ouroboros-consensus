{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {
    overlays = [ ];
    config = { };
  }
}:

with pkgs;

let specs = callPackage ./default.nix {};

in {
  shell = mkShell {
    nativeBuildInputs = [
      specs.agdaWithDeps
      specs.latex
    ];
  };

  run.shell = mkShell {
    nativeBuildInputs = [
      specs.agda
      cabal-install
      (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
        specs.ledger.hsExe
        specs.midnight.hsExe
      ]))
    ];
  };
}
