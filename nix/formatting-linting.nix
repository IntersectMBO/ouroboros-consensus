pkgs:

let
  inherit (pkgs) lib;

  runFormatting = tool: script: pkgs.writeShellApplication {
    name = "run-${lib.getName tool}";
    runtimeInputs = [ pkgs.bash pkgs.fd tool ];
    text = ''bash ${script}'';
  };

  checkFormatting = tool: script: pkgs.runCommand
    "check-${lib.getName tool}"
    {
      buildInputs = [ pkgs.fd tool ];
      src = ../.;
    } ''
    unpackPhase
    cd $sourceRoot

    bash ${script}

    EXIT_CODE=0
    diff -ru $src . || EXIT_CODE=$?

    if [[ $EXIT_CODE != 0 ]]
    then
      echo "*** ${tool.name} found changes that need addressed first"
      exit $EXIT_CODE
    else
      echo $EXIT_CODE > $out
    fi
  '';

  checks = {
    fourmolu = checkFormatting pkgs.fourmolu ../scripts/ci/run-fourmolu.sh;
    cabal-gild = checkFormatting pkgs.cabal-gild ../scripts/ci/run-cabal-gild.sh;
    nixpkgs-fmt = checkFormatting pkgs.nixpkgs-fmt ../scripts/ci/run-nixpkgs-fmt.sh;
    dos2unix = checkFormatting pkgs.dos2unix ../scripts/ci/run-dos2unix.sh;
    hlint = pkgs.runCommand "hlint"
      {
        buildInputs = [ pkgs.hlint ];
        src = ../.;
      } ''
      unpackPhase
      cd $sourceRoot

      hlint -j .

      touch $out
    '';
  };

  runs = {
    fourmolu = runFormatting pkgs.fourmolu ../scripts/ci/run-fourmolu.sh;
    cabal-gild = runFormatting pkgs.cabal-gild ../scripts/ci/run-cabal-gild.sh;
    nixpkgs-fmt = runFormatting pkgs.nixpkgs-fmt ../scripts/ci/run-nixpkgs-fmt.sh;
    dos2unix = runFormatting pkgs.dos2unix ../scripts/ci/run-dos2unix.sh;
    hlint = pkgs.writeShellApplication {
      name = "hlint";
      runtimeInputs = [ pkgs.hlint ];
      text = ''hlint -j .'';
    };
  };

  checkAll = pkgs.releaseTools.aggregate {
    name = "consensus-formatting-check";
    meta.description = "Check all formatters and linters";
    constituents = lib.collect lib.isDerivation checks;
  };

  runAll = pkgs.writeShellApplication {
    name = "consensus-formatting-run";
    text = ''for prog in ${with builtins; toString (map lib.getExe (attrValues runs))}; do $prog; done'';
  };

in
checks // {
  all = checkAll;
  run = runAll;
}
