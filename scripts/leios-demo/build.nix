{ pkgs, inputs, ... }:
{

  devShells = {
    devLeiosDemo = pkgs.mkShell {
      name = "leios-demo";
      buildInputs = with pkgs; with python3Packages;
        [
          python3
          ipython
          pandas
          pip
          virtualenv
          python-lsp-server
          jupyterlab
          black

          nixpkgs-fmt
          nil

          shellcheck
        ];
    };
  };

}
