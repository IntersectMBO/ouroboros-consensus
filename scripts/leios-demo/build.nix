{ pkgs, inputs, ... }:
{

  devShells = {
    devLeiosDemo = pkgs.mkShell {
      name = "leios-demo";
      buildInputs = with pkgs; with python3Packages;
        [
          python
          ipython
          pandas
          altair
          pip
          virtualenv
          python-lsp-server
          black
          itables
          ipywidgets
          jupyterlab-widgets
          widgetsnbextension
          jupyterlab
          jupyter
          venvShellHook

          nixpkgs-fmt
          nil

          shellcheck
        ];
    };
  };

}
