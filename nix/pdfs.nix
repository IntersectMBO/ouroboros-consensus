final: prev: {
  consensus-pdfs = final.stdenvNoCC.mkDerivation {
    name = "ouroboros-consensus-pdfs";
    src = ../docs/pdfs;
    nativeBuildInputs = [
      (final.texlive.combine {
        inherit (final.texlive)
          amsmath cleveref collection-fontsrecommended enumitem latexmk
          scheme-small siunitx todonotes accents;
      })
    ];
    buildPhase = ''
      cd report
      latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" report.tex
      cd ..

      cd utxo-db
      make
      cd ..
    '';
    installPhase = ''
      mkdir -p $out

      cp **/*.pdf $out/
    '';
  };
}
