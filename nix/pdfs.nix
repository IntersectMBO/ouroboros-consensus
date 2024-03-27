final: prev: {
  consensus-pdfs = final.stdenvNoCC.mkDerivation {
    name = "ouroboros-consensus-pdfs";
    src = ../docs;
    nativeBuildInputs = [
      (final.texlive.combine {
        inherit (final.texlive)
          scheme-small
          collection-latexextra
          collection-latexrecommended
          collection-mathscience
          bclogo
          latexmk;
      })
    ];
    buildPhase = ''
      cd tech-reports

      cd report
      latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" report.tex
      cd ..

      cd utxo-db
      make
      cd ..

      cd ..

      cd formal-spec
      make
      cd ..
    '';
    installPhase = ''
      mkdir -p $out

      cp tech-reports/**/*.pdf $out/
      cp formal-spec/consensus-spec.pdf $out/
    '';
  };
}
