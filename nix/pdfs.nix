inputs: final: prev: {
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

      cd utxo-db-lsm
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
      cp ${final.agda-spec.docs}/pdfs/consensus-spec.pdf $out/consensus-spec-agda.pdf

      mkdir -p $out/nix-support
      for pdf in $out/*.pdf; do
        echo "file binary-dist $pdf" >> $out/nix-support/hydra-build-products
      done
    '';
  };
}
