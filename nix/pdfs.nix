final: prev: {
  consensus-pdfs = final.stdenvNoCC.mkDerivation {
    name = "ouroboros-consensus-pdfs";
    src = ../docs/report;
    nativeBuildInputs = [
      (final.texlive.combine {
        inherit (final.texlive)
          amsmath cleveref collection-fontsrecommended enumitem latexmk
          scheme-small siunitx todonotes;
      })
    ];
    buildPhase = ''
      latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" report.tex
    '';
    installPhase = ''
      mkdir -p $out
      cp *.pdf $out/
    '';
  };
}
