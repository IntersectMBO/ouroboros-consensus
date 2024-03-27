# Document name
ifndef DOCNAME
	DOCNAME = consensus-spec
endif

.PHONY: $(DOCNAME).pdf all clean

all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(DOCNAME).tex
	latexmk -f -pdf -pdflatex="pdflatex -synctex=1 -interaction=nonstopmode" -use-make $(DOCNAME).tex

watch: $(DOCNAME).tex
	latexmk -pvc -pdf -pdflatex="pdflatex -synctex=1 -interaction=nonstopmode" -use-make $(DOCNAME).tex

clean:
	latexmk -CA

