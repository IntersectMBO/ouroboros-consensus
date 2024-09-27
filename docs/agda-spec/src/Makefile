PDF_NAME=consensus-spec
AGDA?=agda
AGDA_RUN=$(AGDA) --transliterate
OUT_DIR?=./dist
LATEX?=latexmk -xelatex -shell-escape -halt-on-error
LATEX_DIR=latex
PRE=$(addprefix $(LATEX_DIR)/,\
  agda.sty agda-latex-macros.sty iohk.sty fonts/* references.bib)
PDF_DIR=$(OUT_DIR)/pdfs
LIB_NAME=Ledger
INTERFACE_LIB_NAME=InterfaceLibrary

# Agda -> LaTeX -> PDF
latexFiles=$(patsubst %.lagda, $(LATEX_DIR)/%.tex,\
             $(shell find . -name '*.lagda' | sed 's|\.\/||g'))
$(latexFiles): $(LATEX_DIR)/%.tex: %.lagda
	@echo "Compiling $<"
	$(AGDA_RUN) --latex --latex-dir=$(LATEX_DIR) $< # --only-scope-checking
define latexToPdf =
    @echo "Generating $@"
    $(eval PDF=$(notdir $@))
    mkdir -p $(dir $@)
    cd $(LATEX_DIR) && $(LATEX) -jobname=$(PDF:.pdf=) $(subst $(LATEX_DIR)/,, $<)
    mv $(LATEX_DIR)/$(PDF) $@
endef

$(PDF_DIR)/$(PDF_NAME).pdf: $(LATEX_DIR)/Spec/PDF.tex $(latexFiles) $(PRE)
	$(latexToPdf)

# User commands
.PHONY: default clean docs

docs: $(PDF_DIR)/$(PDF_NAME).pdf

clean:
	rm -rf $(LATEX_DIR)/Spec\
	       $(LATEX_DIR)/$(LIB_NAME)\
	       $(LATEX_DIR)/$(INTERFACE_LIB_NAME)\
	       $(LATEX_DIR)/$(PDF_NAME).*\
	       $(LATEX_DIR)/*.aux\
	       $(OUT_DIR)/

wipe: clean
	rm -rf $(LATEX_DIR)/Spec\
	       $(OUT_DIR)/
