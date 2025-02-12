# Constants
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
HS_DIR=$(OUT_DIR)/haskell
MALONZO_DIR=MAlonzo/Code
CABAL_TEST=cabal run test

# Agda -> LaTeX -> PDF
latexFiles=$(patsubst %.lagda, $(LATEX_DIR)/%.tex,\
             $(shell find . -name '*.lagda' | sed 's|\.\/||g'))
$(latexFiles): $(LATEX_DIR)/%.tex: %.lagda
	@echo "Compiling $<"
	$(AGDA_RUN) --latex --latex-dir=$(LATEX_DIR) $<

define latexToPdf
    @echo "Generating $@"
    $(eval PDF=$(notdir $@))
    mkdir -p $(dir $@)
    cd $(LATEX_DIR) && $(LATEX) -jobname=$(PDF:.pdf=) $(subst $(LATEX_DIR)/,, $<)
    mv $(LATEX_DIR)/$(PDF) $@
endef

$(PDF_DIR)/$(PDF_NAME).pdf: $(LATEX_DIR)/Spec/PDF.tex $(latexFiles) $(PRE)
	$(latexToPdf)

# Agda -> Haskell
define agdaToHs
    @echo "Generating $@"
    $(eval CABAL_FILE=$(1).cabal)
    $(eval HS_DIST=$(HS_DIR)/Spec)
    mkdir -p $(HS_DIST)
    cp -r Spec/hs-src/* $(HS_DIST)/
    cp Spec/hs-src/$(CABAL_FILE) $(HS_DIST)/
    $(AGDA_RUN) -c --ghc-dont-call-ghc --compile-dir $(HS_DIST) $<
    find $(HS_DIST)/MAlonzo -name "*.hs" -print\
      | sed "s#^$(HS_DIST)/#        #;s#\.hs##;s#/#.#g"\
      >> $(HS_DIST)/$(CABAL_FILE)
endef
HS_CONSENSUS=$(HS_DIR)/Consensus/$(MALONZO_DIR)/Spec/Foreign/HSConsensus.hs
$(HS_CONSENSUS): Spec/Foreign/HSConsensus.agda
	$(call agdaToHs,cardano-consensus-executable-spec)

# User commands
.PHONY: default clean docs hs hsTest

consensus.hs: $(HS_CONSENSUS)

hs: consensus.hs

consensus.hsTest: $(HS_CONSENSUS)
	cd $(HS_DIR)/Spec && $(CABAL_TEST)

hsTest: consensus.hsTest

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
