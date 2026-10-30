AGDA?=agda
AGDA_RUN=$(AGDA) --transliterate
OUT_DIR?=./dist
HS_DIR=$(OUT_DIR)/haskell
CABAL_TEST=cabal run test

.DEFAULT_GOAL := check

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
# Always invoke Agda so changes in imported Markdown modules are picked up.
consensus.hs: Spec/Foreign/HSConsensus.agda
	$(call agdaToHs,cardano-consensus-executable-spec)

.PHONY: check docs hs hsTest consensus.hs consensus.hsTest clean

# Markdown is read directly in the repository. Checking it needs no rendering.
check:
	$(AGDA_RUN) Everything.agda

docs: check

hs: consensus.hs

consensus.hsTest: consensus.hs
	cd $(HS_DIR)/Spec && $(CABAL_TEST)
hsTest: consensus.hsTest

clean:
	rm -rf $(OUT_DIR)/
