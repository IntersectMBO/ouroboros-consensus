##
# Project Title
#
# @file
# @version 0.1

pkg ?= all

install-cabal-docspec:
	curl -sL https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20230517/cabal-docspec-0.0.0.20230517-x86_64-linux.xz > cabal-docspec.xz
	xz -d < cabal-docspec.xz > "$$HOME"/.local/bin/cabal-docspec
	rm -f cabal-docspec.xz
	chmod a+x "$$HOME"/.local/bin/cabal-docspec

install-cabal-hoogle:
	cabal install cabal-hoogle --allow-newer --ignore-project

install-latex-svg-image:
	-git clone https://github.com/jasagredo/latex-svg
	cd latex-svg && git checkout jasagredo/draw-tikz && echo "packages: latex-svg-image" > cabal.project && cabal install latex-svg-image --lib --force-reinstalls
	rm -rf latex-svg

report:
	$(MAKE) -C docs/pdfs/report consensus-report

haddocks: install-cabal-docspec install-latex-svg-image
	cabal build $(pkg)
	-cabal-docspec --extra-package latex-svg-image --extra-package directory
	cabal haddock $(pkg)

hoogle: install-cabal-hoogle
	$(MAKE) haddocks pkg=all
	cabal-hoogle --compiler $$(which ghc | xargs ls -lah | rev | cut -d'/' -f1 | rev) generate
	cabal-hoogle --compiler $$(which ghc | xargs ls -lah | rev | cut -d'/' -f1 | rev) run -- server --local

build:
	cabal build $(pkg)

clean:
	cabal clean
# end
