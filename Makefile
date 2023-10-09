##
# Project Title
#
# @file
# @version 0.1

haddocks:
	cabal-docspec --extra-package latex-svg-image --extra-package directory
	cabal haddock $(package)

report:
	$(MAKE) -C docs/pdfs/report consensus-report
# end
