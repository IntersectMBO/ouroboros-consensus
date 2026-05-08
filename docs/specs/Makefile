# Literate Quint specifications
#
# Workflow:
#   make          — extract .qnt from .md, typecheck, run tests
#   make extract  — just extract .qnt files
#   make check    — typecheck extracted .qnt files
#   make test     — run quint tests
#   make sim      — run a simulation (20 steps, 3 nodes, k=2)
#   make clean    — remove extracted .qnt files
#
# Prerequisites (provided by nix develop):
#   - lmt   (literate markdown tangle)
#   - quint (Quint specification language)

SPECS := praos.md

# Extracted .qnt files (derived from markdown sources)
QNT_FILES := praos.qnt

.PHONY: all extract check test sim clean

all: extract check

extract: $(QNT_FILES)

$(QNT_FILES) &: $(SPECS)
	lmt $(SPECS)

check: extract
	quint typecheck praos.qnt

sim: extract
	quint run --main PraosRun --max-steps=20 praos.qnt --hide PraosRun::Praos::messages PraosRun::Praos::hashCounter PraosRun::Praos::slot

test: extract
	@if [ -f praosTests.qnt ]; then quint test --main PraosTests praosTests.qnt; else echo "No test file yet"; fi

clean:
	rm -f $(QNT_FILES)
