export CDDL_INCLUDE_PATH=ledger-cddls/eras/byron/cddl-spec:ledger-cddls/eras/shelley/impl/cddl-files:ledger-cddls/eras/allegra/impl/cddl-files:ledger-cddls/eras/mary/impl/cddl-files:ledger-cddls/eras/alonzo/impl/cddl-files:ledger-cddls/eras/babbage/impl/cddl-files:ledger-cddls/eras/conway/impl/cddl-files:disk:.:

cddlc -u2tcddl disk/block.cddl > block.compiled.cddl
cddlc -u2tcddl disk/ledger-state-snapshot.cddl > ledger-state-snapshot.compiled.cddl
cddlc -u2tcddl node-to-node/blockfetch.cddl > blockfetch.compiled.cddl
cddlc -u2tcddl node-to-node/txsubmission2.cddl > txsubmission2.compiled.cddl
cddlc -u2tcddl node-to-node/chainsync.cddl > chainsync.compiled.cddl
cddlc -u2tcddl node-to-client/localstatequery.cddl > localstatequery.compiled.cddl
