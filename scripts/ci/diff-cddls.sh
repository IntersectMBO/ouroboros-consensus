#!/bin/bash

git clone git@github.com:cardano-scaling/cardano-blueprint

check_diff () {
    diff "ouroboros-consensus-cardano/cddl/node-to-node/$1" "cardano-blueprint/src/network/node-to-node/$1"
    if [ $? -ne 0 ]
    then
        echo "::warning ouroboros-consensus-cardano/cddl/node-to-node/$1 differs from cardano-blueprint/src/network/node-to-node/$1"
    else
        echo "$1 OK"
    fi
}

check_diff "blockfetch/block.cddl"
check_diff "chainsync/header.cddl"
check_diff "txsubmission2/txId.cddl"
check_diff "txsubmission2/tx.cddl"
