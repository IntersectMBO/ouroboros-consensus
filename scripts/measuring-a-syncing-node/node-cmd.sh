function dorun {
    rm -rf mainnet

    dir=$1
    mkdir "$dir"

    time env CHAINSEL_EVENTLOG_FREQ=100 ./result_cardano-node/bin/cardano-node run --topology config/topology.json --config config/config.json --shutdown-on-slot-synced 100000000 +RTS -N12 -s -l-agu 1>log-node.txt 2>&1
    mv log-node.txt cardano-node.eventlog "$dir"
    (cd "$dir"; zstd cardano-node.eventlog; rm cardano-node.eventlog)
}

dorun rts12-gu-100mil
