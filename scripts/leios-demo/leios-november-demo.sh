#!/bin/bash

if [[ ! "$SECONDS_UNTIL_REF_SLOT" =~ ^[0-9]*$ ]] || [[ "$SECONDS_UNTIL_REF_SLOT" -le 0 ]]; then
     echo "Error: \${SECONDS_UNTIL_REF_SLOT} must be a positive integer of seconds, which will be added to the execution time of this script." >&2
     exit 1
fi

if [ ! -d "${CLUSTER_RUN_DATA%/}" ]; then
    CLUSTER_RUN_DATA="${CLUSTER_RUN_DATA%/}"
    echo "Error: CLUSTER_RUN_DATA directory '$CLUSTER_RUN_DATA' not found or is not a directory." >&2
    exit 1
fi

if [[ -z "${CARDANO_NODE}" ]]; then
    echo "Error: \${CARDANO_NODE} must be the path to the cardano-node exe." >&2
    exit 1
fi

if [[ -z "${IMMDB_SERVER}" ]]; then
    echo "Error: \${IMMDB_SERVER} must be the path to the immdb-server exe." >&2
    exit 1
fi

if [[ -z "${LEIOS_SCHEDULE}" ]]; then
    echo "Error: \${LEIOS_SCHEDULE} must be the path to the JSON file that lists the schedule of Leios offers." >&2
    exit 1
fi

if [[ -z "${LEIOS_UPSTREAM_DB_PATH}" ]]; then
    echo "Error: \${LEIOS_UPSTREAM_DB_PATH} must be the path to the source Leios database." >&2
    exit 1
fi

if [[ -z "${REF_SLOT}" ]] || [[ ! "$REF_SLOT" =~ ^[0-9]*$ ]] || [[ "$REF_SLOT" -lt 0 ]]; then
     echo "Error: \${REF_SLOT} must be a non-negative integer, a slot number" >&2
     exit 1
fi

now=$(date +%s)
ONSET_OF_REF_SLOT=$(( $now + ${SECONDS_UNTIL_REF_SLOT} ))
echo "REF_SLOT=$REF_SLOT"
echo "ONSET_OF_REF_SLOT=$ONSET_OF_REF_SLOT"
echo "$REF_SLOT" >ref_slot
echo "$ONSET_OF_REF_SLOT" >onset_of_ref_slot

#
# one veth pair per edge in the topology (TODO one veth pair with a SFQ qdisc for each edge?)
#

cleanup_netns() {
    for i in 1 2 3; do
        echo "pids in ns${i}:"
        for pid in $(sudo ip netns pids ns$i); do
            echo -n "$pid "
            ps -aux | grep -e $pid
        done
    done
    for i in 1 2 3; do sudo ip netns del ns$i; done

    # no need to cleanup links, since deleting the namespace deletes those
    # links

    # the script itself invokes this handler directly to stop the processes, so
    # reset the SIGNAL handlers
    trap - EXIT INT TERM
}
trap cleanup_netns EXIT INT TERM

for i in 1 2 3; do set -x; sudo ip netns add ns$i; { set +x; } 2>/dev/null; done
set -x
sudo ip link add veth12 netns ns1 type veth peer name veth21 netns ns2
sudo ip link add veth23 netns ns2 type veth peer name veth32 netns ns3
{ set +x; } 2>/dev/null

# adapted from https://unix.stackexchange.com/a/558427
add_addrs() {
    i=$1
    j=$2
    set -x
    sudo ip -n ns$i link set veth$i$j up
    sudo ip -n ns$j link set veth$j$i up
    sudo ip -n ns$i addr add 10.0.0.$i/32 dev veth$i$j
    sudo ip -n ns$i addr add local 10.0.0.$i peer 10.0.0.$j dev veth$i$j
    sudo ip -n ns$j addr add local 10.0.0.$j peer 10.0.0.$i dev veth$j$i
    { set +x; } 2>/dev/null
}

add_addrs 1 2
add_addrs 2 3

# mydelay="netem delay 100ms"
# myrate="tbf rate 20mbit burst 32kbit latency 70ms"   # burst and latency from Gemini 2.5 Pro
add_qdiscs() {
    i=$1
    j=$2
    # I Googled "netem delay after tbf of tbf after netem delay" and the AI
    # Overview implied that I want netem as the child.
    #
    # Also, the netem man page gives an example where a tbf is the parent of a
    # netem delay https://man7.org/linux/man-pages/man8/tc-netem.8.html
    set -x

    # My inital setup, but Marcin Wójtowicz says the netem delay qdisc is too
    # dumb and so still queueing too much, ie bufferbloat
    #
    # sudo tc -n ns$i qdisc add dev veth$i$j handle 1: root $myrate
    # sudo tc -n ns$i qdisc add dev veth$i$j parent 1: handle 2: $mydelay

    # Gemini 2.5 Pro asserts the fq_codel defaults are both robust and also
    # realistic for this long fat intercontinental scenario
    #
    # TODO compare and contrast once Marcin Wójtowicz sends recommendations
    sudo tc -n ns$i qdisc add dev veth$i$j root handle 1: netem delay 20ms
    sudo tc -n ns$i qdisc add dev veth$i$j parent 1:1 handle 10: htb default 1
    sudo tc -n ns$i class add dev veth$i$j parent 10: classid 10:1 htb rate 100mbit
    sudo tc -n ns$i qdisc add dev veth$i$j parent 10:1 handle 100: fq_codel

    { set +x; } 2>/dev/null
}

add_qdiscs 1 2
add_qdiscs 2 1
add_qdiscs 2 3
add_qdiscs 3 2

UPSTREAM_BIND_ADDR=10.0.0.1
UPSTREAM_BIND_PORT=3001
NODE0_BIND_ADDR=10.0.0.2
NODE0_BIND_PORT=3002
# TODO an actually mocked downstream peer wouldn't need to call bind()
DOWNSTREAM_BIND_ADDR=10.0.0.3
DOWNSTREAM_BIND_PORT=3003

FROM_NODE0_TO_UPSTREAM_ADDR=${UPSTREAM_BIND_ADDR}
FROM_NODE0_TO_UPSTREAM_PORT=${UPSTREAM_BIND_PORT}
FROM_DOWNSTREAM_TO_NODE0_ADDR=${NODE0_BIND_ADDR}
FROM_DOWNSTREAM_TO_NODE0_PORT=${NODE0_BIND_PORT}

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/leios-november-demo.XXXXXX)
echo "Using temporary directory for DB and logs: $TMP_DIR"

rm -f ./leios-run-tmp-dir
ln -s "$TMP_DIR" ./leios-run-tmp-dir

pushd "$CARDANO_NODE_PATH" > /dev/null

##
## Run cardano-node (node0)
##

echo "Creating topology-node0.json in $(pwd)"
cat << EOF > topology-node0.json
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "${FROM_NODE0_TO_UPSTREAM_ADDR}",
          "port": ${FROM_NODE0_TO_UPSTREAM_PORT}
        }
      ],
      "advertise": false,
      "trustable": true,
      "valency": 1
    }
  ],
  "publicRoots": []
}
EOF

mkdir -p "$TMP_DIR/node0/db"

cp "$LEIOS_UPSTREAM_DB_PATH" "$TMP_DIR/node0/leios.db"
sqlite3 "$TMP_DIR/node0/leios.db" 'DELETE FROM ebTxs; DELETE FROM txCache; DELETE FROM ebPoints; VACUUM'

CARDANO_NODE_CMD="sudo ip netns exec ns2 env LEIOS_DB_PATH=$TMP_DIR/node0/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node0.json
    --database-path $TMP_DIR/node0/db
    --socket-path node0.socket
    --host-addr $NODE0_BIND_ADDR --port $NODE0_BIND_PORT"

echo "node0: $CARDANO_NODE_CMD"

$CARDANO_NODE_CMD 1> "$TMP_DIR/node0.log" 2>"$TMP_DIR/node0.stderr" &

CARDANO_NODE_0_PID=$!

cleanup_node_0() {
    sudo ip netns exec ns2 kill $CARDANO_NODE_0_PID
    cleanup_netns
}
trap cleanup_node_0 EXIT INT TERM

echo "node0 started with PID: $CARDANO_NODE_0_PID"

##
## Run a second Cardano-node (To be eventually replaced by a mocked downstream node)
##

cat << EOF > topology-downstream.json
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "${FROM_DOWNSTREAM_TO_NODE0_ADDR}",
          "port": ${FROM_DOWNSTREAM_TO_NODE0_PORT}
        }
      ],
      "advertise": false,
      "trustable": true,
      "valency": 1
    }
  ],
  "publicRoots": []
}
EOF

mkdir -p "$TMP_DIR/downstream/db"

cp "$LEIOS_UPSTREAM_DB_PATH" "$TMP_DIR/downstream/leios.db"
sqlite3 "$TMP_DIR/downstream/leios.db" 'DELETE FROM ebTxs; DELETE FROM txCache; DELETE FROM ebPoints; VACUUM;'

DOWNSTREAM_PEER_CMD="sudo ip netns exe ns3 env LEIOS_DB_PATH=$TMP_DIR/downstream/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-downstream.json
    --database-path $TMP_DIR/downstream/db
    --socket-path downstream.socket
    --host-addr ${DOWNSTREAM_BIND_ADDR} --port ${DOWNSTREAM_BIND_PORT}"

echo "downstream: $DOWNSTREAM_PEER_CMD"

$DOWNSTREAM_PEER_CMD 1>"$TMP_DIR/downstream.log" 2>"$TMP_DIR/downstream.stderr" &

DOWNSTREAM_PEER_PID=$!

cleanup_node_1() {
    sudo ip netns exec ns2 kill $DOWNSTREAM_PEER_PID
    cleanup_node_0
}
trap cleanup_node_1 EXIT INT TERM

echo "downstream started with PID: $DOWNSTREAM_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

# Marcin Wójtowicz's recommended instrumention for assesing the faithfulness of
# the qdisc-based network emulation and/or the overall behavior of the
# ouroboros-network stack and kernel's TCP logic when handling the
# immdb-server's socket writes
sudo ip netns exec ns1 bash -c "{ while true; do date; ss -i -m -t sport = :3001; sleep 0.1s; done; } 1>${TMP_DIR}/catch-ns1-ss.log 2>&1" &

ss_pid=$!

UPSTREAM_PEER_CMD="sudo ip netns exec ns1 ${IMMDB_SERVER}
    --db $CLUSTER_RUN_DATA/immdb-node/immutable/
    --config $CLUSTER_RUN_DATA/immdb-node/config.json
    --initial-slot $REF_SLOT
    --initial-time $ONSET_OF_REF_SLOT
    --leios-schedule $LEIOS_SCHEDULE
    --leios-db $LEIOS_UPSTREAM_DB_PATH
    --address ${UPSTREAM_BIND_ADDR}
    --port ${UPSTREAM_BIND_PORT}"

echo "upstream: $UPSTREAM_PEER_CMD"

$UPSTREAM_PEER_CMD &> "$TMP_DIR/upstream.log" &

UPSTREAM_PEER_PID=$!

cleanup_immdb() {
    sudo ip netns exec ns1 kill $UPSTREAM_PEER_PID
    sudo ip netns exec ns1 kill $ss_pid
    cleanup_node_1
}
trap cleanup_immdb EXIT INT TERM

echo "upstream started with PID: $UPSTREAM_PEER_PID"

TIMEOUT=${TIMEOUT:-25}
read -t "$(($TIMEOUT + $SECONDS_UNTIL_REF_SLOT))" -n 1 -s -r -p "Press any key to stop the spawned processes, or just wait ~$SECONDS_UNTIL_REF_SLOT + $TIMEOUT seconds..."
echo

echo "Temporary data stored at: $TMP_DIR"

echo "$(date) Tearing down processes $UPSTREAM_PEER_PID (upstream peer, aka immdb-server), $CARDANO_NODE_0_PID (node0), and $DOWNSTREAM_PEER_PID (downstream peer)..."
cleanup_immdb

# Log analysis

echo -e "Send MsgLeiosBlockTxsRequest\tRecv MsgLeiosBlockTxsRequest\tSend MsgLeiosBlockTxs    \tRecv MsgLeiosBlock         \tTxsBytesSize" \
    >$TMP_DIR/LF-req-rsp.txt
paste \
    <(cat $TMP_DIR/node0.log | grep -e Send.BlockTxsRequest | jq -r .at) \
    <(cat $TMP_DIR/upstream.log | grep -e 'Recv MsgLeiosBlockTxsRequest' | cut -d' ' -f1) \
    <(cat $TMP_DIR/upstream.log | grep -e 'Send MsgLeiosBlockTxs' | cut -d' ' -f1) \
    <(cat $TMP_DIR/node0.log | grep -e 'Receive.BlockTxs"' | jq -r .at) \
    <(cat $TMP_DIR/node0.log | grep -e 'Receive.BlockTxs"' | jq -r .data.msg.txsBytesSize) \
    >>$TMP_DIR/LF-req-rsp.txt

ANALYSIS_CMD="python3 ouroboros-consensus/scripts/leios-demo/log_parser.py
        $REF_SLOT $ONSET_OF_REF_SLOT
        ${LEIOS_SCHEDULE}
        $TMP_DIR/node0.log $TMP_DIR/downstream.log
        scatter_plot.png"
echo "analysis: $ANALYSIS_CMD"
$ANALYSIS_CMD

# Status

sleep 1s

echo
echo Any processes still running:
ps -aux | grep -e '[S]TART' -e '[c]ardano-node' -e '[i]mmdb' | cut -c-180

echo "(Hopefully there were none!)"
echo

exit 0
