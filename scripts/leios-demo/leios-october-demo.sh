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

set -x

#
# one veth pair per edge in the topology (TODO one veth pair with a SFQ qdisc for each edge?)
#

cleanup_links() {
    sudo ip link del veth12
    sudo ip link del veth23

    # this handler only runs when the script was interrupted, so no need to
    # update the SIGNAL handlers
}

sudo ip link add veth12 type veth peer name veth21
sudo ip link add veth23 type veth peer name veth32
trap cleanup_links EXIT INT TERM

cleanup_netns() {
    for i in 1 2 3; do
        echo "pids in ns${i}:"
        for pid in $(sudo ip netns pids ns$i); do
            echo -n "$pid "
            ps -aux | grep -e $pid
        done
    done
    for i in 1 2 3; do sudo ip netns del ns$i; done

    # no need to call cleanup_links, since deleting the namespace deletes those
    # links

    # the script directly calls invokes handler, so when teardown is complete,
    # reset the SIGNAL handlers
    trap - EXIT INT TERM
}

for i in 1 2 3; do sudo ip netns add ns$i; done

sudo ip link set veth12 netns ns1
sudo ip link set veth21 netns ns2
sudo ip link set veth23 netns ns2
sudo ip link set veth32 netns ns3

trap cleanup_netns EXIT INT TERM

# adapted from https://unix.stackexchange.com/a/558427
add_addrs() {
    i=$1
    j=$2
    sudo ip -n ns$i link set veth$i$j up
    sudo ip -n ns$j link set veth$j$i up
    sudo ip -n ns$i addr add 10.0.0.$i$j peer 10.0.0.$j$i dev veth$i$j
    sudo ip -n ns$j addr add 10.0.0.$j$i peer 10.0.0.$i$j dev veth$j$i
}

add_addrs 1 2
add_addrs 2 3

sudo ip -n ns3 addr add 10.0.0.33/32 dev veth32   # TODO ?

mydelay="netem delay 100ms"
myrate="tbf rate 20mbit burst 2048b latency 10ms"
add_qdiscs() {
    i=$1
    j=$2
    # I Googled "netem delay after tbf of tbf after netem delay" and the AI
    # Overview implied that I want the following. TODO double-check
    sudo tc -n ns$i qdisc add dev veth$i$j handle 1: root $myrate
    sudo tc -n ns$i qdisc add dev veth$i$j parent 1: handle 2: $mydelay
}

add_qdiscs 1 2
add_qdiscs 2 1
add_qdiscs 2 3
add_qdiscs 3 2

set +x

NS1_ADDR1=10.0.0.12
NS2_ADDR1=10.0.0.12
NS2_ADDR2=10.0.0.21
NS3_ADDR2=10.0.0.32
# TODO an actually mocked downstream peer wouldn't need to bind any addr
NS3_ADDR3=10.0.0.33   # TODO ?

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/leios-october-demo.XXXXXX)
echo "Using temporary directory for DB and logs: $TMP_DIR"

rm -f ./leios-run-tmp-dir
ln -s "$TMP_DIR" ./leios-run-tmp-dir

pushd "$CARDANO_NODE_PATH" > /dev/null

##
## Run cardano-node (node-0)
##

echo "Creating topology-node-0.json in $(pwd)"
cat << EOF > topology-node-0.json
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "${NS1_ADDR1}",
          "port": 5201
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

mkdir -p "$TMP_DIR/node-0/db"

cp "$LEIOS_UPSTREAM_DB_PATH" "$TMP_DIR/node-0/leios.db"
sqlite3 "$TMP_DIR/node-0/leios.db" 'DELETE FROM ebTxs; DELETE FROM txCache; DELETE FROM ebPoints; VACUUM'

CARDANO_NODE_CMD="sudo ip netns exec ns2 env LEIOS_DB_PATH=$TMP_DIR/node-0/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node-0.json
    --database-path $TMP_DIR/node-0/db
    --socket-path node-0.socket
    --host-addr $NS2_ADDR2 --port 5201"

echo "node-0: $CARDANO_NODE_CMD"

$CARDANO_NODE_CMD 1> "$TMP_DIR/cardano-node-0.log" 2>"$TMP_DIR/cardano-node-0.stderr" &

CARDANO_NODE_0_PID=$!

cleanup_node_0() {
    sudo ip netns exec ns2 kill $CARDANO_NODE_0_PID
    cleanup_netns
}

trap cleanup_node_0 EXIT INT TERM

echo "Cardano node 0 started with PID: $CARDANO_NODE_0_PID"

##
## Run a second Cardano-node (To be eventually replaced by a mocked downstream node)
##

cat << EOF > topology-node-1.json
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "${NS3_ADDR2}",
          "port": 5201
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

mkdir -p "$TMP_DIR/node-1/db"

cp "$LEIOS_UPSTREAM_DB_PATH" "$TMP_DIR/node-1/leios.db"
sqlite3 "$TMP_DIR/node-1/leios.db" 'DELETE FROM ebTxs; DELETE FROM txCache; DELETE FROM ebPoints; VACUUM;'

DOWNSTREAM_PEER_CMD="sudo ip netns exe ns3 env LEIOS_DB_PATH=$TMP_DIR/node-1/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node-1.json
    --database-path $TMP_DIR/node-1/db
    --socket-path node-1.socket
    --host-addr ${NS3_ADDR3} --port 5201"

echo "downstream: $DOWNSTREAM_PEER_CMD"

$DOWNSTREAM_PEER_CMD 1>"$TMP_DIR/cardano-node-1.log" 2>"$TMP_DIR/cardano-node-1.stderr" &

DOWNSTREAM_PEER_PID=$!

cleanup_node_1() {
    sudo ip netns exec ns2 kill $DOWNSTREAM_PEER_PID
    cleanup_node_0
}

trap cleanup_node_1 EXIT INT TERM

echo "Cardano node 1 started with PID: $DOWNSTREAM_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

UPSTREAM_PEER_CMD="sudo ip netns exec ns1 ${IMMDB_SERVER}
    --db $CLUSTER_RUN_DATA/immdb-node/immutable/
    --config $CLUSTER_RUN_DATA/immdb-node/config.json
    --initial-slot $REF_SLOT
    --initial-time $ONSET_OF_REF_SLOT
    --leios-schedule $LEIOS_SCHEDULE
    --leios-db $LEIOS_UPSTREAM_DB_PATH
    --address ${NS1_ADDR1}
    --port 5201"

echo "upstream: $UPSTREAM_PEER_CMD"

$UPSTREAM_PEER_CMD &> "$TMP_DIR/immdb-server.log" &

UPSTREAM_PEER_PID=$!

cleanup_immdb() {
    sudo ip netns exec ns1 kill $UPSTREAM_PEER_PID
    cleanup_node_1
}

trap cleanup_immdb EXIT INT TERM

echo "ImmDB server started with PID: $UPSTREAM_PEER_PID"

TIMEOUT=${TIMEOUT:-25}
read -t "$(($TIMEOUT + $SECONDS_UNTIL_REF_SLOT))" -n 1 -s -r -p "Press any key to stop the spawned processes, or just wait ~$SECONDS_UNTIL_REF_SLOT + $TIMEOUT seconds..."
echo

echo "Temporary data stored at: $TMP_DIR"

echo "$(date) Tearing down processes $UPSTREAM_PEER_PID (upstream peer, aka immdb-server), $CARDANO_NODE_0_PID (node-0), and $DOWNSTREAM_PEER_PID (downstream peer, aka node-1)..."
cleanup_immdb

# Log analysis

cat $TMP_DIR/cardano-node-0.log >logA
#cat $TMP_DIR/cardano-node-1.log >logB
cat $TMP_DIR/cardano-node-0.log >logB

python3 ouroboros-consensus/scripts/leios-demo/log_parser.py $REF_SLOT $ONSET_OF_REF_SLOT logA logB "scatter_plot.png"

# Status

sleep 1s

echo
echo Any processes still running:
ps -aux | grep -e '[S]TART' -e '[c]ardano-node' -e '[i]mmdb' | cut -c-180

echo "(Hopefully there were none!)"
echo

exit 0
