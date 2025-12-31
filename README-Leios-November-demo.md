# Introduction

This file is the successor to https://github.com/IntersectMBO/ouroboros-consensus/blob/leios-202510-demo/README-Leios-October-demo.md.
It summarize the findings and progress made during November 2025 on the Leios networking prototype.

- The surprisingly-high latency observed in the October demo was explained and reined in.
- Key structured log events were added to the prototype.
- Observability/reporting/monitoring was improved.
- Packaging of the prerequisites for executing the demo was improved.

# Bufferbloat

The investigation into the unexpectedly high latency seen in October and related refinements to the prototype are apparent in the asynchronous conversation that took place in the comments on this tracking Issue https://github.com/IntersectMBO/ouroboros-consensus/issues/1756.

- The latency was due to https://www.bufferbloat.net.
  In October, the bufferbloat arose directly from the naive use of [Toxiproxy](https://github.com/Shopify/toxiproxy) for the initial demo.
- As user-space mechanism, Toxiproxy cannot introduce latency/rate/etc in a way that will influence the kernel algorithms managing the TCP stream.
- [Linux Traffic Control](https://tldp.org/HOWTO/Traffic-Control-HOWTO/intro.html) is the approriate mechanism.
- An example of relevant commands for a more appropriate WAN (Wide Area Network) emulation can be found in this GitHub comment https://github.com/IntersectMBO/ouroboros-consensus/issues/1756#issuecomment-3587268042.
    - `htb rate 100mbt` limts the sender's bandwidth.
    - `fq_codel` paces the sender's traffic, adapting to any bottleneck between it and the recipient.
    - `netem delay` established the link latency of 20ms between `fq_codel` and the recipient.
- The Networking Team is now taking over this aspect of the setup for future Leios demos, refining//enriching the WAN emulation, preparing some testing on actual WANs (physically separated machines), and considering which mechniams ought to mitigate the risk of Leios-induced bufferbloat (perhaps as part of an ATK-LeiosProtocolProtocolBurst) increasing the latency of Praos traffic once Leios is deployed on mainnet.

# Improved Logging

Additional key events in both the mocked upstream peer and the node-under-test are now emitted as structured JSON, which the demo's analysis script processes.
Highlights include the following.

- The node reliably indicates when concludes it acquired the last of the txs it was missing from an EB.
  In particular, this event is raised then a MsgLeiosBlockTx arrives with all the txs that the node was missing from some EB.
  Even if the final tx were to arrive as part of a separate EB, this event would still be emitted for each EB that the MsgLeiosBlockTx completes.
- Both the node and upstream peer report when ChainSync's MsgRollForward, BlockFetch's MsgRequestRange and MsgBlock, and Leios's MsgLeiosBlockRequest, MsgLeiosBlock, MsgLeiosBlockTxsRequest, and MsgLeiosBlockTxs are sent and received.
  The demo's analysis script displays a table of when these messages were sent and received.
  This table very usefully indicates how much to alter the timings in the `demoSchedule.json` file in order to change which parts of the Praos traffic a particular EB's exchange overlaps with.
- A patch to `ouroboros-network` was introduced to allow two additional timings, which will are expected to help make subsequent visualizations of the message exchange more accurate.
    - When a mini protocol begins trying to enqueue a message in the mux, even if the mux is unable to accept that message immediately.
    - When the demux receives the last byte of some message, even if the mini protocol doesn't attempt to decode that message immediately.
- The `ss` tool is being used to sample socket statistics throughout the demo's execution, so that the TCP algorithm's state can be monitored.
  For example, the `rtt` and `notsent` fields are directly related to bufferbloat.

# Packaging and Monitoring with Grafana

See [this README](https://github.com/input-output-hk/ouroboros-leios/tree/main/demo/2025-11)


# Building from Source

Contributers who want to build the demo from source will need to packages in the three repositories on these commits.
No other packages have yet been patched for this demo, the appropriate versions are those used in the 10.5.1 build.
Beware that the listed commits do not already include `source-repository-package` stanzas in their `cabal.project` files, if that's
the contributor's chosen method for cross-repo dependencies.

```
$ for i in ouroboros-consensus ouroboros-network cardano-node; do (cd $i; echo REPO $i; git log -1); done
REPO ouroboros-consensus
commit 7929c3716a18abb852f8abec7111c78f2059287e (HEAD -> nfrisby/leios-202511-demo, origin/nfrisby/leios-202511-demo)
Author: Nicolas Frisby <nick.frisby@iohk.io>
Date:   Thu Nov 27 12:57:43 2025 -0800

    leiosdemo202511: polishing per-message table format
REPO ouroboros-network
commit 479f0d0d82413162c8444b912394dd74c052831f (HEAD -> nfrisby/leios-202511-demo, tag: leios-202511-demo, origin/nfrisby/leios-202511-demo)
Author: Nicolas Frisby <nick.frisby@iohk.io>
Date:   Thu Nov 27 10:49:49 2025 -0800

    leiosdemo202511: introduce BearerBytes class
REPO cardano-node
commit 93d2c8481912309faf5a7d9058f9fdeca95710a0 (HEAD -> nfrisby/leios-202511-demo, origin/nfrisby/leios-202511-demo)
Author: Nicolas Frisby <nick.frisby@iohk.io>
Date:   Thu Nov 27 11:02:11 2025 -0800

    leiosdemo202511: integrate ouroboros-network BearerBytes
```
