# LeiosFetch specification

The LeiosFetch mini protocol is how upstream nodes serve Leios's Endorser Blocks (EBs) to downstream nodes.
It's a multi-step process, which begins with information received via the LeiosNotify mini protocol.

## LeiosNotify triggers LeiosFetch

For each election, LeiosNotify delivers announcements of that election to a node.
Each announcement specifies the full hash and byte size of an EB _body_.
An adversarial stake pool can send multiple announcements for a single election, which is called _equivocation_.

Announcements can also arrive via ChainSync, which is the case for syncing nodes and for the Recovery Path (see LeiosFetchRecoveryPath below).

LeiosFetch prevents equivocation from inducing unbounded work (eg fetching multiple EBs), because LeiosFetch ignores all announcements except the first it receives for each election.
(TODO that's only true as of https://github.com/input-output-hk/ouroboros-leios/issues/1069)
One clarification to avoid confusion: LeiosNotify doesn't ignore all but the first---so that equivocation proofs can spread through the network, for example---but LeiosFetch only uses the first announcement, regardless of whether it arrived via ChainSync or LeiosNotify.

LeiosNotify also allows the node's _direct_ upstream peers to indicate when they have the EB body and so can begin to serve it to this node, MsgLeiosBlockOffer.
It also allows MsgLeiosBlockTxsOffer to indicate that it's _additionally_ ready to serve the transactions that that EB body references.

## When to send MsgLeiosBlockRequest

Once a node has processed both an announcement and some offers for the body of that announcement's EB, it will send a MsgLeiosBlockRequest to the peers that have offered that EB body.
(TODO the announcement isn't required yet, but will be as of https://github.com/input-output-hk/ouroboros-leios/issues/1082.)
The following decisions are made by `LeiosDemoLogic.leiosFetchLogicIteration`.

- Definition of __FullBodyHedging__: the node sends a request to every peer that has offered an EB body the node wants (ie the first announced) and does not already have, regardless of whether any other requests for that EB body that are already inflight.
- Definition of __BoundedInflightBytes__: the only thing that might prevent a node from sending a request to a peer who has offered an EB body the node wants and doesn't already have is if either this request was already sent to that peer or if the node already has too many bytes inflight with that peer.
  There are two limits.
    - For BigLedgerPeers, the limit is 5×12 mebibytes = 60 mebibytes (note that that's also what the maximum LeiosFetch ingress buffer is set to slightly more than) (TODO is this enough? Especially during a ProtocolBurstAttack?).
    - For all other peers, the limit is 5 megabytes.
    - (TODO these should be configurable and/or adaptive to changes in the protocol parameter that limits the cumulative size of transactions referenced by an EB.)
    - (TODO it's confusing and maybe even wrong that one uses base 10 and the other base 2)

Prioritization of which requests to send is discussed below, see LeiosFetchPrioritization.

## How to process MsgLeiosBlock

When a MsgLeiosBlock arrives, the LeiosFetch logic processes it; see `LeiosDemoLogic.processLeiosBlock`.
If the received body has a hash or size that doesn't match the announcement or it includes some transaction more than once, it's discarded as invalid and the node disconnects from the peer who sent it.
(TODO the _untrustworthy_ (!) offer's size is used rather than the announcement's until https://github.com/input-output-hk/ouroboros-leios/issues/1082.)
If the received body had already arrived before during this execution of the node, processing is merely bookkeeping and tracing.
Otherwise, the node determines which transactions the body references that it needs to fetch.

There are two ways the node could determine that it doesn't need to fetch one of the reference transactions.

- Definition of __LeiosFetchFromTxCache__: if the referenced transaction is in the LeiosTxCacheIndex, then that transaction is already currently in the LeiosDB.
- Definition of __LeiosFetchFromMempool__: if the referenced transaction is in the Mempool, the node simply copies it to LeiosDB and inserts it into LeiosTxCacheIndex.
- (If a transaction is in both the LeiosTxCacheIndex and the Mempool, then it's just the LeiosFetchFromTxCache path plus a little more bookkeeping.)

All referenced transactions that still need to be fetched are then assembled into "jobs".

- Definition of __LeiosFetchJob__: one set of transactions that LeiosFetch determined it needs to fetch when processing an EB body; no more than 64 kibibytes total per job.
  (Note that this choice assumes a single transaction's size can't exceed 64 KiB in the near future.)
  The job data structure doesn't contain the transaction itself (those are the bytes we need to fetch!) but it contains enough information to send a sufficient MsgLeiosBlockTxsRequest and to check that the eventual corresponding MsgLeiosBlockTxs reply actually contains the requested transactions.

## When to send MsgLeiosBlockTxsRequest

The following decisions are also made by `LeiosDemoLogic.leiosFetchLogicIteration`.

- Definition of __FullClosureHedging__: the node sends a request to every peer that has offered an EB closure the node has outstanding jobs for, regardless of whether any other requests for those jobs are already inflight.
- Definition of __LeiosFetchFromPeer__: whenever BoundedInflightBytes allows for more requests, the node will request some of the outstanding jobs from a peer who offered them (and hasn't already been sent requests for those exact jobs).
  The node will choose to request jobs that are already inflight with the fewest peers (aka "RarestFirst").
  (Note that any disconnection must involve some bookkeeping so that the rarity counts remain accurate.)
  The order of the jobs with the minimum-rarity is chosen randomly.
  Jobs are picked one at a time in that way until the peer's BoundedInflightBytes bound is met.
  Recall that a BigLedgerPeer's inflight allowance is much greater than a single EB's closure, so an idle BigLedgerPeer will request _all_ jobs for an EB, regardless of their current inflight rarity (TODO Leverage that to optimize the decision logic somehow? Seems unlikely to matter) (TODO but maybe its effect on the order in which the jobs are requested from the BigLedgerPeer matters?).

When BoundedInflightBytes allows requesting multiple jobs from a single peer, they'll be combined into a set of MsgLeiosBlockTxsRequest messages.
There may be more jobs than messages, depending on how the jobs' sizes relate to the configured maximum message size (TODO it's 500 kB at the moment, but should be configurable and/or definite in terms of some protocol parameter).
There might be more than one job per MsgLeiosBlockTxsRequest, but each job will fit entirely within one MsgLeiosBlockTxsRequest (for this peer).

Prioritization of which requests to send is discussed below, see LeiosFetchPrioritization.

TODO if a job request is sent much later than its originating body arrived, it's possible the LeiosTxCache has meaningfully evolved in the meantime.
So, when a job is sent, perhaps a second filter against the _current_ LeiosTxCacheIndex would be worthwhile.
Note that, for the sake of eviction, the LeiosTxCacheIndex retains its EB bodies in memory.
That happens to be the data necessary for resolving a job's bitfield/set of offsets into the set of transaction hashes that need to be checked for in the LeiosTxCache.
Which is to say: the existing LeiosTxCacheIndex could already allow for offset-based checks.

TODO the above rules do not increase the probability that the jobs that arrive first deliver the "next" tx that is missing from the EB.
Increasing that probability would make it more likely that the node's LeiosVoting thread could start/progress its validation work (which must process the txs in the same order as the EB body specifies) even before the whole EB closure has arrived.
However, that job ordering bias is in tension with the goal of having different jobs in flight with different peers: RarestFirst ensures we utilize our (honest) peers' cumulative bandwidth as efficiently as possible (so that we have the whole EB _before_ the last reply arrives).
On the other hand, with RarestFirst, an adversarial peer can notice the request it received was one of the early jobs, and delay its response, which means the LeiosVoting thread will be starved of work even as other (non-"next") jobs arrives.
One possibility, for example, might be to give jobs a "rarity boost" if those jobs are closer to the front of the EB, so that _RarestFirst doesn't prevent (some) hedging of the leftmost jobs of an EB_.
Another possibility would be to piggy back on __LeiosFetchClosureStreaming__: always pick earlier segment's jobs, only applying RarestFirst _within_ segments.

## How to process MsgLeiosBlockTxs

When a MsgLeiosBlockTxs arrives, the LeiosFetch logic processes it, see `LeiosDemoLogic.processLeiosBlockTxs`.
The following checks are done (in order) for each job that MsgLeiosBlockTxs would discharge.
- If the received transactions don't have the exact total count and cumulative size that matches the request that was sent, the message is discarded as invalid and the node disconnects from the peer who sent it.
- If a reply for this same job had already arrived before during this execution of the node, processing is merely bookkeeping and tracing.
- If the received transactions don't have the exact order, hashes, and sizes that match the request that was sent, the message is discarded as invalid and the node disconnects from the peer who sent it.
  (Note that each job doesn't need to store the individual transactions' hashes, but rather the hash of those hashes suffices; this reduces the memory footprint of jobs.)
If none of those checks are triggered for the job, then the node inserts its transactions into the LeiosDB and then the LeiosTxCacheIndex.

## Request Prioritization

The following rule does not distinguish between EB bodies and EB closures.

- Definition of __LeiosFetchPrioritization__: the node implements a refinement of the core FreshestFirst idea.
    - If two EBs are both older than L (for L = 3 × L_hdr + L_vote + L_diff), then the younger EB is higher priority.
    - If one EBs is older than L and the other is younger than L, then the younger EB is higher priority.
    - If two EBs are both younger than L, then the older EB is higher priority.
    - If the node doesn't know the current slot---for example, because it's syncing---then it can't compare the EBs' ages to L.
      In that case, the older EB has the higher priority, since that's suitable for syncing nodes.

EBs with the same slot use their hash as an "arbitrary" tiebreaker.
(TODO this tiebreaker should instead be totally random, unlike the Praos tiebreaker.)

Note that this prioritization only happens per-peer.
Different peers do not affect each other's prioritization, except through the RarestFirst job prioritization.
In particular, there's no global budget that one peer could monopolize.

## Recovery Path

- Definition of __LeiosFetchRecoveryPath__: if the network violates the L_hdr protocol parameter, then the node might have fetched a different EB for some election than the EB that gets certified for that same election.
  In this case, the node must fetch a second EB for that election, but only after it has validated a certificate for that second EB's announcement.
  (TODO https://github.com/input-output-hk/ouroboros-leios/issues/1071 makes it possible to validate the cert in a CertRB before having fetched that EB)
  (TODO https://github.com/input-output-hk/ouroboros-leios/issues/1070 re-introduces the ability to fetch a second (and only a second!) EB, once https://github.com/input-output-hk/ouroboros-leios/issues/1069 limits the node to just one per election)

The Recovery Path relies on the fact that ChainSync's MsgRollForward also provides announcements as a side-effect and also provides offers as a side-effect of serving a header whose "cert bit" is set.

Also, the Recovery Path alone suffices for syncing nodes, at least in the MVP release.

## Node Initialization Rules

At startup, the node merely checks for which EB's it already has both the body _and_ the closure.
For those EBs, it will not do any additional fetching.

But for any EBs it hadn't _fully_ acquired the closure of before most recently shutting down, it will fully re-fetch them if any peers announce and offer them (which is only likely via the Recovery Path).
In particular, the LeiosTxCacheIndex is always completely empty when the node initializes.

## Locally-Forged EBs

The node has a few special cases to prevent LeiosFetch from fetching an EB that this node forged.

## Memory and CPU usage

The overall LeiosFetch design (as well as LeiosNotify, LeiosTxCache, etc) crucially admits efficient pruning of the necessary bookkeeping state (driven at least by each advancement of the immutable tip, and sometimes even more aggressively) and also accommodates the worst-case amount of unpruned data without risking using an untenable amount of RAM.
An argument based on the Poisson Binomial and the limiting case of an infinitely flat stake distribution stochastically bounds that worst-case---even allowing for grinding---to no more than 10,000 elections between the immutable tip and the wall-clock.

A couple key examples:

- LeiosTxCacheIndex evicts transactions as soon as they're not referenced by any of the latest 128 announcements.
  Because each EB body can reference at most ~14,000 transactions, that ensures the LeiosTxCacheIndex never has to track more than ~2 million transactions at once.
- If a peer sends an announcement from too far in the future, the node discards it and disconnects.

## How to respond to MsgLeiosBlockRequest and MsgLeiosBlockTxsRequest

As a server, the node should disconnect if a downstream peer requests something the node doesn't have.
This constraint is justifiable, because the node also eagerly _offers_ what it does have to downstream peers.

Beyond that check, the node simply has to enforce some basic static bounds and then retrieve the requested data from the LeiosDB and then serialize it to the wire.
(TODO denormalizing the schema will make this retrieval less expensive.)

TODO see the "edit:" in Issue https://github.com/input-output-hk/ouroboros-leios/issues/1074

## TODO Why so much hedging?

The FullBodyHedging and FullClosureHedging rules could induce a great deal of redundant ingress for this node and egress for its upstream peers.
However, we don't see any cheaper alternative that is able to resist the adversarial upstream peer who offers something but then serves it to us very slowly; much slower than other peers would have.
It's the challenge of _tail latency_ despite the possibility of having a large number of adversarial upstream peers---if we (unknowingly) allocate our requests to them _instead_ of to our honest peers, then it's as if we're temporarily eclipsed.
We are technically connected to some honest peers, but if we're not utilizing _those_ connections, then they can't contribute to a bound of our tail latency.

It may be sound to hedge less aggressively at the start of an EB's diffusion and then more as that EB's age increases, but we haven't yet had any simulations in place that would assist us in tuning that time-varying hedging can without weakening the degree to which the T22 attack vector is mitigated (that'd be tuning and mitigating at `mainnet` scale, which dwarfs devnet/testnet).
On the other hand, the tremendous capacity for wasted egress that full hedging introduces almost surely _itself_ weakens the T22 argument by overloading honest servers' (burst) egress capacity.

There are a few mechanisms we've considered that might help; it's work in progress to use these or something we haven't yet considered/found to balance wasted egress versus tail latency.

- An _egress scheduler_ allows the node to serve some peers while ignoring others.
  The key benefit is that the first-served peers joined the set of nodes that are able to service requests---hence it's better for the network overall than the kernel's default behavior of spreading egress uniformly among all downstream peers.
  Because it improves the "infectious spread", this component seems unavoidable.
  However, it does directly introduce a cause of possibly-very-significant delays before an honest server replies to an honest peer, so some network-/topology-level argument amendment will be needed.
    - TODO How should the egress scheduler prioritize among its downstream peers' requests?
        - prioritize according to past performance _as an upstream_, ie mutual/bidirectional connections, so that "a node is (likely to be) served well if it has served well"---remains susceptible to instantaneous betrayals and perhaps ignores brief betrayals.
	- prioritize according to protocol-scheduled connections---but they're totally ignorant of how performant that peer-to-peer connection will be/has been, so fail to utilize the SPO's provisioned hardware as well as they could be.
- If the egress scheduler starves a peer for "too long" (TODO need a concrete trigger specification), it can send MsgDropped instead of ever sending a proper reply.
  Hopefully, the peer's (hedged) request will have already been served by a different node by the time it receives MsgDropped.
  If not, it could resubmit the request.
    - TODO Perhaps MsgDropped should have an even stronger semantics, not limited to a single request... something more like MsgChoke, a la BitTorrent's `choke` and `unchoke` messages (beware: Claude Fable warns the benefits don't translate from there to here---I don't understand that well enough)?
- And MsgTryCancel would be a client-side analog of MsgDropped: the downstream peer can notify the server that it already received a reply for that request from a different peer.
  If the server receives MsgTryCancel before it has written the identified request's reply to the socket already, then it can eliminate that egress by sending (the tiny) MsgDropped instead (sooner than it would have without this client-supplied information).

The only other relief would be to reduce the maximum closure size---ie, just accept the _relative_ waste, but decrease its corresponding _absolute_ waste enough to recovery a non-naive T22 mitigation.
We'd still need something like the above ideas in order to justify subsequently re-increasing the maximum closure size to its originally intended magnitudes.

## TODO Streaming closures

There is one low-hanging fruit that would easily increase the network's effective egress capacity: pipelining the diffusion of closures.

- Definition of __LeiosFetchClosureStreaming__: The necessary changes are slight: each job is associated with one segment its EB's closure, according to fixed and objective segmentation, and the MsgLeiosBlockTxsOffer message is enriched to identify that segment.
  Thus, as soon as a node finishing fetching some segment, it can begin serving it, even while it's still fetching the other segments.

The LeiosFetch decision logic and the LeiosNotify events change just as slightly.
LeiosFetch merely partitions an EB's jobs by segment, and LeiosNotify needs to send one offer per segment instead of one per EB.

EB bodies could be pipelined similarly; however, the power-to-weight ratio of doing so is much worse.
First: ultimately, bodies should be much smaller than closures, so there's less latency to hide here.
Second: the structure would require the announcement carry a Merkle hash root over the segments' hashes instead of simply the body's hash.
And so MsgLeiosBlock would carry a segment of the body and the hash of all the other segments.
The segments' sizes could likely be fixed and objective, just as for closures.
As long as there aren't soo many segments of a body (and there shouldn't be), the extra hashing CPU cost is negligible.
It's merely a matter of code/design complexity.
