# Leios Valid Claims at Startup

Part of: [System Overview](index.md)

## Problem

A CertRB already in the VolatileDB at startup never passes through `chainSelAddBlock` again — `isMember` short-circuits it and BlockFetch won't re-fetch it — so nothing hands it the ledger view its certificate must be checked against.
Its claim stays unverified for the run, which closes the Recovery Path for the EB it certifies; where that is the only remaining route to that EB, the block can never become selectable.

## Policy

At startup, a CertRB we can't vouch for is treated as a block we don't have; we re-acquire it by the ordinary path, which carries the view as a matter of course.
Concretely: **forget every CertRB that isn't on the selection after initial chain selection.**

Forgetting means *unknown but still stored*.
A forgotten block is absent to every query — whether we hold it, whether it's fetched, what its predecessor's successors are, and reads by hash.
Its bytes stay, and the store still recognises the hash if the block is added again: that add writes nothing and simply starts admitting the block.
The adding caller sees an ordinary successful add either way.

## Mechanism

The VolatileDB gains an in-memory set of forgotten hashes.
It is empty when the VolatileDB is opened, and stays empty until something calls the forget operation — which the ChainDB can only do once initial chain selection has produced a selection to keep.

- `getBlockInfo`, `filterByPredecessor` and `getBlockComponent` treat a hash in that set as absent.
  Everything derived from them inherits it: `getIsMember`, `ChainDB.getIsFetched`, and `chainSelAddBlock`'s `isMember`.
- The index entry itself is untouched, so re-admission restores exactly the original file and offset.
- `putBlockImpl`'s existing dedup is the one exception — it consults `currentRevMap` directly, so it still finds the block, writes nothing, and removes the hash from the forgotten set.
- A new VolatileDB operation takes the CertRBs to keep and forgets every other cert-carrying block in its index.
  The ChainDB calls it once, after initial chain selection, passing the CertRBs on the resulting selection.
  The VolatileDB thus needs no notion of a selection, and the ChainDB needs no way to enumerate the index.
- Two trace points: the batch forgotten at startup, and each re-add that re-admits one.
  The second is what tells you the path fired.

## Correctness

**Liveness — a forgotten CertRB C is recovered exactly when it is wanted.**
If C is on a chain we would select, some current peer's candidate runs through it, so ChainSync delivers C's header and its ancestors', validated incrementally from our intersection — no forecast horizon arises, because the header state advances header by header.
`getIsFetched` reports C absent, so BlockFetch requests it in that candidate's range.
`chainSelAddBlock` reads `isMember` before calling `putBlock`, so it sees absent and proceeds; `putBlock` writes nothing and re-admits C; `precheckLeiosCert` then runs with the `Predecessor` BlockFetch derived from C's own validated header, which is the ledger view at C's predecessor's slot — the announcing slot, and so exactly the view the certificate must satisfy.
Conversely, if no peer ever offers C again, no chain we would select contains C, so C is unselectable and its claim is worthless.

**Progress.**
Forgetting happens once, at startup; re-admission is permanent.
The set only shrinks during a run, so there is no cycle between the two.

**Safety.**
A node need not preserve everything across a restart; it must not lose *too much*, and the measure of "too much" is what recovery costs.
Resyncing from genesis is always correct and always intolerable downtime.
This policy sits near the other end: it leaves the entire selected chain untouched, and what it discards is bounded by the VolatileDB's off-selection contents — normally a handful of fork blocks.
Each is recoverable from the network by the ordinary fetch path, and only fetched when some chain we would actually select needs it.
So the information lost is bounded, and its recovery is both cheap and demand-driven.
A block we forged is covered by this like any other and needs no separate treatment.

What remains are three invariants the mechanism must uphold regardless, because breaking them damages the running node rather than merely costing a re-fetch:

- **Nothing on the selection is forgotten.**
  Otherwise `filterByPredecessor` loses an edge of the chain whose headers `cdbChain` still holds, and `copyToImmutableDB` throws when `getKnownBlockComponent` can't find a block it is copying.
- **No second copy reaches disk.**
  `putBlockImpl` dedups against `currentRevMap`, which retains the entry, so a re-add appends nothing.
  The duplicate that would make `validateFile` hit `DuplicatedBlock` and truncate a file at the next startup is never created.
- **No reader holds a forgotten hash.**
  Every by-hash read takes its hash from the index or the selection, so making forgotten blocks absent to reads strands nobody.
  Low risk, and contained: no consumer of the node's interfaces (which excludes direct access of its on-disk files) can learn a forgotten block's hash, so none can request one.
  The exposure is confined to ChainDB-internal readers — a sweep that is not yet finished.

**Downstream consumers.**
DBImmutaliser is the only interesting one: it opens the VolatileDB and nothing more, so it never calls the forget operation and the set stays empty for it — it sees the whole index, unchanged.
Any other tool that opens the VolatileDB directly is in the same position.

## Alternative Designs

### Forget by deleting

This design would isolate the changes to the VolDB to all happen at startup.
However, it also introduces a non-trivial concept to the VolDB: mutation.

In particular, the forgotten state could be eliminated by *actually* forgetting — deleting those blocks from disk, so that "we do not have it" needs no new semantics at all.
That would introduce non-append mutation to the VolatileDB's design, which is not a small step.
The affected files would have to be rewritten, and the durability that doing so would need is deliberately out of reach — `fsync` is _intentionally_ so far not even offered by the filesystem API the VolatileDB is implemented against.
That's because `fsync` is unacceptable during the node's normal behavior.
On the other hand, `fsync` would tolerable as part of startup, but nothing else has justified adding it to the interfaces.

### Leverage LeiosDB

This design was rejected for the following reason.

We could properly persist each verified Leios cert — in the LeiosDb, say — and load `ValidClaims` from it at startup, so that the verdict survives the restart and no CertRB need be forgotten.
However, that cannot replace the forgotten state, because correctness would require the persisted claim to be durable before the block is: `chainSelAddBlock` would have to verify the cert _and persist it_ before `putBlock`, putting a synchronous SQLite commit on the block-add path, once per new claim — contending with exactly the EB and closure traffic that is already requiring some redesign of the LeiosDb, since it's bottlenecking some devnets.

Notes:
- Switching to this design would also require some bespoke migration logic for a node that upgrades to it, since its first subsequent restart couldn't populate `ValidClaims` from the _new_ persistent store.
  That migration logic would be comparable to the startup scan that initialized the forgotten state.
- Demoting this design to an optimization layered on top of the forgotten state would free its writes to be lazy and best-effort, but then it merely avoids only the CertRB re-fetches that arise from forgetting them.

### React to MsgRollForward

This design was implemented and then reverted, so we have some concrete insights into it.

A CertRB's header is revalidated by every peer whose candidate chain runs through it, and a validated header carries the ledger view at its predecessor's slot.
That is the announcing slot, and so exactly the view the claimed certificate must satisfy.
The ChainSync client therefore can hand that view to the ChainDB, which could read the block _it already holds_ and verify the certificate.
However, `verifyLeiosCert` costs milliseconds, so that verification cannot run on the ChainSync client's thread: it needs a work queue and a background thread to drain it, plus a new edge from the network layer into the ChainDB's internals to fill the queue.

That concurrency was the most painful part of the implementation, and none of it is incidental.
Every peer's client discovers the same work at about the same time, so the producers have to deduplicate — keying the queue by CertRB is what collapses tens of identical items into one.
The queue consumer's lifetime has to be the node's rather than a peer's: anything simpler, such as letting one client do the work while the others skip it, silently loses the job when that client's peer disconnects mid-verification, because the other clients that already skipped it don't get a second chance.
The forgotten state introduces none of this: it reaches the same verified claim through the ordinary fetch-and-add path, which already carries that view, already deduplicates across peers in BlockFetch, and already runs off the ChainSync threads — in exchange for re-fetching a handful of off-selection blocks.

Notes:
- The reverted work included a benchmark that measures `verifyLeiosCert` at roughly 0.9ms plus 1.9us per signer — about 2ms for a committee of 1000 and 9ms for one of 5000.
  Those would be significant additions to the established costs of header validation.
- This design does cover one corner case the forgotten state does not: a candidate chain whose blocks BlockFetch never fetches, where the header alone would settle the claim (since it triggers reading the previously-fetched CertRB from disk).
  That only matters for acquiring - via The Recovery Path — an EB certified by a CertRB even before we want to select it, so the loss is merely latency, but not on the scale that the The Recovery Path should prioritize.
