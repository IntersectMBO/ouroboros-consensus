# Leios single-node fetch — reference spec (prose)

**Status:** draft for review (prose §1–§4; §6/§7 appendices). Written autonomously; anticipated additions in §5.

**Scope.** A single Leios node's logic, centered on the LeiosFetch decision logic, cast
wide enough to cover the single-node subsystem managing multiple, possibly-adversarial,
coming-and-going peers. Stops just above the mini-protocol codecs/multiplexing; does not
model the diffusion layer's churn decisions (churn arrives as events). Deliberately
diverges from the implementation; the implementation will match this design eventually.

**Conventions.** `EB` = endorser block (point: slot + hash). A closure of an EB = all txs
its body references — by `TxHash`, the hash of the *entire* tx (we avoid `TxId`, which
connotes hashing only the tx body). A job = a batch of an EB's txs of ≈ `jobSize` bytes — the unit of tx
fetching. An election = `(slot, stake-pool id)`. EBs are identified four ways, by need: `EbHash` (the hash —
content identity, for body/tx fetch and offers); `EbRealPoint = (slot, EbHash)` (adds the slot, for
slot-indexed location/GC); `Election = (slot, pool)`; and `HeaderHash` (an announcement's identity — and a
cert's identity, since a cert binds to its `RbHeader`, not merely the `EbHash` it names). An announcement is
an `RbHeader` (it announces an EB and declares its body and
closure sizes); `HeaderHash` is its hash, and the whole header travels on the wire and is validated
(signature/KES, etc.) on receipt — `ebHash`, `bodySize`, and `closureSize` are read from the validated
header. `election(header)` reads an announcement's election from its header; a bare `EbHash` does not
determine its `Election` (the pool lives only in the header), so recovering it is a lookup — needed only
when ingesting an offer (`electionOf`, §2). Two announcements with the same election but
different headers are an equivocation. A certificate (cert) certifies one EB per election (the
certified EB); under honest-majority stake distribution and anti-equivocation voting rules at most one EB per election is
certifiable. Peers are upstream and classified (`BEH-PeerClass`). Sizes in bytes. "Stored"/"persisted"
throughout means a write is readable by this process's other components (intra-process visibility) — what the
consumer-facing fan-out waits for, not ACID "durable". Behaviors
are named (PascalCase code names) rather than numbered, and grouped by mini-protocol:
**LeiosNotify** (announcements, offers, certs, votes), **LeiosFetch** (transferring bodies,
jobs, certs), **startup/shutdown** (the state that must survive a restart), and **everything else** (cross-cutting).

**Baseline vs skew fallback.** The baseline assumes timing is benign: clock skew and `L_hdr` hold
well enough that the certified EB for an election is always the one a node saw announced first (an
equivocated election simply never certifies). The cert-handling machinery beyond that — the skew
fallback, tagged **[SF]** — is split by how much complexity it adds. A *minimal*
part is sprinkled occasionally through the main spec, just enough to enable a fully syncing or briefly-offline node, even under benign timing. The *full*
part is collected in the §6 appendix; it treats certs as first-class Leios messages, which can reduce the latency and increase the thoroughness of the minimal machinery. Read the main spec first; its inline
**[SF]** tags point to §6 wherever the main spec already includes some minor extraneous parts merely to leave room for the full SF mechanisms.

---

## §1 Behaviors to cover

### LeiosNotify

- **`BEH-Wanting`** (client; announcement-gated, anti-equivocation). `LevBlockAnnouncement(peer, header)`: a peer
  relays the `RbHeader` announcing that EB `ebHash` exists for its election and declaring its body and closure
  sizes. We validate the whole header (signature/KES, etc.) on receipt — hence it travels in full, not just
  `ebHash`/sizes. A LeiosNotify header must actually announce an EB; one that announces none is a junk
  LeiosNotify message — disconnect. (Contrast ChainSync: a `LevRollForward` header that announces no EB is a
  normal block, no disconnect — it just has no announcement effect.) Centrally we keep the first EB announced per election;
  an announcement naming a different EB for the same election is an equivocation, kept once as
  proof but never wanted. An EB is wanted iff it is that first-announced EB for some election (and not yet
  complete, nor below the immutable tip). This bounds the want-set by the number of elections
  (sortition-rate-limited), not by adversary offers. Per upstream peer we also bound the
  announcement stream: at most two announcements per election — the first, then at most one
  equivocation (different-header); a duplicate or
  a third is misbehavior — disconnect. All sizes come
  from announcements — known up front, before any body/tx arrives, enabling fetch budgeting and
  size-validation (`BEH-Responses`). A validated cert (ChainSel's `LevCertValidated`)
  also makes its EB wanted; **[SF]** when it names an EB other than our first-seen, the want
  switches to the certified EB — skew fallback (§6).

- **`BEH-Offers`** (client; availability). `LevBlockOffer(peer, ebHash)` / `LevBlockTxsOffer(peer, ebHash)` (LeiosNotify),
  `LevRollForward(peer, header)` (ChainSync; `header` announces `ebHash`): `peer` has `ebHash`'s body / full closure; a closure
  offer implies a body offer. Offers carry no size (sizes come from announcements). Junk-offer
  defense, judged per peer: a LeiosNotify offer is legitimate only for the first EB `peer` announced
  for an election (**[SF]** §6 also admits its cert-asserted EB, via `LevCertOffer`); an
  equivocation proof from `peer` does not add an offerable EB. An offer for anything else ⇒ disconnect `peer`. Otherwise
  record availability (bounded — see `LstPeerOfferings`, §2). (So an honest `peer` offering what it saw
  first is fine even when that's our equivocation — it's `peer`'s first; we record it, no disconnect.)
  Honest peers never trip the disconnect, because `BEH-NotifyServe` obeys offer-only-if-announced:
  the announcement precedes the offer on our connection to `peer`.
  ChainSync offers (`LevRollForward`) are a backstop body+closure offer — the peer has `ebHash`
  on its chain — fundamental while a node is syncing (peers don't re-offer historical EBs over
  LeiosNotify) and useful right after a brief partition/eclipse. Bounded per `(peer, election)`
  (latest wins), so a peer churning its header chain cannot accumulate offers. (**[SF]** §6 reads
  this as a cert assertion: the offered EB is the certified one, with precedence over a LeiosNotify
  offer.)

- **`BEH-NotifyServe`** (server). Each downstream peer has a notification queue
  (`LstPeerNotifyQueue`) the node feeds as it receives announcements/certs and completes closures. A credit
  (`MsgLeiosNotificationRequestNext`) from the peer raises the queue's capacity by one; the server
  sends the highest-priority entry whenever the queue is non-empty and sending isn't otherwise
  blocked.
  The queue is back-pressure, not a buffer: its capacity is the peer's outstanding credits, and
  producing into a full queue evicts the lowest-priority entry (which may be the new one).
  Honest peers keep hundreds of credits outstanding, so their queues never fill. The shared
  `notifyPriority` rule (§2) is evaluated against the current `now` at each send and eviction — it is
  time-varying, so the queue keeps no stored order (§2). Relay, from the central state:
  the first announcement when it first arrives; on equivocation, an equivocation proof (carrying
  the equivocating header always, and the first only when the node has not already sent this
  peer the first, `LstPeerOfferGates[peer][el] ≠ ebHash₁`). (**[SF]** §6 adds the cert relay — a `MsgLeiosCertOffer` for a
  certified EB — and suppresses an equivocation proof once the election is certified.) Because the
  announcement state advances at most twice per election, the server never attempts more than that
  to any peer, and needs no per-peer announcement count — only `LstPeerOfferGates` for offer-gating. Offers:
  enqueue an offer for `ebHash` to a peer only once the node has stored the artifact at the
  offered level (`BEH-Completion`) — its body for a body offer, its full closure for a closure
  offer — and `ebHash`'s gate is open for that peer (`LstPeerOfferGates[peer][el] = ebHash`, `el` its election). Never offer before that
  level's artifact is stored nor before the gate is open (persist-before-expose /
  offer-only-if-announced); the gate is what lets clients (`BEH-Offers`) safely disconnect on
  offers for
  never-announced, never-certified EBs.

### LeiosFetch

- **`BEH-BodyFetch`** (any class, no multiplicity limit). While we lack `ebHash`'s body, request
  the whole body from every peer that offers it. No cap on how many peers
  we body-fetch the same `ebHash` from simultaneously.

- **`BEH-ChunkJobs`** (TxCache/Mempool-deduped). When `ebHash`'s body first arrives, split `txset(ebHash)`:
  the TxCache hits (`txCacheOnBody`, §7) are not fetched — `dbCopyFromTxCache` pulls them into `ebHash`'s
  closure; a tx the Mempool holds (`mempoolQueryPresent`; read directly but un-owned — §2) is copied in and
  offered to the TxCache (`txCacheOnAcquire`); the rest are partitioned into jobs of ≈
  `jobSize` (e.g. 64 or 128 kB — parameter). If that leaves no jobs — the TxCache/Mempool already
  held all of `txset(ebHash)` — then nothing is fetched and `ebHash` is complete as soon as its body and
  those cache-reads (its whole closure) are stored (`BEH-Completion`). The LeiosFetch logic treats each EB's txs
  independently — it does not track which other EBs share a tx. So the cache only saves work
  for EBs whose bodies arrive after the tx is cached; if N EB bodies missing the same
  uncached tx arrive before that tx does, it'll be fetched N times (recall that the adversary always has the option to issue EBs that don't overlap at all).

- **`BEH-ClosureFetch`** (class-dependent, no multiplicity limit), once we've decided the jobs for `ebHash`:
  - Stake-sampled offerer of the closure → immediately request every not-yet-received job of
    `ebHash` from them.
  - PeerSharing-sampled offerer of the closure → request not-yet-received jobs up to ≈
    `peerSharingClosureByteLimit` (~1 MB) in flight to that peer for that closure, choosing jobs whose
    txs are currently in-flight with the fewest peers (rarest-first); as responses arrive,
    send more requests to refill back toward the limit, using a low/high-water scheme to avoid constant churn.

- **`BEH-Responses`.** Body: validate hash and size against the announcement; store; chunk into jobs
  (`BEH-ChunkJobs`). Job: validate (the job's txs' hashes/sizes); insert the txs into the
  TxCache (for future EBs) and mark this EB's job received. No cross-EB completion sweep —
  only this EB's state advances.

- **`BEH-Completion`** (+ fan-out). `ebHash` is complete when both its body and its closure (the txs its
  body references) are stored (Conventions). The logic tracks `ebHash`'s remaining txs, so it knows in-memory — no disk query — the
  instant the last part is in hand: either the last outstanding job's txs arriving (`LevBlockTxs`),
  or, when `BEH-ChunkJobs` finds the TxCache/Mempool already held everything, at chunk time (no jobs
  to await). Completion lands when those writes do (`LevDiskDone`). Fan out: immediately the fetch
  side stops pursuing this EB; after the write lands, expose `ebHash`'s closure to the disk-reading
  consumers — offering it to downstream peers (`BEH-NotifyServe`), voting, and notifying
  ChainSel: it must be told that `ebHash`'s closure is now available (readable in the store), since it
  may have a block whose adoption was waiting on exactly that closure, and the notification lets it
  (re)process such a block (its acquired-set/reprocess machinery is out of the fetch core, §1
  below). (persist-before-expose.)

- **`BEH-FetchPriority`** (which EBs first). Certified EBs (a validated cert, via
  `LevCertValidated`) outrank all uncertified ones, regardless of slot. Within each tier:
  uncertified → FreshestFirst (highest slot — stay current near the tip, ready to vote);
  certified → FreshestLast (lowest slot), deferring to the common case — a syncing node, where
  the pending certified EBs lie on one chain, so oldest-first fills the gap from the front and
  unblocks ChainSel's contiguous adoption; across forks (rare, and few certs pend then) it can
  misorder. (Prioritization bites via the per-peer active-EB cap (§3): when a peer is at its cap, this
  ordering picks which of its offered-and-wanted EBs occupy the slots.) TODO: the exact order is really ChainSel's to drive — it knows which chain it is
  adopting and which closure it needs next — so a ChainSel→LeiosFetch signal would supersede this
  heuristic.

- **`BEH-Timeout`.** A generous per-request timeout (≈ today's BlockFetch client timeout). It
  targets buggy/overloaded/dead peers, not adversaries. On timeout: `disconnectFrom(peer)`.

- **`BEH-FetchServe`** (server). On a downstream request for an EB body or (parts of) a closure
  (**[SF]** §6 adds certs): if the store has it, serve it; else disconnect that peer (a well-behaved
  downstream peer only requests what we advertised). Any freshest-EB caching lives inside the
  disk/TxCache IO layer — the server logic is intentionally unaware.

### Startup/Shutdown

The durable record across a restart is the disk store (bodies and txs; **[SF]** certs) and the chain
(ChainDB — owned elsewhere — which holds the CertRBs). Every per-peer state variable
(`LstPeerFirstAnnouncements`/`LstPeerOfferings`/`LstPeerOfferGates`/`LstPeerNotifyQueue`/`LstPeerInflight`/`LstPeerPresent`)
is connection-scoped — gone on shutdown, rebuilt as peers reconnect.

- **`BEH-Shutdown`.** Flush outstanding store writes so every acquired body and tx (both EB closures and TxCache) (**[SF]** cert)
  is on disk, and persist the TxCache window (§7). Nothing else of Leios's own state is persisted.
- **`BEH-Startup`.** Begin with empty per-peer state (peers (re)connect via `LevPeerAdd`); rebuild the TxCache
  from its persisted window and GC it against the wall clock (§7) *before* pruning the store; prune everything below the current immutable tip (`BEH-TipAdvance`). The
  central want/cert/announce state (`LstWanted`/`LstCertified`/`LstFirstAnnouncements`) is re-derived, not
  persisted — ChainSel re-validates the CertRBs on our selected chain (re-emitting
  `LevCertValidated`) and re-delivers their announcements (`LevRollForward`), and gossiped-only
  announcements are re-heard as peers reconnect. (TODO: `BEH-Startup` could also traverse *every*
  CertRB in the ChainDB — including volatile-fork blocks ChainSel won't replay at startup — to
  recover their certs too; whether that's worthwhile is open.) Re-deriving the want-set consults the
  store via `dbQueryPresent` (`BEH-Startup` is its principal caller), so an already-complete EB is not re-fetched.

### Everything else

- **`BEH-PeerClass`.** Every upstream peer is either Stake-sampled (drawn from the stake
  distribution) or PeerSharing-sampled (drawn from Peer Sharing). The class is fixed for the
  connection's lifetime. The two classes are never conflated in any limit or count.

- **`BEH-PeerChurn`.** Three lifecycle events per peer connection. `LevPeerAdd(peer, class)`: allocate
  empty per-peer state. `LevPeerWindDown(peer)`: the diffusion layer is gracefully terminating the
  connection — where `peer` is upstream (we fetch) issue it no new requests; where `peer` is downstream
  (we serve) stop feeding `LstPeerNotifyQueue[peer]`; but keep `peer`'s outstanding in-flight requests so their
  responses can still land (the diffusion layer time-bounds the drain; our per-request
  `BEH-Timeout` bounds it independently). `LevPeerRemove(peer)`: the connection is gone — its
  mini-protocols are torn down, so no further message from `peer` can physically arrive — drop all
  of `peer`'s per-peer state; its outstanding `LstPeerInflight` requests will simply never be answered,
  which is harmless (nothing is shared across peers). The graceful path is `LevPeerWindDown` then
  `LevPeerRemove` after the peer drains; an abrupt `LevPeerRemove` (the peer died) skips the wind-down.

- **`BEH-TipAdvance`** (immutable tip → slot `s`). Schedule disk GC (drop volatile data below the new tip)
  and promote (copy the now-immutable data from volatile storage to the immutable store), then range-delete
  every slot-indexed state variable below the tip. This GC imposes a constraint on almost all of the
  state: each retained state variable must be slot-indexed so the prune is an index-accelerated
  range-delete `[.., s)`, never a scan. So the `Election`-keyed state variables — LeiosNotify
  `LstFirstAnnouncements`/`LstPeerFirstAnnouncements`/`LstPeerOfferings`/`LstPeerOfferGates` (**[SF]** §6's `LstAnnouncedCert`) and LeiosFetch
  `LstWanted`/`LstCertified` — range-delete on their slot-major key. Two things sit outside this discipline:
  the TxCache is not tip-pruned at all — it has its own window/age eviction (§7); and `LstPeerInflight` is
  not slot-GC'd — responses and `LevPeerRemove` reclaim it (a `BEH-Timeout` disconnects, triggering
  `LevPeerRemove`, §2).

- **`BEH-Adversarial`** (peers). Offer-then-withhold, equivocate, lie about sizes/certs, send
  unrequested/garbage, or churn a header chain through many equivocating EBs. The node stays
  safe (no corruption; PeerSharing peer's per-closure byte limit and the per-peer active-EB cap respected; one cert
  per election; `LstPeerOfferings` bounded to ≤2 EBs per peer-election) and live (no-multiplicity-limit
  body/stake-closure/cert fetching means an honest offerer always gets us the data; the generous
  timeout disconnects dead peers). Announcement-gating (`BEH-Wanting`) bounds the want-set to
  the number of elections, so an adversary cannot inflate it via offers.

- **`BEH-SelfIssued`** (this node's block issuer). When our own issuer produces an EB, it enters via the peerless
  `LevSelfIssued(header, body)` stimulus (§3) — fed to the fetch logic as if announced-and-received locally: the
  EB announcement and its body "arrive". `BEH-Wanting` records
  it like any first announcement, then `BEH-ChunkJobs` splits its `txset` — and since we built the EB from our
  own Mempool, those txs are there (modulo a Mempool-churn race), so the closure is acquired *en passant* via
  the Mempool path (`mempoolQueryPresent` → `txCacheOnAcquire`), completing the EB with no fetch. Thereafter it
  diffuses, is served, and is voted on like any other EB. (The issuer's internal logic — EB construction,
  sortition — is out of scope; this is just the hand-off into this document's machinery.)

**Out of the fetch core** (effect interfaces + boundary events): disk store; notifying ChainSel that
an EB closure is acquired (its acquired-set/reprocess machinery); GC/promote bodies; voting. The ChainSync↔Leios bridge is not
modeled here. A ChainSync `MsgRollForward` delivers one RB `header`; the bridge echoes it as the stimulus
`LevRollForward(peer, header, predEb)`, enriching the wire payload with the predecessor's announced-EB
identity (`predEb` — just the relevant parts: its `HeaderHash`, EB hash, and election, from the preceding `HeaderState`)
— needed because the cert-bit case below certifies the EB the predecessor announced, which `header` alone
does not provide. Regardless of its cert bit, `header`
announces the RB's own EB `ebHash` (`BEH-Wanting`),
updating the central `LstFirstAnnouncements` but not `LstPeerFirstAnnouncements`, which is specific to LeiosNotify. When the cert
bit is set, the RB's body certifies the earlier EB `ebHash′` that `predEb` identifies: the peer offers `ebHash′`'s body+closure
(`BEH-Offers`, a backstop to LeiosNotify), and — separately — ChainSel validates that cert on first
processing the CertRB and emits `LevCertValidated(headerHash′, ebHash′)`, which makes `ebHash′`
wanted. **[SF]** §6's `BEH-CertFetch` is the other emitter of `LevCertValidated`, from a cert
offered via `LevCertOffer`.

---

## §2 State + event alphabet

### Genuine state (the minimal core)

LeiosNotify:
- `LstFirstAnnouncements : Election ↦ AnnState`, `AnnState = One RbHeader | Two RbHeader RbHeader` —
  the first accepted announcement, plus the first equivocation, if any, once seen; in `Two` the left
  header is by convention the older, first-announced one. Drives `BEH-Wanting` and the
  server relay `BEH-NotifyServe`.
- `LstPeerFirstAnnouncements : Peer ↦ Election ↦ AnnSeen`, `AnnSeen = One HeaderHash | Two HeaderHash` — the ≤2
  announcement headers this peer sent; the ≤2 bound and equivocation-disconnect derive from it.
  (**[SF]** §6: a peer's cert assertion lives in `LstPeerOfferings`'s `CertSide`.)
- `LstPeerOfferings : Peer ↦ Election ↦ These CertSide OfferSide` — availability; `These` makes the
  all-empty entry simply absence, with no meaningless empty record. `OfferSide = Body |
  BodyAndClosure` is the LeiosNotify offer level. `CertSide = Cert HeaderHash EbHash CertNotificationSeen` — the
  announcement (`HeaderHash`) a peer claims a cert for (the cert's identity, for `BEH-CertFetch`) and the `EbHash`
  it thereby offers; the offered EB is the `CertSide` EB if present, else the first-announced. A
  `LevRollForward` records `These (Cert headerHash ebHash _) BodyAndClosure` (latest wins), so a peer churning its
  header chain cannot accumulate offers. **[SF]** The `CertNotificationSeen` flag is set only by §6's
  `LevCertOffer`; when set, §6 reads `CertSide` as a cert assertion rather than as plain availability.
- `LstPeerOfferGates : Peer ↦ Election ↦ EbHash` — per downstream peer and election, the one EB
  whose gate is open (the EB we announced to that peer; **[SF]** §6 pivots it to the certified EB).
  Election-keyed, so GC range-deletes on the slot-major key. It gates offers
  (`offer-only-if-announced`): we offer `peer` an EB `ebHash` (at election `el`) only when `LstPeerOfferGates[peer][el] = ebHash`.
  It does not gate announcement-sending — `LstFirstAnnouncements` already does that; `BEH-NotifyServe`
  skips an EB offer if its gate here isn't yet open.
- `LstPeerNotifyQueue : Peer ↦ Set Notification` — per-downstream-peer pending notifications; capacity =
  that peer's outstanding credits. Because `notifyPriority` depends on `now` (§ Helper functions), there is
  no fixed order to store and keep sorted: it is an unordered set, and priority is evaluated at the only two
  moments it is consulted — sending removes the `notifyPriority(now, ·)`-max, and producing into a full set
  evicts the `notifyPriority(now, ·)`-min (which may be the entry just produced). The order between two
  pending notifications can therefore flip with nothing but the passage of time, so neither operation may
  trust a previously-computed order.

LeiosFetch:
- `LstWanted : Election ↦ WantState` — deduped by election: at most one wanted EB per election, so
  GC range-deletes on the slot-major key. An election enters via its first `LevBlockAnnouncement` or via a
  validated cert (`LevCertValidated`), so `dom(LstWanted)
  ⊆ dom(LstFirstAnnouncements) ∪ dom(LstCertified)`. `WantState = AwaitingBody { ebHash, bodySize, closureSize } |
  AwaitingTxs { ebHash, closureSize, jobs : NonEmpty (JobId ↦ Job) }` — `ebHash : EbHash` names which EB of the
  election is wanted (**[SF]** a cert can switch it in place). Over all of time, `LstWanted[el].ebHash` takes
  at most two distinct values, because its two insertion sources each name one: the first announcement
  names its EB (later, different-EB announcements are equivocations, never wanted), and a
  validated cert names the certified EB (at most one EB per election is certifiable, by honest-majority
  anti-equivocation).
  They coincide in the baseline; **[SF]** skew is the only way the cert's EB differs from the first-announced.
  `AwaitingTxs.jobs` holds only the
  outstanding jobs (a job is removed once its `LevBlockTxs` arrives) and is non-empty: when the last job's
  response would empty it, `complete(el)` runs and removes the `el` entry instead, so an `AwaitingTxs` never
  rests with no jobs. `Job = { txs : NonEmptySet TxHash }`, txs fixed at chunk time (§5 would shrink it as
  constituent txs arrive by other means). Cached txs (`BEH-ChunkJobs`) never become jobs, so they are
  implicitly on hand.
- `LstCertified : Election ↦ (HeaderHash, EbHash)` — validated cert per election (grown by `LevCertValidated`):
  the certified announcement's `HeaderHash` paired with its `EbHash`, one per election. The `EbHash` is the
  `BEH-FetchPriority` source and the wanted EB; the `HeaderHash` is the cert's identity, against which a
  conflicting cert claim is rejected (the cert-conflict disconnect, §3/§6). In the baseline the `EbHash`
  always equals the first-seen; **[SF]** it may differ only when timing misbehaved (§6).
- `LstTxCache` — a bounded cross-EB tx dedup cache; load-bearing for throughput (§7), with sound membership;
  recall bounded by a space–time trade-off (the window bounds resources, reuse beyond it re-fetches). Its in-memory index (`TxHash → (refcount, acquired)`)
  + `≤128`-announcement window, membership, two-trigger eviction, separate backing store, and persistence are
  all isolated in §7; the main spec only calls its hooks (`txCacheNoteAnnouncement` / `txCacheOnBody` /
  `txCacheOnAcquire` / `dbCopyFromTxCache`).
- `LstPeerInflight : Peer ↦ Seq Req` (a FIFO), `Req = ReqBody Election EbHash | ReqJob Election EbHash JobId`
  **[SF]** `| ReqCert Election HeaderHash` (a cert binds to an announcement, so it is keyed by `HeaderHash`, not `EbHash`) — the LeiosFetch protocol delivers replies in request-issue order
  (at least today), so a response matches the front of the sequence, which is popped and used to validate
  it, never against current `LstWanted`: if the want moved
  on after we sent the request (cert switch, tip advance), the peer's reply is still valid, and it's
  our doing — not the peer's — that we no longer want it, so we drop the result without
  disconnecting. The `Election` is carried only so the handler can advance `LstWanted[el]` (a write) in
  O(log n). Each `Req` is tagged with its issue time (for `BEH-Timeout`). Not slot-GC'd — reclaimed by a
  matching response, or by `LevPeerRemove` (which a `BEH-Timeout` triggers, via `disconnectFrom`); the §5 stagger clock is EB age, not this.

Shared:
- `LstPeerPresent : Peer ↦ { class : Class, phase : Phase }`, `Class = StakeSampled | PeerSharingSampled`
  (`BEH-PeerClass`), `Phase = Active | WindingDown`. A peer is `Active` from `LevPeerAdd` until
  `LevPeerWindDown` flips it to `WindingDown` (the diffusion layer is gracefully terminating it); while
  winding down we issue it no new requests but keep its outstanding `LstPeerInflight` until answered or
  timed out.

### Derived (NOT stored — recomputed; deferred to incrementalization)

LeiosNotify:
- `electionOf : Peer ↦ EbHash ↦ NonEmptySet Election` — per peer, the elections for which `peer`
  (first-)announced (**[SF]** or cert-asserted) an EB hashing to `ebHash`. The offer-ingestion index: a
  bare-`ebHash` offer is *validated* by `electionOf[peer][ebHash]` being non-empty (else junk ⇒ disconnect)
  and *recorded* into `LstPeerOfferings[peer][el]` for each `el` in that set — the body/closure is
  content-addressed, so holding it serves every election that peer named it for. A derived cache of
  `LstPeerFirstAnnouncements` (+ the headers it references), GC'd in lockstep: as an election ages below the
  immutable tip and is range-deleted there, it is removed from its first-announced `ebHash`'s set, and the
  `ebHash` key disappears when the set empties (`NonEmptySet ⇒ absence`). This is the one place a bare
  `EbHash` is mapped back to an `Election`.
- `offerersBody(ebHash)` / `offerersClosure(ebHash)` = peers `peer` for whom some `LstPeerOfferings[peer][el]`
  (`el ∈ electionOf[peer][ebHash]`) has an
  `OfferSide` (`Body` or `BodyAndClosure` / exactly `BodyAndClosure`) and whose offered EB (the
  `CertSide` EB if present, else `peer`'s first-announced) is `ebHash`. These are reverse indexes of
  `LstPeerOfferings`: they invert its forward `Peer ↦ Election ↦ …` shape to answer EB → the set of peers
  offering it (at that level), which is the direction the fetch loop queries — so incrementalization
  maintains the inversion as a stored `EbHash ↦ NonEmptySet Peer` index rather than rescanning every peer
  (an EB with no offerers is simply absent — no empty value rests in the index).

LeiosFetch:
- `jobInflightPeers(ebHash, j) = |{ peer | ReqJob _ ebHash j ∈ LstPeerInflight[peer] }|`  (job-level rarest-first + §5 stagger)
- `activeEbs(peer) = |{ ebHash | some Req for ebHash ∈ LstPeerInflight[peer] }|`  (per-peer active-EB cap, §3)
- `peerSharingInFlightBytes(peer, ebHash) = Σ job sizes for ReqJob _ ebHash _ ∈ LstPeerInflight[peer]`  (PeerSharing peers' ~1 MB limit)
- (There is no reverse `tx ↦ {EB}` index, because cross-EB membership is deliberately not tracked; `BEH-ChunkJobs`/`BEH-Responses`.)

Discipline. Nothing in this section is stored — it is all recomputed from the genuine core on demand. The
core is deliberately kept information-complete: every derived quantity here is a pure function `f` of the
core, so the core never drops anything a cache would need, and no cache is ever folded back into the core.
That makes incrementalization a mechanical, later step — to make some `f` fast, add a stored field holding
its value, declare the invariant `cache == f(core)`, and re-establish that invariant in every rule that
writes the core. So the spec stays simple now, and the optimized implementation is a faithful refinement of it.

### Helper functions (pure; not state)

- `notifyPriority(now, notification)` — a pure comparator giving a total order on notifications against the wallclock
  `now`: announcements for EBs younger than `L_hdr` > announcements for EBs younger than `3·L_hdr` >
  everything else (older announcements, certs, offers, votes); within a tier, higher EB slot (fresher) wins.
  Because the tiering is relative to `now`, the order slides as the clock advances — a fresh announcement
  outranks a bottom-tier cert now, yet once it ages past `3·L_hdr` the higher-slot cert overtakes it — so it
  is recomputed per comparison rather than cached, and any `notifyPriority`-ranked collection (e.g.
  `LstPeerNotifyQueue`) is consulted only when acting, never via a stored order. Shared by the
  `LstPeerNotifyQueue` dequeuer and every enqueuer (`BEH-NotifyServe`).

### Behind the disk store interface (dumb; shared)

`dbWriteBody` · `dbWriteTxsIntoClosure` · **[SF]** `dbWriteCert` · `dbQueryPresent(keys)` (which of those
bodies/txs/**[SF]** certs the store holds) · `dbGarbageCollect(slot)` · `dbPromote(point)` — presence facts
only; no EB↔tx knowledge, no completion, no broadcast, so the logic composes EB-completeness (body present ∧
all closure txs present) from these. The TxCache is a separate component (§7); the main spec reaches it only
through its hooks, and `dbCopyFromTxCache(txs)` (in `BEH-ChunkJobs`) copies cache hits into the closure.

### Reads of un-owned state

The **Mempool** belongs to another component; the logic only *reads* it and never mirrors it —
subscribing to its every change would merely maintain a pointless private copy. `BEH-ChunkJobs`
reads it via `mempoolQueryPresent(txs)` (which of those txs the Mempool holds; a hit is offered to the
TxCache, `txCacheOnAcquire` §7, and thereafter treated as cached) to skip fetching txs the node already
holds. This is the one place the logic reads another component's live state directly rather than via
messages; unlike `Lst…` state, we neither own nor GC it.

### Parameters (shared)

`jobSize` (~64–128 kB) · `peerSharingClosureByteLimit` (~1 MB) · `peerSharingLowWater`/`peerSharingHighWater` ·
`requestTimeout` (generous, ≈ BlockFetch) · `frontSkew` (job-ordering bias, see §3 Job ordering) ·
`L_hdr` (Leios header-diffusion window; `notifyPriority` tiers) · `stakeMaxActiveEbs` (~5) ·
`peerSharingMaxActiveEbs` (~1–3) (per-peer active-EB cap). §5 (anticipated): `staggerPeers` (~1–2) · `staggerDelay` (~1–2 s).

### Messages and stimuli

**Wire messages.** The sender is not part of the message.

- LeiosNotify:
    - `MsgLeiosNotificationRequestNext`
    - `MsgLeiosBlockAnnouncement(header)`
    - `MsgLeiosBlockEquivocationProof(header₁?, header₂)`
    - `MsgLeiosBlockOffer(ebHash)` / `MsgLeiosBlockTxsOffer(ebHash)`
    - **[SF]** `MsgLeiosCertOffer(header)`
- LeiosFetch:
    - `MsgLeiosBlockRequest(ebHash)` / `MsgLeiosBlock(ebHash, body)`
    - `MsgLeiosBlockTxsRequest(ebHash, txs)` / `MsgLeiosBlockTxs(ebHash, txs)`
    - **[SF]** `MsgLeiosCertRequest(headerHash)` / `MsgLeiosCert(cert)`
- ChainSync:
    - `MsgRollForward(header)` merely echoed from ChainSync, not directly received by Leios logic

`MsgLeiosBlockTxsRequest(ebHash, txs)` carries the requested job's tx hashes —
the `jobId` is the client's own handle and never goes on the wire — and the matching `MsgLeiosBlockTxs(ebHash, txs)`
carries those txs, paired to the outstanding `ReqJob` by FIFO order (§2). (TODO: since both ends already
hold `ebHash`'s body, they agree on `txset(ebHash)` and an ordering of it, so `MsgLeiosBlockTxsRequest`'s `txs` could be a compact
intset/bitfield over that index rather than full tx hashes.)

**Stimuli**.

- For received messages: `LevX(peer, …) = (peer, MsgLeiosX(…))` — the stimulus name is the wire name with the
  `MsgLeios` prefix dropped (`LevBlock` ↔ `MsgLeiosBlock`). Except `LevRollForward`, which breaks the
  mold. Its wire payload is `MsgRollForward(header)`, but the cert-bit case certifies the EB the *predecessor*
  announced, which `header` alone does not identify. So the bridge enriches the stimulus with that EB's
  identity — just the relevant parts (EB hash + election), from the preceding `HeaderState`:
  `LevRollForward(peer, header, predEb)` (see §3).
- `LevCertValidated(headerHash, ebHash)` ChainSel validated a CertRB's cert (`headerHash` = the certified
announcement, `ebHash` = the EB it announced); peerless, deduped by election — at most
once per election; updates `LstCertified[el]`/`LstWanted[el]`)
- `LevPeerAdd(peer, class)`
- `LevPeerWindDown(peer)`
- `LevPeerRemove(peer)`
- `LevTimer(timeout, req)`
- `LevDiskDone(op)`
- `LevImmTipAdvanced(slot)`
- `LevSelfIssued(header, body)` an EB this node created, see `BEH-SelfIssued`
- (§5) `LevTimer(staggerDelay, req)`.

**Actions**.
Outbound effects are inlined in the rules (no separate action vocabulary). Issuing a request = send the wire
request (`MsgLeiosBlockRequest` / `MsgLeiosBlockTxsRequest` / **[SF]** `MsgLeiosCertRequest`) to `peer` and
record the matching `Req…` in `LstPeerInflight[peer]`. Others: serve the response (`MsgLeiosBlock` /
`MsgLeiosBlockTxs` / **[SF]** `MsgLeiosCert`, the same message a client receives from upstream); relay a `MsgLeiosBlockAnnouncement` / `MsgLeiosBlockEquivocationProof` / **[SF]** `MsgLeiosCertOffer`; schedule
`dbWriteBody`/`dbWriteTxsIntoClosure`/**[SF]** `dbWriteCert`/`dbCopyFromTxCache`; `disconnectFrom(peer)`; schedule
`dbGarbageCollect`/`dbPromote`; set a timer; update state; emit closure-complete.

---

## §3 Event → effect rules

All updates within a rule are one atomic step. "consider fetching" = run the class-aware
LeiosFetch decision (below). `complete(el)` is an impure helper (not a stimulus), invoked synchronously by
`LevBlock`/`LevBlockTxs` once `el`'s body is present and no jobs remain: let `ebHash = LstWanted[el].ebHash`;
remove `el` from `LstWanted`; emit closure-complete; defer the disk-reading fan-out until the relevant
`LevDiskDone` (persist-before-expose) — once the write lands, that fan-out enqueues a closure offer into
`LstPeerNotifyQueue[d]` for each downstream `d` with `LstPeerOfferGates[d][el] = ebHash`. A request is only issued to `peer` if `LstPeerPresent[peer]` exists with `phase = Active`. Offers update
`LstPeerOfferings[peer][el]` (each `el ∈ electionOf[peer][ebHash]`) per the `These` state machine (§2): `LevBlockOffer`/`LevBlockTxsOffer` raise
the first-announced EB's `OfferSide`; `LevRollForward` records `CertSide` and `BodyAndClosure`.
(**[SF]** §6's `LevCertOffer` also writes `CertSide`.)

### LeiosNotify rules

- **`LevBlockAnnouncement(peer, header)`**: validate `header` (signature/KES, etc.; invalid ⇒ `disconnectFrom(peer)`),
  and it must announce an EB (a LeiosNotify header that announces none is junk ⇒ `disconnectFrom(peer)`);
  let `ebHash`/`bodySize`/`closureSize` be its fields, `el = election(header)`, `h` = its `HeaderHash`. Per-peer check on `LstPeerFirstAnnouncements[peer][el]` (LeiosNotify only; a ChainSync `LevRollForward` skips
  it): absent → `One h`; `One h` (same) → duplicate ⇒
  `disconnectFrom(peer)`; `One h₁` (`h ≠ h₁`) → `Two h₁`; `Two _` (or any third distinct header) →
  `disconnectFrom(peer)`. If accepted, note it to the TxCache (`txCacheNoteAnnouncement(ebHash)`, §7) and update central `LstFirstAnnouncements[el]`: absent → `One header`, and if `ebHash` is not complete nor below the immutable tip
  `LstWanted[el] = AwaitingBody{ ebHash, bodySize, closureSize }`, then enqueue the
  announcement into every downstream `LstPeerNotifyQueue` and consider fetching; `One` → `Two` (different
  header) → record the equivocation, enqueue the equivocation proof downstream (**[SF]** §6
  suppresses this once the election is certified); already `Two` → nothing further. An equivocating
  `ebHash` is never wanted (**[SF]** until/unless certified, §6).

- **`LevBlockEquivocationProof(peer, header₁?, header₂)`** (equivocation proof; `header₂` always, `header₁` only if `peer` had not
  already sent us the first; `ebHashᵢ`/`hᵢ` are each header's EB / `HeaderHash`): validate it is a genuine
  equivocation — both headers validly signed (signature/KES, etc.), each announcing an EB, for the same
  election with `h₁ ≠ h₂` (using `header₁` if present else our recorded first
  `LstPeerFirstAnnouncements[peer][el] = One h₁`; if neither, or if a header announces no EB, `disconnectFrom(peer)`). Advance `LstPeerFirstAnnouncements[peer][el]` to `Two` (inconsistent / already-`Two`
  with other headers → `disconnectFrom(peer)`). If `LstFirstAnnouncements[el]` is not yet `Two`, record the
  equivocation and enqueue the proof downstream (**[SF]** §6 suppresses this once the election is certified).
  If it was absent the proof carries `header₁` (else validation above disconnected): set `Two h₁ h₂` (left `h₁` =
  `ebHash₁`, the older, first-announced winner) and make `ebHash₁` wanted just as `LevBlockAnnouncement`'s `One` branch — gated
  on `ebHash₁` being neither complete nor below the immutable tip. If it was `One h₁`, `ebHash₁` is already the
  recorded, wanted first; just advance to `Two`. The equivocating `ebHash₂` is never wanted.

- **`LevBlockOffer(peer, ebHash)`** (LeiosNotify): `ebHash` must be one `peer` (first-)announced — i.e. `electionOf[peer][ebHash]`
  is non-empty (anything else, e.g. an EB `peer` only relayed as an equivocation, ⇒ `disconnectFrom(peer)`).
  Raise the `OfferSide` to at least `Body` in `LstPeerOfferings[peer][el]` for each `el ∈ electionOf[peer][ebHash]`.
  Consider fetching if `ebHash` is wanted. No `LstWanted` creation, no size. (**[SF]** §6 also admits `peer`'s cert-asserted EB and its precedence
  over the first-announced.)

- **`LevBlockTxsOffer(peer, ebHash)`** (LeiosNotify; implies a body offer): same `electionOf[peer][ebHash]` check; set
  `OfferSide = BodyAndClosure` at each such `el`. Consider fetching if `ebHash` is wanted.

- **`LevRollForward(peer, header, predEb)`** (ChainSync): if `header` announces an EB `ebHash` (unlike
  LeiosNotify, a non-announcing chain header is a normal block — no disconnect, just no announcement effect),
  update
  the central `LstFirstAnnouncements[election(header)]` exactly as `LevBlockAnnouncement`'s central branch (set `LstWanted`,
  relay, detect equivocation) — but never `LstPeerFirstAnnouncements` (chain relay is exempt from the per-peer ≤2
  bound). This holds regardless of the cert bit. If the cert bit is set, the RB's body certifies the
  earlier EB `ebHash′` that its predecessor announced — given by `predEb`, the predecessor's announced-EB identity
  (its `HeaderHash` `headerHash′`, EB hash `ebHash′`, and election `el′`) from the preceding `HeaderState`, which the wire `MsgRollForward(header)` does not
  carry (hence the enriched stimulus, §2):
  record `peer`'s body+closure offer of `ebHash′` in `LstPeerOfferings[peer][el′]`
  (`These (Cert headerHash′ ebHash′ _) BodyAndClosure`, latest wins). ChainSel validates that cert and emits `LevCertValidated(headerHash′, ebHash′)`.
  Cert-conflict disconnect: if `LstCertified[el′]` is already set with a `HeaderHash` other than `headerHash′`,
  this peer is offering a cert that cannot exist ⇒ `disconnectFrom(peer)`.
  Consider fetching. (**[SF]** §6: `ebHash′`'s offer takes precedence over a LeiosNotify offer for the
  first-announced; the recorded `CertSide` also drives `BEH-CertFetch` — request the cert from that peer when we
  lack it, subject to the preferable-header suppression TODO there; and the cert-conflict disconnect above
  extends to the `LevCertOffer` vehicle.)

- **`LevNotificationRequestNext(peer)`**: raises `LstPeerNotifyQueue[peer]`'s capacity by one; if non-empty, dequeue
  the highest-priority notification (`notifyPriority`) and send it. Sending an announcement for `ebHash`
  opens its gate (`LstPeerOfferGates[peer][el] := ebHash`), then consider offering that EB to `peer` if its
  artifact is stored. (**[SF]** §6: sending a `MsgLeiosCertOffer` pivots `peer`'s gate — drop the superseded announced EB for that election from `LstPeerOfferGates[peer]`, add the certified EB.)

### LeiosFetch rules

**Decision** (class-aware; offer-triggered, not a global budget sweep). **A peer's request set is a
function of that peer's own state alone** — its offers (`offerersBody`/`offerersClosure` restricted
to `peer`), its in-flight `LstPeerInflight[peer]`, and its class — plus the shared want-state
(`LstWanted`/`LstCertified`). It never reads another peer's per-peer state, so the per-peer decision
loops are mutually independent (embarrassingly parallel). Exactly two couplings to cross-peer state
exist, both flagged below: PeerSharing rarest-first, and (§5, not yet) staggered requesting.

- Per-peer active-EB cap (all classes; the bound on how many distinct EBs a peer fetches at once).
  Among `peer`'s offered-and-wanted EBs, the *admitted* ones are every EB already active for `peer`
  (some `Req _ ebHash _ ∈ LstPeerInflight[peer]`) plus — taking the rest in `BEH-FetchPriority` order
  (§1: certified FreshestLast ahead of uncertified FreshestFirst) — as many not-yet-active EBs as keep
  `activeEbs(peer) ≤ maxActiveEbs[class]` (`stakeMaxActiveEbs` for stake-sampled, `peerSharingMaxActiveEbs`
  for peer-sharing; separate per class, never conflated). The issue-rules below fire only for admitted EBs.
  This caps the number of distinct EBs in flight per peer — per-EB multiplicity stays uncapped — so a burst
  of announcements, or a peer answering only as slowly as `BEH-Timeout` permits, accrues at most
  `maxActiveEbs[class]` EBs' worth of outstanding requests from that peer rather than one per announcement.
  Reads only `LstPeerInflight[peer]`, so it is not a cross-peer coupling.
- Body (any class) — for each wanted election with `LstWanted[el] = AwaitingBody{ ebHash, … }` that `peer`
  offers (`peer ∈ offerersBody(ebHash)`): issue `ReqBody el ebHash` to `peer` unless `(peer, ReqBody el ebHash) ∈
  LstPeerInflight[peer]`. No per-EB multiplicity cap and no cross-peer check — every offerer is asked
  independently (the active-EB cap above bounds the distinct EBs per peer, not the offerers per EB).
- Stake closure — for a `StakeSampled` `peer ∈ offerersClosure(ebHash)` with `LstWanted[el] = AwaitingTxs`:
  issue `ReqJob el ebHash j` to `peer` for each still-outstanding job `j ∈ LstWanted[el].jobs` with
  `(peer, ReqJob el ebHash j) ∉ LstPeerInflight[peer]`. No per-EB job cap. Its only shared input is `LstWanted[el].jobs`
  (which any peer's response shrinks) — central want-state, not another peer's state.
- PeerShare closure — for a `PeerSharingSampled` `peer ∈ offerersClosure(ebHash)`: while
  `peerSharingInFlightBytes(peer, ebHash) < peerSharingClosureByteLimit`, pick a still-outstanding job not already in-flight
  to `peer` minimizing `jobInflightPeers(ebHash, j)` — rarest-first, the one standing exception: it reads
  how many *other* peers each job is in-flight with — ties broken by the random front-skewed order
  (Job-ordering step below). Issue `ReqJob el ebHash j` to `peer`; stop at the limit; responses refill within a low/high-water
  band.
- Job ordering — every job selection uses the same random order, slightly skewed toward lower-index
  (front-of-EB) jobs (`frontSkew`): the randomness avoids cross-peer head-of-line waiting, and the front-skew
  makes a growing contiguous prefix arrive well before the last job.
- (§5, not yet) Staggered requesting adds the second cross-peer coupling — while an EB is young, hold
  each of its items to ≤ `staggerPeers` peers (again reading cross-peer in-flight).

- **`LevBlock(peer, ebHash, body)`**: the front of `LstPeerInflight[peer]` must be `ReqBody _ ebHash` (§2; drop if not); let `el`
  be its election. Validate hash = `ebHash` and actual size against `ebHash`'s announced `bodySize` (from
  the request, not `LstWanted`); on mismatch drop (adversarial). Else, if `LstWanted[el]` still wants `ebHash`: `scheduleDisk dbWriteBody`, then `BEH-ChunkJobs` — `txCacheOnBody`
  (§7) hits and Mempool hits (`mempoolQueryPresent`) are copied in (`dbCopyFromTxCache` / `txCacheOnAcquire`),
  the rest → jobs — set `LstWanted[el] = AwaitingTxs{ ebHash, jobs }`,
  and if no jobs → `complete(el)`. Otherwise the want has moved on (cert switch / tip advance) — drop
  the result, no disconnect. Consider fetching.

- **`LevBlockTxs(peer, ebHash, txs)`**: the front of `LstPeerInflight[peer]` must be a `ReqJob _ ebHash jobId` for `ebHash`
  — the `jobId` is recovered from that front entry, not the wire (§2; drop if not); let `el` be its election. Validate against the request; on mismatch drop. Else: `scheduleDisk dbWriteTxsIntoClosure`; offer the txs to the TxCache (`txCacheOnAcquire`, §7).
  If `LstWanted[el]` still wants `ebHash`, remove `jobId` from its outstanding jobs (only `el`
  advances; if now empty → `complete(el)`); otherwise the want has moved on — drop, no disconnect.
  Consider fetching (PeerSharing peers refill toward `peerSharingHighWater`).

- **`LevCertValidated(headerHash, ebHash)`** (the cert for announcement `headerHash`, which announced `ebHash`, has been validated — by ChainSel on a CertRB, or
  **[SF]** by `BEH-CertFetch`; emitted only post-validation, so reliable (§6 covers ChainSel's tentative-header timing); `el` =
  the announcement's election; **peerless** — the cert is cryptographic — and deduped by election, so it fires at
  most once per election): set `LstCertified[el] = (headerHash, ebHash)` (`BEH-FetchPriority` now ranks `ebHash` top) and
  ensure `LstWanted[el]` wants `ebHash` — but, exactly as `LevBlockAnnouncement`, only when `ebHash` is neither complete
  (consult the store via `dbQueryPresent`, as `BEH-Startup` does) nor below the immutable tip, since the cert routinely
  validates after we already fetched and completed the first-announced EB; if so, add an `AwaitingBody`
  entry when absent (sizes from its announcement). Consider fetching. (**[SF]** §6: when `ebHash` *differs* from
  the first-seen EB `LstWanted[el]` was tracking, switch `LstWanted[el]` to `ebHash` in place — applying the
  same completeness check (we may already hold it, e.g. self-issued via `BEH-SelfIssued`): complete ⇒ drop the
  entry, body present ⇒ `AwaitingTxs`, else `AwaitingBody`; the first-seen's in-flight reqs are left to the
  response-guard / `BEH-Timeout`. Also, if `LstAnnouncedCert[el]` is unset, set it to `ebHash` and enqueue a
  `MsgLeiosCertOffer` for `ebHash` into every downstream peer's `LstPeerNotifyQueue` (the cert relay, deduped via
  `LstAnnouncedCert`); and disconnect any peer whose `LstPeerOfferings[_][el]` `CertSide` names a different `HeaderHash`.)

- **`LevBlockRequest(peer, ebHash)` / `LevBlockTxsRequest(peer, ebHash, txs)`** (server): if `dbQueryPresent` confirms the store
  has the requested body / txs, serve it; otherwise `disconnectFrom(peer)`. (**[SF]** §6 adds `LevCertRequest`.)

### Everything-else rules

- **`LevPeerAdd(peer, class)`**: `LstPeerPresent[peer] = { class, Active }`; `LstPeerOfferings[peer] = ∅`.

- **`LevPeerWindDown(peer)`**: `LstPeerPresent[peer].phase = WindingDown`. Where `peer` is upstream, issue it no new requests;
  where `peer` is downstream, stop feeding `LstPeerNotifyQueue[peer]`. Outstanding `LstPeerInflight[peer]` stay until answered
  or `BEH-Timeout`. Consider fetching (re-route what `peer` won't deliver to other peers).

- **`LevPeerRemove(peer)`**: `LstPeerPresent \= {peer}`; drop
  `LstPeerFirstAnnouncements[peer]`/`LstPeerOfferings[peer]`/`LstPeerOfferGates[peer]`/`LstPeerNotifyQueue[peer]`/`LstPeerInflight[peer]`.
  The connection is gone, so no further message from `peer` can arrive. Consider fetching.

- **`LevTimer(timeout, req)`**: `req` is overdue → `disconnectFrom(peer)` for the peer that owns it.

- **`LevDiskDone(op)`**: mark the bytes stored (now readable); release fan-out deferred on `op`.

- **`LevImmTipAdvanced(s)`**: `scheduleDisk dbGarbageCollect(s)`/`dbPromote`; range-delete every slot-indexed state variable
  below `s` (`BEH-TipAdvance`) — the `Election`-keyed
  `LstFirstAnnouncements`/`LstPeerFirstAnnouncements`/`LstPeerOfferings`/`LstPeerOfferGates`/`LstWanted`/`LstCertified` (**[SF]** §6's `LstAnnouncedCert`).
  The TxCache is not pruned here (its own window/age eviction, §7) and `LstPeerInflight` is not GC'd here. Consider fetching.

- **`LevSelfIssued(header, body)`** (peerless; `BEH-SelfIssued`): treat `header` as a first announcement —
  run `LevBlockAnnouncement`'s central-branch update (record `LstFirstAnnouncements[el]`, make wanted, relay
  downstream, `txCacheNoteAnnouncement`), but no per-peer state and no wire-validation (there is no peer, and
  we built the header), exactly as `LevRollForward` skips `LstPeerFirstAnnouncements`. Then run `BEH-ChunkJobs`
  on `body` directly — no in-flight `Req` to match, we already hold it: the Mempool holds the txs we built the
  EB from (modulo a churn race), so the closure is acquired via `mempoolQueryPresent`/`txCacheOnAcquire` and
  `complete(el)` fires with no fetch. Consider fetching (for any txs the race left un-acquired).

---

## §4 Expected properties

(Safety = invariant at every trace state; Liveness = eventually, with fairness.)

### LeiosNotify

- Want is announcement-gated (safety). `dom(LstWanted) ⊆ dom(LstFirstAnnouncements) ∪ dom(LstCertified)`, and for
  each `el ∈ dom(LstWanted)`, `LstWanted[el].ebHash` is the first-announced EB for `el` (**[SF]** or `LstCertified[el]`'s `EbHash`); one
  wanted EB per election is structural (the key). An equivocating EB is never wanted/fetched/served
  **[SF]** unless it is the certified one.
- Offers are announced or certified (safety). The EB offered by `peer` (the `CertSide` EB if present,
  else first-announced) is the first EB `peer` announced for its election **[SF]** or `peer`'s
  cert-asserted EB — judged per peer against `LstPeerFirstAnnouncements[peer][el]` / `LstPeerOfferings[peer][el]`'s `CertSide`,
  not the global `LstFirstAnnouncements`. Anything else triggered `disconnectFrom`.
- Offers are bounded (safety). `∀ peer, el`: `LstPeerOfferings[peer][el]` holds at most one cert claim (`HeaderHash`) and one
  offer level; a peer churning headers cannot accumulate more.
- Announce-before-offer (safety). We never send a downstream peer an offer for an EB unless it
  is in that peer's `LstPeerOfferGates`.
- Bounded announcements (safety). Per upstream peer and election: ≤2 uncertified announcements
  (`LstPeerFirstAnnouncements[_][el]`); **[SF]** and `LstPeerOfferings[_][el]`'s `CertSide` holds ≤1 cert claim (`HeaderHash`)
  across both cert-assertion vehicles (one-shot `LevCertOffer`, repeatable `LevRollForward`); a
  second `LevCertOffer` or a different `HeaderHash` disconnects.

### LeiosFetch

- Cert uniqueness (safety). At most one EB per election is certified, and a peer claiming a cert for a
  different `HeaderHash` than the certified announcement is disconnected — baseline on the cert-bearing
  `LevRollForward` path; **[SF]** §6 extends the same check to the `LevCertOffer` vehicle.
- Client soundness (safety). Each in-flight `ReqBody`/`ReqJob`/**[SF]** `ReqCert` was issued to a peer
  offering that `ebHash`/job/cert, for an `ebHash` wanted at issue time. A request may outlive
  its want (its response is then dropped, §3), so it is reclaimed by a
  matching response or by `LevPeerRemove` (which a `BEH-Timeout` triggers) rather than held invariant against current `LstWanted`.
- Server soundness (safety). The node serves a downstream request only for data stored (readable);
  a request for absent data triggers `disconnectFrom`.
- TODO (Server-vs-Client soundness race). We offer `peer` an EB; `peer` honestly requests it (its own Client
  soundness); but if we GC the artifact in between — e.g. it fell below our immutable tip and was dropped
  rather than promoted — the request finds it absent and we disconnect an honest `peer` (and symmetrically a
  peer can disconnect us). Diffusing it needs a GC grace window: offered content stays readable for at least
  the offer→request latency past when it would otherwise be dropped, so the disconnect fires only on a genuine
  misbehaver. The exact delay — and whether a delay alone suffices vs. refcounting offered content — is unsettled.
- Sizes and hashes match the announcement (safety). Every body/closure fetched or served matches both its
  announced size and its content hash — a body hashes to its announced `ebHash`, a job's txs match the
  requested job — and any mismatch is rejected (adversarial).
- PeerShare closure cap (safety). `∀ peer : PeerSharing peer, ebHash: peerSharingInFlightBytes(peer, ebHash) ≤ peerSharingClosureByteLimit`.
- Per-peer active-EB cap (safety). `∀ peer: activeEbs(peer) ≤ maxActiveEbs[class(peer)]` (per class, never conflated).
- In-flight integrity (safety). Across the disconnect/response/timeout races — a response and a
  `BEH-Timeout`/`disconnectFrom` landing on the same request, or a response arriving after its peer was
  removed — every issued request leaves `LstPeerInflight` exactly once: never double-removed, never leaked.
  The reference model cannot actually reach this hazard (stimuli are handled atomically and `LstPeerInflight`
  is the sole source of truth, so a redundant removal is a no-op and any straggler is reclaimed by
  `LevPeerRemove`); we state it anyway because the model is a conformance oracle for a concurrent
  implementation whose threads do interleave these, and because any derived mirror of in-flight — per-peer
  and global byte budgets (Σ in-flight sizes), reverse request indexes — desyncs under a double-remove or a
  leak. So it is the invariant incrementalization and the real node must preserve, not one the atomic core
  can currently violate.
- No wasted re-request (safety). No in-flight request targets an already-complete EB/received job **[SF]** or already-validated cert.
- Persist-before-expose (safety). Anything offered/handed-off is already stored (readable) — within a run
  and across a clean restart. (Resume-from-suspend can still race a reader against a GC deleter; robustly
  closing that gap needs content pinning — the deferred TODO of §7.)
- Progress (liveness). Wanted data persistently offered by an honest present peer is eventually
  fetched; order is certified (FreshestLast) ahead of uncertified (FreshestFirst).
- Completion (liveness). Once body+all-txs present, completion is detected and (after the write lands) offered to all consumers (a.k.a. fanned-out).

### Everything else

- Adversary-tolerance (liveness). Withholding/equivocating/lying peers cannot prevent acquisition
  from an honest offerer; the generous timeout disconnects dead peers, reclaiming their slots (not a security mechanism).

---

## §5 Anticipated refinements (roadmap)

- **Staggered requesting** (hold-off before broadening). Soften the "immediately request from
  every offerer" aggressiveness: while an EB is younger than `staggerDelay` (~1–2 s) measured
  from its own slot-time (not from when we received offers or issued requests), keep each item
  (body/job) in-flight with at most `staggerPeers` (~1–2) peers; once the EB ages past that,
  pull the item from all other offerers of its closure. Clocking on EB age rather than message
  timing makes the window objective and un-gameable. (Consequence: an EB we first learn of
  already older than `staggerDelay` gets no hold-off — pulled from everyone at once, right for a
  laggard.) Per-class counts, never conflated. Clock = the EB slot (the `LstWanted` election key);
  multiplicity = `jobInflightPeers`/per-class variants; needs no per-request issue-time.
- **Limit stale EBs within the active-EB cap.** The per-peer active-EB cap (§3) currently treats all EBs
  alike. It could additionally refuse to admit a new not-yet-active EB when that EB is already "too old" and
  the peer's active set already holds at least one "too old" EB — so a peer works on at most one stale EB at a
  time, keeping its slots for fresher EBs we can still vote on. ("Too old" threshold TBD — e.g. past the
  freshness window the `notifyPriority` tiers already use.)
- **Shrink jobs as their txs arrive by other means.** A job's tx-set is fixed at chunk time, so a request for
  it (to another offerer, or a re-issue) asks for the whole set even after some of its txs have arrived — via
  a concurrent job (for another EB), a Mempool insert, or a TxCache hit — and those redundant bytes are
  received and deduped (sound, just wasteful bandwidth). Refinement: shrink the outstanding job to drop the
  already-acquired txs, so subsequent requests for it exclude them. Partial redundancy only ever trims future
  requests this way: we never cancel an already-sent request just to re-issue a smaller version of the same
  job (churn for no gain, and remote cancels are unreliable anyway — they race the response already in flight).
  A cancel is reserved for total redundancy — the next item.
- **Best-effort job cancellation (only once a job is wholly redundant).** Once a job is entirely in hand — its
  `MsgLeiosBlockTxs` arrived, or all its txs were acquired by other means — other peers still holding that same job
  in flight have nothing left to give. (We ask several at once, aggressive / rarest-first, mostly stake-sampled,
  so this is common.) Send each a best-effort `MsgTryCancel` for it (perhaps only stake-sampled peers, the ones
  we ask aggressively — TBD). We never assume a cancel succeeds: the peer is still obligated to send the matching
  response to the original `MsgLeiosBlockTxsRequest`, and nothing ever responds to the `MsgTryCancel` itself — but with a
  cancel outstanding we now also accept an entirely empty `MsgLeiosBlockTxs`, which is how a peer that honored the
  cancel discharges that obligation. Saves the redundant transfer when honored, costs only the cancel message
  when not.
- **Chunk the EB body (Merkle-rooted segments).** `BEH-BodyFetch` currently pulls the whole body from a
  single offerer (and redundantly from each). Instead, split the body into objectively-agreed segments —
  e.g. fixed `bodyJobsSize` (~64 kB) spans of its canonical serialization — and let the body's announced hash
  (what `ebHash` commits to) be a Merkle root over them. Each segment then carries a Merkle path, is verifiable
  on its own, and can be fetched from a different offerer in parallel: the body analogue of chunking txs into
  jobs, needing no FEC. (FEC over the segments would be a further elaboration — any sufficient subset
  reconstructs, so a slow or missing peer costs only its parts. New parameter `bodyJobsSize`.)
- **Chunk the closure incrementally (incremental offers for streaming closures).** The closure is already split into jobs, but a peer
  offers it all-or-nothing: `BodyAndClosure` advertises the complete closure, so a peer can only offer once it
  holds every tx. Independently of whether the body is chunked, a peer could offer the closure incrementally —
  advertising the first chunk as soon as it has it, even while later chunks are still missing, so we can start
  fetching what is available immediately. This needs only that `MsgLeiosBlockTxsOffer` carry one extra small
  field saying which chunks the offer covers: a count of leading chunks held, or a short bitfield with one bit
  per chunk.
- **Resist a protocol storm.** Bad luck can put very many EBs in flight at once — far more closure to diffuse
  than the network handles comfortably. The hazard is that voting certifies an EB whose closure honest nodes
  could not diffuse well enough to hold, leaving a certified EB the network cannot actually select. TODO: read
  the recent proposal for how to prevent this scenario.

---

## §6 Skew fallback

Under the baseline (§1–§4) the certified EB for an election is the one every honest node saw
announced first, so none of this fires. It exists for the rare timing/network failure where
the certified EB differs from a node's first-seen, and for the LeiosNotify cert-gossip path that
diffuses a cert independently of the chain. §6 only *adds* — one state variable (`LstAnnouncedCert`), the three
cert messages with their stimuli, behaviors, and rules. Its extensions to existing main-spec rules are not
repeated here: they live inline at those rules, tagged **[SF]**. §6 redefines no main-spec state type. All of
it is **[SF]**.

§6 presumes ChainSel validates a CertRB's cert before tentatively forwarding that CertRB's header downstream —
diffusion pipelining would otherwise forward the header ahead of body validation. That is what stops us from
causing a downstream peer to process a cert-bit `LevRollForward` before we ourselves have validated the cert.
The cost is a small per-CertRB delay to that tentative-header forward, not a headers-first concession: the
CertRB's body is already in hand at processing, so it merely reorders the validation work. (Acting on a cert —
setting `LstCertified`, switching `LstWanted`, offering it downstream — is gated separately on
`LevCertValidated`, i.e. on our own validation, independent of this timing.)

### Added state

- `LstAnnouncedCert : Election ↦ EbHash` — the validated cert we have offered downstream peers
  for the election. The cert analogue of `LstFirstAnnouncements`, driving and deduping the `MsgLeiosCertOffer` relay.
- The `CertNotificationSeen` flag on `LstPeerOfferings`'s `CertSide` (§2) is set here — by `LevCertOffer`,
  never by `LevRollForward` — recording the cert-assertion vehicle. The main spec leaves it unset.
- `dbWriteCert` — a validated cert is written to the disk store (keyed by `HeaderHash`), whence it is read to
  serve a `LevCertRequest` or to apply late.

Cert identity is the announcement `HeaderHash`, not the `EbHash`: a cert binds to an *announcement*. So
`MsgLeiosCertRequest`/`ReqCert` key by `HeaderHash`, `CertSide = Cert HeaderHash EbHash CertNotificationSeen`
carries both the claimed announcement (for `BEH-CertFetch`) and the offered `EbHash`, and `predEb` carries the
predecessor's `HeaderHash`. Validation needs only the `HeaderHash` (the identity the votes commit to) plus the
election's committee, and the want/priority effect needs the `EbHash`+`Election` — so nothing relies on
retaining the full header, and the certified `HeaderHash → cert` lives in the disk store post-validation.
`LstCertified` pairs the certified `HeaderHash` with its `EbHash` (`Election ↦ (HeaderHash, EbHash)`) so a
conflicting cert claim can be rejected at offer time (§3/§6); `LstAnnouncedCert` stays `Election ↦ EbHash`
(relay-dedup).

### Added events

- Messages: `MsgLeiosCertOffer(header)`, `MsgLeiosCert(cert)`, `MsgLeiosCertRequest(headerHash)`
- And the corresponding `Lev*` stimuli.

### Behaviors

- **`BEH-CertNotify`**. `LevCertOffer(peer, header)` carries an `RbHeader`, read as both an
  announcement of its EB `ebHash` (so it can be the first announcement we see; sizes usable like any
  `LevBlockAnnouncement`; exempt from the ≤2 uncertified-announcement bound) and an offer of that EB's specific
  cert. At most one `LevCertOffer` per `(peer, election)`: a second naming a different `HeaderHash` is a peer
  claiming two certs for one election — impossible, so disconnect `peer`. Likewise, if we already hold a
  validated cert for the election (`LstCertified[el]`) whose `HeaderHash` differs from the one offered, this
  offer asserts a non-existent cert — the cert-conflict disconnect (§3) applied to the `LevCertOffer` vehicle.
  On receipt we record
  `peer`'s cert assertion in `LstPeerOfferings`'s `CertSide` (its EB and the `CertNotificationSeen` flag), and the
  peer must stop sending vote notifications (which are otherwise out of this document's scope) for `ebHash`. A `LevCertOffer` offers the cert, not
  body/closure — those still come via `LevBlockOffer`/`LevBlockTxsOffer`. If we don't already hold the
  validated cert, fetch it (`BEH-CertFetch`) aggressively — every offerer, no multiplicity cap — to
  resist slow-loris stalling, as for body/job fetch.

- **`BEH-CertFetch`**. When any peer has offered a cert and we lack
  the validated cert, request it from that peer (`MsgLeiosCertRequest(headerHash)`, recording `ReqCert el headerHash`) aggressively (every offerer, no
  multiplicity cap, `BEH-Timeout` for dead peers). On the response, validate. Invalid → disconnect
  only the provider. Valid → emit `LevCertValidated(headerHash, ebHash)` (its full §3 effect — including the
  **[SF]** skew extension — then applies). ChainSel, on first processing a CertRB (its header diffused by ChainSync, its body by
  BlockFetch), is the other emitter of `LevCertValidated`
  (§1). TODO: while a cert-bearing `LevRollForward`'s header is still preferable to our current selection,
  BlockFetch is already fetching that CertRB body (which carries the cert), so the
  redundant `MsgLeiosCertRequest` to that peer could be suppressed until the header is no longer preferable.
  Conversely, when we do fetch the cert over LeiosFetch first, we could paste that header and the `LevCert`
  cert together into the CertRB and hand it to the ChainDB, so BlockFetch stops trying to acquire it.

- **Cert-serving** (`BEH-FetchServe` extension). A downstream `LevCertRequest(peer, headerHash)` is served from
  the store if present, else `disconnectFrom(peer)` — exactly as for bodies/closures.

### Rules

- **`LevCertOffer(peer, header)`** (`ebHash` = header's EB, `headerHash` = its `HeaderHash`, `el = election(header)`): treat the header as an
  announcement (run the `LevBlockAnnouncement` update; exempt from the ≤2 bound). If `LstCertified[el]` is
  already set with a `HeaderHash` ≠ `headerHash` ⇒ `disconnectFrom(peer)` (offering a cert that cannot exist —
  the §3 cert-conflict disconnect). Update `LstPeerOfferings[peer][el]`'s
  `CertSide`: if it already has `CertNotificationSeen` set ⇒ `disconnectFrom(peer)` (a second `LevCertOffer`);
  if it is `Cert headerHash′ _ _` with `headerHash′ ≠ headerHash` ⇒ `disconnectFrom(peer)`; if `Cert headerHash _ _` (same) → set
  `CertNotificationSeen`, keeping the `OfferSide`; if there is no `CertSide` → set it to `Cert headerHash ebHash` with
  `CertNotificationSeen`, dropping any `OfferSide` (the prior body/closure were for the first-announced,
  a different EB). A `LevCertOffer` offers the cert, not body/closure — those still come via
  `LevBlockOffer`/`LevBlockTxsOffer` for the certified EB. If we lack the validated cert, fetch it
  (`BEH-CertFetch`) aggressively. `peer` must also stop sending vote notifications for `ebHash`.

- **`LevCert(peer, cert)`**: validate `cert` against the `headerHash` of the `MsgLeiosCertRequest(headerHash)` it
  answers — the front `ReqCert` of `LstPeerInflight[peer]` (§2; drop if the front isn't a `ReqCert`) — plus the
  election committee. Invalid → `disconnectFrom(peer)`. Valid → `scheduleDisk dbWriteCert`, emit `LevCertValidated(headerHash, ebHash)`
  (`ebHash` = the EB that announcement announced).

- **`LevCertRequest(peer, headerHash)`**: if `dbQueryPresent` confirms the store holds the cert for
  `headerHash`, serve it (`MsgLeiosCert`); otherwise `disconnectFrom(peer)` — the cert analogue of §3's
  body/closure server rule, realizing the `Cert-serving` behavior above.

### Properties

The **[SF]**-tagged clauses in §4 belong here: the cert-conflict disconnect extended to the `LevCertOffer`
vehicle (the `LevRollForward` case is baseline, §3); want/offers admitting the certified EB; and
`LstPeerOfferings[_][el]`'s `CertSide` holding ≤1 distinct cert claim (`HeaderHash`) across both vehicles
(one-shot `LevCertOffer`, repeatable `LevRollForward`).

## §7 TxCache (bounded cross-EB tx dedup)

The TxCache lets `BEH-ChunkJobs` skip re-fetching a tx we already hold because a recent EB referenced it. It is
load-bearing for throughput, not optional: without it a node re-fetches shared txs per-EB, and under
adversarial conditions (e.g. a mempool-fragmentation attack, where pools' EBs share few txs with a node's
mempool) that redundancy can drive Leios throughput toward zero. Its contract, though, is small and stable,
which is what lets the machinery below be skipped by some readers:
- **Sound**: a reported hit is a tx we genuinely hold (`acquired ⟺ bytes present`, maintained by
  construction below) — a false hit would make `BEH-ChunkJobs` skip a needed fetch and stall the closure.
- **Recall**: bounded by a space–time trade-off — we dedup only recent reuse, within the window below, to
  bound the cache's memory and disk; a tx referenced beyond the window, not yet acquired, or from a
  non-qualifying EB is re-fetched, trading some bandwidth for bounded space.

So some readers can treat the cache as that black box — sound membership, recall bounded to recent reuse — and
skip §7's bounding/eviction/persistence machinery; it's the machinery that's separable from the protocol, not
the cache. It is a component separate from the LeiosDB closure store (LeiosDB keeps EBs until immutability; the
TxCache keeps a far tighter recent window), so the two are GC'd independently.

The main spec touches it only through these hooks:

- **`txCacheNoteAnnouncement(eb)`** — `LevBlockAnnouncement` calls this for every accepted announcement; the cache
  applies its own qualifying filter (below) and advances its window.
- **`txCacheOnBody(eb, txset(eb)) → hits`** — `BEH-ChunkJobs` calls this when the body arrives; it returns the
  cached (already acquired) txs so they're not re-fetched, and records that `eb` references `txset(eb)`.
- **`txCacheOnAcquire(txs)`** — called when some txs' bytes arrive (`LevBlockTxs`, or a Mempool hit); for each
  tx the cache is still tracking (`refcount > 0` — some eligible in-window EB references it) it marks
  `acquired` and stores the bytes; a tx no eligible in-window EB references is ignored (not stored).
- **`dbCopyFromTxCache(txs)`** — copy hit bytes from the cache's backing store into the requesting EB's
  LeiosDB closure.

Eviction/GC is internal (announcement- and time-driven); the only place the body sequences it is `BEH-Startup`.

**Hot-path constraint.** The dedup lookup sits in a hot loop — `BEH-ChunkJobs` consults the cache once per tx of
every arriving EB body, up to ~15000 lookups per body, and bodies arrive continuously — so its per-lookup
latency is as much a design driver as soundness and bounded memory. Each lookup must be a single O(1) read of
one in-memory hash table (`LstTxCacheIndex`), taking `acquired` directly; the design deliberately keeps no
second index (no slot-keyed structure, no reverse `tx → {EB}` index — eviction instead re-reads the one leaving
body, below), since consulting multiple structures per lookup, or any scan, would not keep up.

**State.** Two in-memory variables, plus a disk-resident backing store:
- `LstTxCacheIndex : TxHash ↦ { refcount, acquired }` — the membership/refcount index, the one large structure
  (~`128` EBs × up to ~15k txs ≈ 2 million entries). `refcount` = how many in-window qualifying EBs
  whose body we hold reference the tx; `acquired` = its bytes are in the backing store. Dedup hits are read
  from `acquired`, never `refcount`.
- `LstTxCacheWindow : Election ↦ { ebHash, foldedIn }` — the `≤128` qualifying announcements, kept slot-ordered
  (the `Election`'s slot gives both the ordering — pop-min for eviction — and the age). `foldedIn` records
  whether we've folded that EB's body into the refcounts, so eviction knows whether to decrement.
- The backing store (on disk, not an `Lst*` in-memory variable): a separate, content-addressed store of the
  acquired bytes, read/deleted by `TxHash` (`dbCopyFromTxCache`/`dbTxCacheEvict`). Its storage mechanism is
  deliberately unspecified; the one requirement is that its own memory use stays bounded — in particular it
  must not keep a full second in-memory `TxHash → location` index alongside `LstTxCacheIndex`.

**Qualifying predicate** (what `txCacheNoteAnnouncement` admits): the announcement is the first we saw for
its election — honest EBs are never equivocated, so the first-seen is the honest EB and certs are
irrelevant here (`LevCertValidated` is ignored) — and it arrived ≤ `30s` after its slot, which stops
an adversary from withholding then bursting stale announcements to reshape the cache.

**Window.** The youngest `128` qualifying announcements by slot. Announcement-driven and body-free: a
young body-less announcement still takes a slot and advances the cutoff — which is why the cache reacts to
announcements even when bodies never arrive.

**Refcount lifecycle.** Increments happen only at body arrival, the only moment the txlist is known (an
announcement is a header; an acquisition is one tx's bytes): `txCacheOnBody` does `++refcount` for each tx of
an in-window EB and marks it folded-in. `acquired` is then set by `txCacheOnAcquire`, but only for a tracked
tx (`refcount > 0`) — so `acquired ⟹ refcount > 0`, and bytes for a tx no eligible EB references are dropped,
not stored. Because the hit-set is read from `acquired`, an EB's own freshly-referenced-but-unfetched txs
correctly miss and get fetched.

**Eviction** — two triggers, both a pop-min from the slot-ordered window (index-accelerated, never a scan):
- the window exceeds `128` (a younger qualifying announcement arrived), or
- the oldest EB's age exceeds `txCacheMaxAge` (time/slot-driven, so it fires even when no announcements arrive).

The action: take the leaving EB; if its body was folded in, re-read that one body, `--refcount` each tx, and
at 0 remove the entry and `dbTxCacheEvict` the bytes (if acquired). A body-less leaving EB folded nothing, so
there is nothing to undo. Eviction is prompt and physical, so the backing-store footprint stays ≈ the
window rather than ballooning to LeiosDB's volatile size.

The age trigger is essential, not redundant: if the network stops issuing EBs the count trigger never fires
and the EB never leaves — yet the immutable tip keeps advancing and LeiosDB GCs its body, after which eviction
could no longer re-read it to decrement (a permanent leak). Hence `txCacheMaxAge` < time-to-immutability
(the body is guaranteed still present when we age-evict) and comfortably above a healthy `128`-EB span (so it
never bites during normal flow). ~8 hr seems sufficient.

**Persistence / restart.** The bytes are durable in the backing store; the in-memory index is not persisted,
but the `≤128` window is. `BEH-Startup` rebuilds the index by reading those windowed EBs' bodies, and GCs the
TxCache (against the jumped wall clock) before pruning the store — so the bodies the age-eviction re-reads are
still present. A restart is a fresh process, so that ordering is a free, deliberate step; after a long downtime
the whole window is past `txCacheMaxAge` and ages out at once → cold cache (consistent with inert-while-syncing),
after a brief one it is kept.

This design does not yet support resume-from-suspend. There the process stays live while the wall clock jumps
mid-flight: the wall-clock-driven age-eviction fires on resume and races the (tip-driven) LeiosDB volatile GC,
and a TxCache reader (`dbCopyFromTxCache`) can race the eviction deleting the same bytes. A GC delay armed
before the suspend is no help — its window has effectively elapsed across the suspend. The robust fix is
pinning: GC, in both stores, skips bytes `LstTxCacheWindow` still references, rather than relying on timing —
deferred (TODO). So restarts are supported; suspend/resume is not yet.

**Properties.** Load-bearing for throughput (its absence collapses throughput under fragmentation). Sound (a
reported hit is genuinely held — never a false hit, which would stall a closure); recall bounded by a
deliberate space–time trade-off (the window bounds memory/disk; reuse beyond it re-fetches). Bounded memory (the one index + the `128`
window; backing store bounded by requirement) and bounded disk (≈ the window, via prompt eviction).
Adversary-resistant: the `30s` gate admits only timely announcements (no withheld-then-bursted reshaping), and
only the first-announced EB per election takes a slot, so admissions are sortition-rate-limited — equivocations
and offer bursts can't inflate the window. Inert while syncing: the announcements seen during catch-up are for
old slots (well past `30s`), so none qualify and none are admitted — a node that has been behind for a while
simply has an empty cache (and anything carried in from before drains over `txCacheMaxAge`).

**Parameters.** `128` (window EBs) · `30s` (announcement promptness) · `txCacheMaxAge` (~8 h, < time-to-immutability).

## §8 Interfaces with out-of-scope components

This document scopes out RB and EB creation (the "block forging thread"), voting, the Inbound Peer Governor, ChainSel's acquired-set/reprocess machinery, and
GC/promote. Some of those neighbors still need to read state this document owns, or feed events into it —
meaning small embellishments here. Collected so they aren't forgotten when those components are built; not
specified in detail.

**Voting.**
- **Per-tx validated flag.** Voting needs the TxCache to record, per tx, whether it has ever been successfully
  validated — a `validated` field alongside `(refcount, acquired)` in `LstTxCacheIndex`, with hooks to set and
  read it. Cheap for a Mempool tx (set it at once via `txCacheOnAcquire`, since the Mempool holds only validated
  txs); a peer-fetched tx is unvalidated until checked. (Moved here from §7.)
- **Timely-announcement set.** Voting needs the EB announcements that arrived within `L_hdr` of their slot
  onset — either we maintain that set or voting queries per-announcement timeliness. We already record
  arrival-vs-slot for the TxCache's `30s` gate (§7), so an `L_hdr`-keyed view of the same is a near relative.
- **Cert generation feeds back in (full SF).** Under the full skew fallback our own voting logic may produce a
  cert; we ingest it as `LevCertValidated` — a third emitter, alongside ChainSel's CertRB path and **[SF]**
  `BEH-CertFetch`. (Direction here is voting → us, not a read.)

**Inbound Peer Governor.**
- **First provider per election.** The governor may want which peer first delivered our first-seen announcement
  for each election (to credit peers that diffuse fresh EBs promptly). `LstFirstAnnouncements` is peerless
  today; this would additionally record, per election, the peer behind the first-seen announcement.
