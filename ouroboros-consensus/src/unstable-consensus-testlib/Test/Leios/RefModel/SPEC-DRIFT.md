# Spec.md drift — RefModel.hs is ahead

Things to reflect back into `Spec.md` (it's at the §1–§8 prose baseline; RefModel.hs has moved
substantially during the EbHashMap / offer-path work). Don't fix Spec.md yet — this is the to-do list.

## Per-`EbHash` dedup + wallclock TTL — favored; `(Slot, PoolId, EbHash)` triple is the alternative

- [ ] **Favored: keep per-`EbHash` dedup with the wallclock TTL (IMPLEMENTED in RefModel.hs).** An offer is
      a per-*content* claim ("I hold the body/closure for this `EbHash`"); the per-election information the
      receiver actually acts on (younger elections raise fetch priority; voting needs the election set)
      rides on the *announcement* stream, not on offers. So one offer per `EbHash` conveys everything an
      offer is for, and a second offer of the same content under a different election adds nothing the
      receiver will act on. Per-`EbHash` dedup suppresses that redundant traffic, and makes the frugal
      behavior the path of least resistance: an honest sender's natural lifecycle (announce, offer once when
      it holds the EB, never re-offer because it never loses-and-reacquires inside the window) is
      automatically compliant — no re-offer timer, no per-election offer bookkeeping.
- [ ] **The snag the TTL closes** (document as the TTL's motivation): with `EbHash`-only offers and no TTL,
      `RedundantOffer` correctness would be a **cross-node GC-epoch invariant**. An honest peer that GC'd
      X's old election and later re-offers X under a *new* election would be wrongly
      `RedundantOffer`-disconnected whenever our immutable tip lags its GC; symmetrically the downstream gate
      would have to be retained for the **network-max GC window W (≈36 h)** rather than our own tip (≈12 h
      immutability), decoupled from our tip in both directions.
- [ ] **How the TTL closes it.** `RedundantOffer` fires only when a non-level-raising offer arrives **within
      `envOfferDedupTtl` (~6 h)** of the peer's prior offer of that `EbHash`; a staler one is treated as a
      fresh epoch and accepted. Robust because a *legit* re-offer can't happen until the peer itself GC'd
      (its tip moved ~an immutability-window, ≥~12 h at real time), so the TTL only has to sit anywhere
      between spam cadence and immutability — every node picks its own, no W agreement. The only sub-TTL
      "legit" re-offerer is a node replaying faster than real time (syncing), which we don't want as upstream
      anyway. The TTL is a little ugly (a wallclock in the dedup) but simple, and its **safe interval is
      enormous — not a tuning challenge**. Lives on the *received*-offer state only (`stPeerOfferings`, via
      the `Time` in `LeiosNotifySide`); slot-GC still bounds memory, the TTL only governs the disconnect.
      New `Env` param **`envOfferDedupTtl`**. The level-raise case (had body, now have closure) is permitted
      regardless of the TTL (`lvl > cl`), and works cross-election because the level lives per-content.
- [ ] **Alternative (no longer the lean): the announcement triple `(Slot, PoolId, EbHash)`, dedup keyed by
      it** (or, halfway, `(Slot, EbHash)` — the CIP offer + our model already carry and confirm the `Slot`,
      so only `PoolId` is missing). It is exact and clockless: distinct elections naming the same content are
      distinct dedup keys, GC'd by our own tip with **no cross-node coupling**, so the snag above never
      arises. But the exactness is about a distinction the offer layer doesn't need: it **permits** the rare
      same-content-multiple-elections case to send one offer per election (redundant traffic the receiver
      won't act on) and **decentralizes** the dedup duty onto every sender. Per-`EbHash` and triple keying
      behave identically except in that rare multi-election case (an `EbHash` named by >1 election is
      basically equivocation/coincidence) — and there, per-`EbHash` suppresses the redundancy while the
      triple permits it. Net: the triple trades the TTL's wallclock for permitted redundancy + a sender
      burden; given the TTL's safe interval isn't a tuning problem, keep-dedup-with-TTL comes out ahead.

## New abstraction not in Spec at all

- [ ] **`EbHashMap a`** (its own module). A bidirectional, reference-counted map: per-EB payload `a`
      keyed by `EbHash`, refcounted by per-`Election` references; the **election side carries no payload**
      (the `b` parameter was dropped — `Refs = Refs !InactiveRef !EbHash`). Each
      election holds an *active* and optional *inactive* (superseded) EbHash ref; the EbHash entry's
      refcount = #elections naming it; GC by slot-major range-delete on the election side cascading to
      drop EbHash entries at refcount 0. `RefCounts` now splits **active vs inactive** counts (so
      "actively wanted/offered" is `active > 0`). `supersede` = cert switches an election's active EB.
      Spec §2 frames `LstWanted`/`LstPeerOfferings`/`LstPeerOfferGates` as plain `Election ↦ …` maps with
      the EbHash↦… reverse indexes "Derived (NOT stored)"; RefModel bakes that reverse index into
      EbHashMap (a maintained bidirectional index). The "derived/deferred" framing for
      `electionOf`/`offerersBody`/`offerersClosure` needs revisiting.
- [ ] **`HeldEbHashSet`** (its own module). The order-agnostic, **`Slot`-keyed** held-EB relation backing
      `LstVolatileBody`/`LstVolatileClosure`: a `Map EbHash RefCount` (a multiset, for O(log) `member`)
      plus a `Map Slot (NESet EbHash)` (the relation). Keyed by `Slot`, **not `Election`**: the keys are
      only consumed by slot-granular GC (`prune`), and only a `(Slot, EbHash)` reference is persisted and
      reconstructable on startup, so the `PoolId` half of an election would be dead weight here. `insert`
      is **total** — a slot normally references at most two EBs (its first-announced and its certified
      one), but slot battles and a bounded sequence of restarts can exceed that, so it never fails (the
      `EbHashMap`'s ≤2-then-`error` contract does **not** apply). `member` is by `EbHash` (multiset count
      `> 0`), independent of slot.

## §2 State

- [ ] **`LstWanted`** is now keyed by `EbHash` (an `EbHashMap WantState`), not `Election ↦ WantState`.
      `WantState` **dropped its embedded `ebHash`** (`AwaitingBody bs cs | AwaitingTxs (These …)`).
      Completion is **per-EbHash**, fanning out to every naming election (`electionsNaming`); a shared
      EbHash keeps its existing fetch progress (`Semigroup WantState` = keep-left).
- [ ] **`LstPeerOfferings`** is now `Peer ↦ PeerOfferings`, a product of an
      `EbHashMap (Maybe LeiosNotifySide, Any)` (offer levels, keyed by `EbHash`) **and** a separate
      `Election ↦ ChainSyncSide` map (per-election ChainSync RB header hashes). Not
      `Peer ↦ Election ↦ These CertSide OfferSide`.
      - `OfferSide` → **`OfferLevel`** (`OfferBody | OfferBodyAndClosure`). The per-EB payload's first
        component is `Maybe LeiosNotifySide`, where **`LeiosNotifySide = LeiosNotifySide OfferLevel Time`**
        carries the offer level *and* when it was last (re)offered (the `RedundantOffer` TTL clock);
        `offerLevel` projects the level. (`OfferLevel` has a `max` `Semigroup`.)
      - `CertSide` → **`ChainSyncSide HeaderHash`**, now held in the separate `Election ↦ ChainSyncSide`
        map (split out of the `EbHashMap` so its election side carries no payload; **dropped the EbHash** —
        the EbHash is the Refs active ref now). The cert no longer touches the LeiosNotify level. The
        recorded HeaderHash is now **read** (per-peer cert-conflict check, see §3), no longer write-only.
      - per-EB **`Any`** flag = "this EB was ChainSync-offered (cert via LevRollForward)"; kept distinct
        from the LeiosNotify level so `RedundantOffer` only consults the latter.
      - **`effectiveOffer`** = LeiosNotify level ⊔ (`Any` ⇒ body+closure), per-EB, no election scan.
- [ ] **`LstPeerOfferGates`** is now `Peer ↦ EbHashMap (Maybe OfferLevel) ()` — payload = the offer
      **level already sent** to that peer (bare `OfferLevel`, no timestamp — downstream is tip-GC), not
      `Election ↦ EbHash`. It is now a pure **dequeue-time** concern (was "open on announcement send"):
      `openGate` records announced (`Nothing`), `bumpGate` records the sent level; serves both causality
      (announced) and dedup (sent-level) — see §3.
- [ ] **`LstPeerNotifyQueue`** is now a **FIFO `Seq`** (`Peer ↦ (Seq Notification, Int)`), not a
      `notifyPriority`-ordered `Set`. Overflow = **tail-drop** (drop the newest message being added).
      Remove `notifyPriority`, the whole **Helper-functions §2 entry and its two-slot-ordered-queues
      Aside** (moot), and the `Set`/priority eviction wording.
- [ ] **NEW: `LstVolatileBody` / `LstVolatileClosure`** (a **`HeldEbHashSet`** each, its own module — see
      "New abstraction" above) — the in-memory sets of EBs whose body / full closure we hold. INVARIANT
      closure ⊆ body. Maintained: body added on `LevDiskDone` body-write, closure added on completion, and
      **a fresh first-announcement or cert extends a held EB's references** (`anchorVolatile`); **trimmed
      at `LevImmTipAdvanced`** (`pruneBelow`, now a `Slot -> Bool` predicate). `LstVolatileClosure`
      is ChainSel's queryable "available complete closures" set (no disk read); `NotifyVotingAndChainSel`
      is the "set grew" signal. **Reconstructed from disk on startup** (`initSt`; see the Startup section) —
      it is the only central state rebuilt across a restart.
- [ ] **`Req` dropped its `Election`** (`ReqBody EbHash bs cs | ReqJob EbHash JobId Job`); likewise
      `DiskWrite` (`WriteBody Body | WriteClosure EbHash [Tx]`). Spec §2 carries `el` in both.
- [ ] **`Notification`** offer ctors dropped the election and merged into one
      **`NotifyOffer EbHash OfferLevel`** (was `NotifyBlockOffer` / `NotifyBlockTxsOffer`); the `OfferLevel`
      distinguishes body vs body+closure (`offerable` / `bumpGate` take it).
- [ ] **`NotifyEquivProof RbHeader RbHeader`** carries *both* headers; the wire message keeps the optional
      first header (`MsgLeiosBlockEquivocationProof (Maybe RbHeader) RbHeader`), with the omit-or-include
      decision made at *send* time (see §3).
- [ ] **NEW offence `RedundantOffer`** — an inbound offer that raises no LeiosNotify level disconnects
      (within the TTL; see §3).
- [ ] **NEW equivocation-proof offences**: `HalfEquivocationProof` (the proof omits the first header and we
      have no recorded first announcement from that peer to fill it in) and `RedundantEquivProof` (the proof
      *includes* a first header the peer had already announced to us — it should have omitted it).
      `BogusEquivocationProof` (not a genuine equivocation) is unchanged.
- [ ] **NEW parameter `envOfferDedupTtl`** (~6 h, wallclock; the `RedundantOffer` dedup epoch, kept well
      below the immutability window) — §2 Parameters.

## §2 Messages / stimuli

- [ ] **`MsgLeiosBlockOffer(Slot, ebHash)` / `MsgLeiosBlockTxsOffer(Slot, ebHash)`** carry `(Slot, EbHash)`
      (matching the CIP), no longer the full election (Spec §2 + the §5 dedup TODO say they carry an
      election). The receiver resolves EbHash → election(s) via its own first-announcement index
      (`electionsFirstAnnouncing`, the old `electionOf`).
- [ ] **The model both provides and confirms the offer's `Slot`.** *Provide* (send): `sendNotification`
      derives the slot per-peer from `gateSlot` — an election we actually announced this EbHash to *that*
      peer under (scanned from `LstPeerOfferGates`) — so it is always confirmable by the recipient.
      *Confirm* (receive): `hOffer` filters the peer's first-announcements of the EbHash to those at the
      claimed `Slot`; an empty result ⇒ `UnannouncedOffer`. The carried `Slot` would also let dedup be
      keyed by `(Slot, EbHash)` (obviating the wallclock TTL), but we deliberately kept dedup per-`EbHash`
      + TTL for now — only `(a)` of the CIP-alignment was adopted. `notifyMsgSlot` now reads the stale
      horizon's slot straight off the offer message (the old `youngestAnnouncedSlot` is gone).
- [ ] **`MsgLeiosBlockTxsRequest(ebHash, bitfield)`** — the txs argument is now a **bitfield of positions**
      into the EB body's `bodyTxlist` (`NonEmpty Word16`, the set-bit positions), not `NonEmpty TxHash`.
      Matches the real wire's `TxBitmaps`; the model takes the abstract position-set form (no `Data.Bits`).
      `Job` carries `(Word16, TxHash)` pairs — `jobPositions` feeds the request, `jobTxs` still validates
      the response against the expected hashes. The store resolves positions itself:
      **`dbReadClosureTxs :: EbHash -> NonEmpty Word16 -> m [Tx]`** (was `[TxHash]`), so `hServeTxs` is one
      DB call + a length check (no separate body read; an out-of-range position ⇒ short result ⇒
      `RequestedAbsentData`). The response `MsgLeiosBlockTxs(ebHash, [Tx])` is unchanged (request-only change).
- [ ] **Removed `LevGarbageCollect` / `GarbageCollect` / `Promote`.** `DiskOp` is now just
      `Write DiskWrite | RecordRef Slot EbHash`. They were trivial passthroughs: `LevImmTipAdvanced` does the
      meaningful in-memory `pruneBelow` and now emits no effect, and disk-side GC/promotion is left unmodeled
      (the model never simulated the disk's response to those ops anyway).

## §3 Rules

- [ ] **`LevBlockOffer`/`LevBlockTxsOffer`**: offer carries `(Slot, ebHash)`; resolve naming elections
      filtered to the claimed `Slot` (non-empty ⇒ else `UnannouncedOffer`), raise the per-EB LeiosNotify
      level once. A non-raising offer ⇒ `RedundantOffer` disconnect **only if** the peer's prior offer of
      that EbHash was within `envOfferDedupTtl` (the wallclock TTL); a staler one is treated as a fresh
      epoch and accepted.
- [ ] **`LevNotifyDequeue`**: FIFO pop-front. Discard (no credit consumed) if stale (announce/equiv via
      header slot) **or** if it's an offer that fails the gate (`offerable`: unannounced ⇒ causality, or
      sent-level already ≥ this level ⇒ dedup). Else send; announcement → `openGate`, offer → `bumpGate`.
- [ ] **Offer emission is unconditional + gate-at-dequeue**: `LevDiskDone`/completion enqueue offers to
      **all** present peers (`enqueueToAll`), not "to gated peers"; the gate filters at dequeue. The
      `Set`'s silent dedup is replaced by the gate sent-level.
- [ ] **`LevBlockAnnouncement` / centralAnnounce** additions: `anchorVolatile` (a fresh announcement
      extends a held EB's elections in `LstVolatile*`); fetch **dedup** (skip wanting if eh already in
      `LstVolatileClosure`); **`enqueueHeldOffers`** (when relaying an announcement for an EB we already
      hold, also offer it — the late-downstream-peer fix).
- [ ] **`LevDiskDone`**: body-write adds to `LstVolatileBody` (+ body offers); completion adds to
      `LstVolatileClosure` (+ closure offers + `NotifyVotingAndChainSel` per naming election). Completion
      is per-EbHash (`completeEb`), not per-election.
- [ ] **`LevRollForward` cert bit / `recordChainSyncSide`**: `supersede` sets the per-EB `Any` ChainSync
      flag (leaving the LeiosNotify level untouched) + inserts `ChainSyncSide hh` into the per-election
      `Election ↦ ChainSyncSide` map; guarded against a repeat cert. Spec records `These (Cert …) BodyAndClosure`.
- [ ] **`CertConflict` now also fires on a peer's *own* prior certified HeaderHash.** A certified
      HeaderHash is a quorum-backed, unique-per-election fact, so a peer asserting two **different
      certified** HeaderHashes for one election (two cert-carrying `LevRollForward`s) is provably lying —
      `rollForwardCert` disconnects if the offered `atHeaderHash` conflicts with **either** our validated
      `LstCertified` cert (the prior check) **or** the peer's own recorded `ChainSyncSide` for that
      election. (Two *uncertified* announcements for one election remain ordinary equivocation — recorded
      via the equiv machinery, never a disconnect; an honest peer can relay producer equivocation.)
- [ ] **`LevBlockEquivocationProof` / `hEquivProof` + `centralEquiv`**: when the inbound proof omits the
      first header it's reconstructed from the peer's recorded first announcement (`collateH1`:
      `HalfEquivocationProof` if neither is available, `RedundantEquivProof` if the peer sent one we already
      had). `centralEquiv` canonicalizes the stored/relayed `AnnTwo` so **our own first-announced header is
      `h1`**, and `sendNotification` relays `NotifyEquivProof` with that first header dropped to `Nothing`
      whenever `announcedToPeer` shows we already sent its announcement to that peer.
- [ ] **An equiv proof for an already-certified election is dropped (fetch-moot; the cert resolves it).**
      `hEquivProof` drops an inbound one **silently — not a disconnect**, since the sender may simply be
      behind on the cert (it rides a different channel); `centralAnnounce`'s `AnnOne→AnnTwo` branch records
      `AnnTwo` but skips the relay. **TODO (Skew Fallback):** once peers offer certs, a peer that offered us
      the cert and *then* sends an equiv proof for that election is provably misbehaving ⇒ disconnect.
- [ ] **`LevCertValidated`**: a `belowTip` (settled) cert is an **early-return** — no `setCertified`,
      `RecordRef`, evict, or fetch (the election is past the immutable tip, and `stCertified`/refs are pruned
      by exactly that condition). Otherwise it uses `supersede` into the `EbHashMap` (and `deleteElection`
      when already complete); the in-place cert-switch is the EbHashMap active/inactive mechanism. The
      "already complete?" test and the body-present anchoring live in the Startup section below.

## Startup / restart reconstruction (NEW — not in Spec)

- [ ] **`initSt :: Monad m => LeiosDb m -> m St`** — on node startup, rebuild `LstVolatileBody` /
      `LstVolatileClosure` from persisted references intersected with disk presence: for each recorded
      `(Slot, EbHash)`, body on disk ⇒ add to `LstVolatileBody`; all of its closure txs present ⇒ also
      `LstVolatileClosure`. **The other central state (`LstFirstAnnouncements`, `LstWanted`, `LstCertified`)
      is deliberately NOT reconstructed.** MVP rationale: restarts are rare/bounded, so we accept the
      laxity — we may repeat some work, and an equivocation slot may transiently drive a `HeldEbHashSet`
      above two EBs (hence its total `insert`). Takes only `LeiosDb` (TxCache/Mempool can't matter at
      startup).
- [ ] **NEW `DiskOp` `RecordRef Slot EbHash`** — persists a first-announced / certified reference.
      **Fire-and-forget: it yields no `LevDiskDone`** and is not part of the persist-before-expose write
      count; the disk reclaims it by slot. Emitted on every **first
      announcement** (`centralAnnounce`'s `Nothing` branch) and every **`hCertValidated`**. (These are
      exactly the references `stVolatile*` ever holds — `electionsNaming` yields precisely the
      first-announced ∪ certified elections — so persisting just these reconstructs `stVolatile*`. OK to
      record refs for EBs whose body/closure we never acquire; `initSt` simply finds nothing on disk.)
- [ ] **NEW `LeiosDb` method `dbReadAllRefs :: m [(Slot, EbHash)]`** — read all persisted references (for
      `initSt`).
- [ ] **`hCertValidated` completeness is now in-memory; `isComplete` is deleted.** The "already complete"
      test is `heldIn eh LstVolatileClosure` (authoritative across restart because `initSt` rebuilds the
      set), not a disk read — so the handler no longer touches the disk (keeps `Ifaces` only for the
      `txCacheEvictForValidCert` hook). The old `isComplete` (a restart-time `dbReadBody` + `dbQueryPresent`
      check) is gone. The `supersedeWant` (not-yet-complete) branch additionally **`anchorVolatile`s the
      cert's election into `LstVolatileBody`** (when the body is held): this keeps the in-memory held set
      equal to what `initSt` would reconstruct from the persisted cert ref, and keeps the body anchored to
      the cert's slot so it survives GC of the announcing election's slot. (`recordCompleteCert` already
      anchored both sets.) No body-present *resume* branch was added: in steady state the in-flight
      `AwaitingTxs` progress is preserved (the `WantState` keep-left `Semigroup` under `supersede`), and the
      only restart case that re-fetches a body already on disk is the accepted MVP laxity.

## §7 TxCache occupancy (equivocation-resistant)

- [ ] **Note once per election, at the central chokepoint.** `txCacheNoteAnnouncement` moved from the
      per-peer ingress (`hAnnouncement`) to `centralAnnounce`'s first-announcement branch (the `Nothing`
      case that sets `AnnOne`). Bugfix: at the ingress it fired once *per peer*, so K adversary connections
      announcing K distinct EBs for one election forced K cache reservations (on-disk tx bytes), capped only
      per-peer — *upstream* of the central one-per-election cap. Now the cache sees exactly
      `LstFirstAnnouncements[el]`. Side-effect: rollforward-carried and self-issued announcements now prime
      the cache too (they route through `centralAnnounce`), which the per-peer placement had missed.
- [ ] **Cache hooks are election-keyed.** `txCacheNoteAnnouncement :: Election -> EbHash -> m ()` (was
      `EbHash -> m ()`) so the cache can maintain the per-election index; the reference-awareness for an
      EbHash shared across elections lives inside the cache (where the tx storage is), not in the model.
- [ ] **New hook `txCacheEvictForValidCert :: Election -> EbHash -> m ()`.** Called in `hCertValidated` on
      every valid cert, with the certified EbHash. Semantics: "election `el` is certified to `eh`; evict any
      tx data retained for `el`'s *other* (superseded) EB." Bounds on-disk to one EB per election at all
      times (transient two only during the switch): pre-cert the index tracks the first-announced; a cert to
      a different EB evicts the uncertified first-announcement. "Equivocate and never certify" is already
      handled by the note-once rule (only the first is ever retained).
- [ ] **Acquire is gated by the *active* want.** `hBlock` and `hBlockTxs` only touch the cache (and persist
      the closure) when the EB is some election's **active** want — via `activeWantStateOf` (the want-state
      iff active refcount `> 0`, one lookup that also subsumes the read of the want-state; it replaced the
      separate `activelyWantsEh`, and both supersede the old `wantsEh = isJust`, which counted inactive/
      superseded refs too). Without
      this, a late response for a job requested *before* a cert-switch/eviction would re-acquire the dead
      EB's txs (and write its closure), undoing the eviction — and an adversary can time exactly that.
      `hBlockTxs` previously had no want-guard at all on `txCacheOnAcquire`/`WriteClosure`; `hBlock` had an
      `isJust` guard, now tightened to the active test for symmetry.

## §4 Properties

- [ ] "Offers are announced or certified" now keys off the EbHashMap supersede state
      (`NoInactiveRefYet` ⇒ must be first-announced) rather than `These`/`CertSide`.
- [ ] "Announce-before-offer" is now enforced at **dequeue** (`offerable`'s gate check), and the gate also
      gives **offer dedup** (no duplicate downstream offer) and **inbound `RedundantOffer`**.
- [ ] New invariants worth stating: `LstVolatileClosure ⊆ LstVolatileBody`; gate sent-level is monotone;
      `RefCounts` active/inactive tally matches the election refs (already an EbHashMap test invariant).

## §5 Roadmap — items now DONE (move out of roadmap)

- [ ] **"Dedup offers/fetches across elections that share an EbHash"** is **done**: offers are
      EbHash-only and deduped hard via the EbHashMap; the suggested "in-memory set of currently-persisted
      bodies, akin to `LstTxCacheIndex`" is exactly **`LstVolatileBody`**. The bullet's premise (offers
      carry the election) is now obsolete.
