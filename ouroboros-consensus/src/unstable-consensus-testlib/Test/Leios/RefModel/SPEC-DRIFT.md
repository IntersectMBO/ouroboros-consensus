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
- [ ] **NEW: `LstVolatileBody` / `LstVolatileClosure`** (`EbHashMap () ()` each) — the in-memory sets of
      EBs whose body / full closure we hold. INVARIANT closure ⊆ body. Maintained: body added on
      `LevDiskDone` body-write, closure added on completion; **trimmed by Promotion** (`pruneBelow` at
      `LevImmTipAdvanced`). `LstVolatileClosure` is ChainSel's queryable "available complete closures" set
      (no disk read); `NotifyVotingAndChainSel` is the "set grew" signal. (LeiosDb may ultimately own it;
      Spec should still specify the maintenance.)
- [ ] **`Req` dropped its `Election`** (`ReqBody EbHash bs cs | ReqJob EbHash JobId Job`); likewise
      `DiskWrite` (`WriteBody Body | WriteClosure EbHash [Tx]`). Spec §2 carries `el` in both.
- [ ] **`Notification`** offer ctors dropped the election (`NotifyBlockOffer EbHash` /
      `NotifyBlockTxsOffer EbHash`).
- [ ] **NEW offence `RedundantOffer`** — an inbound offer that raises no LeiosNotify level disconnects
      (within the TTL; see §3).
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
- [ ] **`LevCertValidated`**: uses `supersede` into the `EbHashMap` (and `deleteElection` when already
      complete); the in-place cert-switch is the EbHashMap active/inactive mechanism.

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
