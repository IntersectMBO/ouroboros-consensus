# Civic Time in the Cardano Node

## Introduction

This document discusses how civic time (eg a value the wall clock could report) relates to the Cardano node, both the current design details and the higher-level fundamental needs.

It also raises the question of what behaviors the node should exhibit when the Praos security argument has failed.

## Pre-Ouroboros Chronos

The Ouroboros Chronos protocol has not been implemented, but some of its basic rules have already been implemented.
Today's node behaviors related to Chronos can be summarized as follows (see [this document](handling_blocks_from_the_future.md) for details).

- The node trusts NTP.
  (A full Ouroboros Chronos implementation would not.)

- The node silently ignores the wall clock moving backwards by a small amount.
  It crashes if the wall clock moves backward by a large amount.
  (This would be an NTP failure/attack vector.)

- The node enforces a small bound for acceptable clock skew with respect to some peer's apparent clock.

- The key indicator of a peer's apparent wall clock is the reception of a header from that peer whose slot onset is ahead of the local wall clock.
  If the header's earliness is beyond the acceptable clock skew, the peer is considered buggy or dishonest; the node disconnects with prejudice.
  If it's instead within bounds, the ChainSync client for that peer is paused until the header is no longer ahead of the local wall clock.

An honest node will not fetch a block before validating the corresponding header, so the above rule prevents a node from receiving a block before the wall clock has reached the onset of its slot.

*Aside*.
It is possible that a block from the future is already in the database when the node starts.
That's a corner case that seems unlikely to matter in practice.

## Time Translations

The Ouroboros Praos protocol and the Cardano details built around it are almost exclusively defined in terms of slots rather than common civic measures of time, such as POSIX time, UTC, etc.
Each block explicitly inhabits some slot.
The same is true for block headers and the election proofs therein.
For the sake of determinism, transactions are labeled with a range of slots in which the transaction can be valid.

The are only two exceptions.

- The node's (commodity) operating system cannot compute the current slot, merely the current civic time.

- The Plutus interface exposes the validity range as POSIX time, in the [`txInfoValidRange` field](https://plutus.cardano.intersectmbo.org/haddock/latest/plutus-ledger-api/PlutusLedgerApi-V1.html#t:TxInfo) (see this [blog post](https://www.iog.io/news/time-handling-on-cardano-part-2-use-cases) for high-level background).
  The Consensus Layer design would have been simpler if the Plutus API provided the validity range in terms of slots, but that ship has sailed.
  (And developers writing Plutus scripts are almost certainly relieved that it did.)
  The "Slot to time translation" paragraph within the Alonzo Ledger specification [`alonzo-ledger.pdf`](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/alonzo-ledger.pdf) explains that this design prevents script authors from assuming the wrong translations between slot and civic time.

Because of those exceptions, the Consensus Layer must use the wall clock and/or the translation back and forth between slots and civic.
Those uses are listed in the following table.

 - Every use involves some translation, whether it be from slot-to-civic or vice versa (which is always the wall clock as a slot).

 - This table excludes some uses that the Consensus Team is in the processing of removing (eg those that could be replaced by annotating validated headers with their slot onset).

 - The rightmost column of the table judges whether the use obviously involves some entity that obviously determines which ledger state to use for the translations.
   The column is for the benefit of a section below, but the key idea is that the translations are inherently chain-dependent, but users and most developers are blissfully unaware of that and, moreover, such a dependency is fundamentally contrary to human intuitions about time.

 - The last few rows make explicit Consensus features that are not yet implemented but will also involve civic time.

| Component | Use | Reads wall clock (whether translated to slot) | Which slots are translated to civic | Whether which ledger state is "obviously" determined |
| - | - | - | - | - |
| ChainSync | enforce the clock skew bound | Yes (raw) | header's slot | Yes, header's intersection [^HeaderBodySplit] |
| ChainSel | define Plutus `txInfoValidRange`s when validating a block | No | validity range | Yes, the block |
| The Mint | checking whether to mint | Yes (translated) | none | Yes, the new block |
| The Mint, via `getSnapshotFor` | recheck validity range[^MempoolAdd] | Yes (translated) | none | Yes, the new block |
| Genesis State Machine, aka GSM | detect that the selection is an old chain | Yes (raw) | selection's slot | Yes, the selection |
| hypothetically[^MempoolAdd]: Mempool (add and resync) | enforce the validity range | Yes (translated) | none | No, dangling txs do not |
| Mempool (only add[^NoResyncTranslation]) | define Plutus `txInfoValidRange` when validating a dangling tx | No | validity range | No, dangling txs do not |
| LocalStateQuery | the `GetInterpreter` query | No | arbitrary slots | ⚠No⚠[^GetInterpreterWild] |
| Ouroboros Peras | ? analogs of ChainSync + The Mint (+ Mempool?) for votes | ? Yes (both)| ? vote slot | ?[^GiorgosIdea] |
| Ouroboros Leios | ? analogs of ChainSync + The Mint (+ Mempool?) for IBs and EBs | ? Yes (both) | ? header slot | ?[^GiorgosIdea] |
| Mithril | ? analogs of ChainSync + The Mint (+ Mempool?) for votes | ? Yes (raw?) | ? | ? |

[^HeaderBodySplit]: Elaboration.
    The node is allowed to ignore headers that intersect more than k blocks back from its current selection.
    So a relevant header's intersection will be one of the youngest k+1 blocks of the selection.
    Also, also, the k+1st headers after that extension are completely sufficient to determine whether the node ought to switch to chain with those headers.
    Thus the youngest k+1 blocks of the selection must be able to translate the slot times of a chain of up to k+1 relevant headers that extend that block but not its successor on the selection.
    (Being able to translate one ChainGrowth stability window into the future suffices to ensure the second part.)

[^MempoolAdd]: Explanation.
    Today's Mempool doesn't actually use the wall clock to enforce the validity range; it instead uses the slot after the selection's tip.
    But it might be preferable to use the wall-clock.
    The recheck in the Mint means there's no risk of minting a block with a stale tx.

[^GetInterpreterWild]: Warning.
    The consumer of the `GetInterpeter` result could use it for anything.
    Remarkably, the fact that the LocalStateQuery mini protocol forced the consumer to explicitly acquire a concrete ledger state (aka a point) before it could even issue the query is not obvious to the user of the consumer.
    One notable known use is the CLI tool's computation of the upcoming the leadership schedule.
    That tool's UX does not currently loudly indicate that the result depends on which ledger state answered the query and that the output could therefore be different if the same question is asked, even during the same epoch.

    This same thing is true of some other queries, but they all have arguments that are obviously chain-dependent.

[^NoResyncTranslation]: Explanation.
    Only Plutus scripts require `txInfoValidRange`, but resyncing the Mempool doesn't re-execute Plutus scripts.
    See "Two-Phase Transaction Validation for Phase-2 Scripts" in the Alonzo ledger spec.

[^GiorgosIdea]: Note.
    We briefly raised this concern/request with Giorgos Panagiotakos.
    He brainstormed an idea of perhaps recording the relevant nonce in vote/header.

## Some Time Translations Cannot Be Predicted

Different eras of the chain can have different slot durations.
Therefore it must be the responsibility of the Hard Fork Combinator (HFC) to define translation back and forth between slots and civic.
Even with all available information, some translations involving the future cannot be predicted.

Recall that the Byron era of Cardano sets the slot duration to 20 seconds, while all other eras so far set it to one second.
Because eras can have different slot durations, a ledger state is fundamentally unable to correctly translate times _arbitrarily_ ahead of its own slot.
Even if that ledger state were to be perfectly recent (ie its slot is very near the wall clock), eras that have not yet been implemented/designed/even considered could have an unpredictable slot duration.

Using a ledger state that is perfectly recent but otherwise arbitrary, how far ahead could the HFC do translations that are necessarily correct?
Ultimately, it depends on how quickly the net could fork to an era with a different slot duration.
In practice, that's a matter of months on Cardano mainnet[^EmergencyRapidity].
In theory --- assuming only that the Praos security argument holds and that honest stakeholders wouldn't vote for the hard fork unless their nodes were already ready for it --- it's one stability window less than the lower bound on the duration between the current era's voting deadline and the proposal being enacted.

  - In Byron, that lower bound was originaly 2k slots = 4320 slots = 86400 seconds = one day, but was doubled to two days before the fork to Shelley happened.
    So the lower bound on translations was at least one day ahead, since the Byron stability window was also 2k slots.

  - After Byron and before Conway, it was 6k/f slots = 259200 slots = 259200 seconds = 3 days.
    So the lower bound on translations was at least 1.5 days ahead, since the post-Byron stability window has always been 3k/f slots.

  - Since Conway, it's been one epoch = 10k/f slots = 432000 slots = 432000 seconds = 5 days.
    So the lower bound on translations is currently at least 3.5 days ahead, since the stability window is still 3k/f slots.

Those lower bounds are for an arbitrary ledger state, and so they're the "worst-case".
For specific ledger states, their detailed location in the epoch can increase the lower bound by up to one epoch, which has always been 5 days in every era so far.

The limit is one stability window less the the post-voting buffer because --- assuming only the Praos security argument --- that's the upper bound on the age of a block the honest Praos node might need to discard as part of a rollback.
In particular, until the oldest such block is after the voting deadline, a rollback could switch to a chain with a different voting outcome in that epoch.
For example, switching from a chain in which the next era is only a few slots away to one in which it is at least another epoch away.

*Aside*.
On the other hand, when using a (non-recent) ledger state in a historical era, the HFC could theoretically use its knowledge about the upcoming eras in order to translate even further ahead.
For example, the HFC could safely assume the one second duration for slots between some given Babbage ledger state and however many slots it might possibly take before that ledger state could transition from Babbage to Conway and from Conway to whatever comes after, because that mystery era is the first time the slot duration could change.
The HFC does not do this in practice, since the extra complexity is not worthwhile; it's not necessary for syncing nodes to be smarter than caught-up nodes.

[^EmergencyRapidity]: Technicality.
    It could perhaps be weeks or days in an emergency, but a change to the slot duration would almost certainly be avoided in that case.

## What About Chain Growth Failures?

At a high-level, today's HFC was derived as follows.
The exact thought process was not recorded during the design work, but this is a plausible reconstruction.

  - {ImmutableTranslations}.
    Because of humans' general assumptions about time, it would be prohibitively confusing if the node (even its internal interfaces) might give different translations of the same slots/civic times.
    So a switch from `t1` to `t2` for the result of some translation is unacceptable.
    At the very least, the node should refuse to do a translation that might be invalidated by on-chain governance in the meantime (eg several weeks into the future).

  - {NonemptyForecastRange}.
    On the other hand, the node should be able to translate slot/civic times slightly ahead of its tip.
    At the very least, some users presumably want some capacity to plan ahead (eg to smartly schedule their node's brief downtime).

  - {RollbackInsensitiveTranslations}.
    As a specific subcase of the first requirement, the node should even refuse to do translations that might be invalidated by a rollback.

  - {MonotonicTranslations}.
    It would also be too confusing if a node agreed to translate some slot/civic time and then subsequently refused to translate that same slot/civic time.
    So `Just t1 -> Nothing` is also unacceptable for some translation, not merely `Just t1 -> Just t2`.

  - {DespiteChainGrowthViolation}[^Anachrony].
    All of the above must hold even in the presence of a Chain Growth violation, except switching from a chain that violates Chain Growth to one that does not is allowed to violate the RollbackInsensitiveTranslations subcase of ImmutableTranslations.

[^Anachrony]: Clarification.
    This was not originally a "requirement", but the behavior was eventually discovered and has so far been accepted as reasonable and potentially even desirable.

**{Iteration 1}**.
The ImmutableTranslations (excluding RollbackInsensitiveTranslations) and NonemptyForecastRange requirements are simple to achieve without the rest.

  - Refuse to translate a slot/civic time that is after the enactment of a governance outcome if using a ledger state that is before the corresponding voting deadline.

  - Require that a governance outcome is enacted at least X slots after the corresponding voting deadline.
    Thus a ledger state can always translate at least X slots ahead of it.

**{Iteration 2}**.
The RollbackInsensitiveTranslations requirement can be additionally supported with simple changes.

  - Refuse to translate a slot/civic time that is after the enactment of a governance outcome if using a ledger state that is less than one stability window after the corresponding voting deadline.
    Just as with the third phase of the Praos epoch structure (which fixes the nonce for the next epoch), this one stability window buffer ensures --- via Chain Growth --- that there will be so many blocks after the deadline that no rollback could alter the governance outcome.

  - Accordingly require that X = one stability window + Y.
    Thus a ledger state can always translate at least Y slots ahead of it.

The MonotonicTranslations requirement is motivated by the following scenario.
The node may switch from a chain that is at least one stability window past the voting deadline to a chain that is not.
With such a switch, the node's translation for an argument after the outcome is enacted would switch from `Just t1` to `Nothing`, according to the above rules.
Due to Praos Chain Growth, the first chain must already have enough blocks to prevent rollbacks from reaching the voting deadline (since it returned `Just t1`).
And the selection rule would therefore ensure that the second chain would have at least as many blocks after the deadline.
So once the second chain grows past the threshold slot again, the translation would switch back from `Nothing` to `Just t1` (ie the same translation).
However, that intermittent `Nothing` is exactly what MonotonicTranslations prohibits.

**{Iteration 3}**.
A pivot from slot counting to block counting can achieve this, since a chain switch can't rollback more than k blocks nor decrease the block count.

  - Keep X = one stability window + Y.

  - Refuse to translate a slot/civic time that is after the enactment of a governance outcome if using a ledger state that is less than k+1 blocks after the voting deadline.

**{Iteration 4} (latest)**.
In order to satisfy the DespiteChainGrowthViolation requirement, today's HFC inlcudes a radical rule.

  - Silently ignore the on-chain governance --- ie the HFC continues with the current era _despite the on-chain governance outcome having signaled the transition to the next era_ --- if the stability window after the voting deadline contains less than k+1 blocks (ie violates Chain Growth).
    (TODO this is today's intended behavior, but a bug is counting all blocks after the voting deadline instead of only those in the subsequent stability window.)

  - Keep X = one stability window + Y.

  - Refuse to translate a slot/civic time that is after the enactment of a governance outcome if using a ledger state that is both less than k+1 blocks after the voting deadline and also less than one stability window past the voting deadline.
    Ledger states that are more than a stability window after the deadline but have fewer than k+1 blocks after the deadline do translations assuming the next epoch is in the same era (regardless of the actual on-chain governance outcome).

Altogether, Iteration 4 ensures that the node will satisfy ImmutableTranslations, NonemptyForecastRange, and MonotonicTranslations as its selection grows without rolling back any blocks (thereby excluding RollbackInsensitiveTranslations), even if those extensions violate Praos Chain Growth.

The Consensus Team would like to remove at least the DespiteChainGrowthViolation requirement and the corresponding possibility of ignoring the on-chain governance for the following reasons.

  - The occasional clarification to colleagues that "the HFC might override the on-chain governance" has always been met with (reasonable) alarm and confusion.

  - Even despite the extreme measure of overriding the on-chain governance, the HFC still is unable to ensure _all_ of its desiderata in the presence of a Chain Growth violation.
    (It's not yet clear whether it would ever be possible to do so.)

  - It seems unlikely that the Ledger Team would agree that it is worthwhile to upstream the block counting logic such that Chain Growth violations prohibit the governance outcomes relevant to the HFC.
    Making the on-chain governance itself detect Chain Growth violation (and specifying as much in the community documentation about governance) seems more reasonable than enabling HFC ledger states that are incoherent in surprising ways (ie increased major protocol version but still in the same era).
    (For the record, the Consensus Team would be on board with this if others consider this option worthwhile.)

  - The rightmost column in the table above indicates that the most fundamental elements of the node (headers and blocks) inherently determine which ledger state must determine their time translations and therefore can freely rely on time translations (likely via a hybrid of Iteration 1 and Iteration 2) that vary depending on which ledger state is used.

  - The other node functions in that table (Mempool and user queries, for now) could prevent sensitivity to rollbacks by merely translating times according to the node's immutable tip ledger state instead of ever overriding the on-chain governance.

It is not yet clear what all the resulting node behaviors would be during a Chain Growth violation --- nor whether it matters!
The node's behavior outside of the Praos security argument is very rarely discussed or even considered.
