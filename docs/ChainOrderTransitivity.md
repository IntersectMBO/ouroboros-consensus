# Transitivity of the chain order

Remember that a relation `~` is _transitive_ if `a ~ b` and `b ~ c` implies `a ~ c` for all `a,b,c`.
It is _non-transitive_ if this is not true, ie if there exist `a,b,c` such that `a ~ b` and `b ~ c`, but not `a ~ c`.

## Context

The abstract Consensus layer currently implements the chain order like this:
From every header, one can extract a `SelectView`, which has an `Ord` instance.
A chain `C` is _preferable_ to a chain `C'` if `C'` is empty or if `selectView(H) > selectView(H')` where `H` and `H'` are the tip headers of `C` and `C'`, respectively.

## Examples for non-transitive orders

### Current chain order is non-transitive

For all Shelley-based eras, we currently use a lexicographic-ish[^lexicographic-ish] combination of the following comparisons in descending order:

 1. Chain length, preferring longer chains.
 2. If the issuer identity is the same, compare by the issue/opcert number, preferring higher values, otherwise, no preference.
 3. VRF tiebreaker[^vrf-tpraos-vs-praos], preferring lower values.

This is not a total order: The relation is reflexive, antisymmetric and total, but non-transitive.

Consider the following three `SelectView`s:

|              | A | B | C |
| ------------ | - | - | - |
| Chain length | l | l | l |
| Issuer       | x | y | x |
| Opcert no    | 2 | o | 1 |
| VRF          | 3 | 2 | 1 |

Lower-case letters stand for arbitrary values (but if they occur repeatedly, they designate the same value).

We have

  - `A < B` due to the VRF tiebreaker,
  - `B < C` due to the VRF tiebreaker, again, but also
  - `C < A` as `A` has a higher opcert number than `C`.

So we have `A < B < C < A < B < C < ...`.

### "Duncan's rule" is non-transitive

For context, see [this comment](https://github.com/IntersectMBO/ouroboros-network/issues/2913#issuecomment-816953407) and [ouroboros-consensus#524](https://github.com/IntersectMBO/ouroboros-consensus/issues/524).

Consider the following alternative rule, for simplicity without the issuer/opcert rule as it is not relevant for the point:

 1. Chain length, the longer chain is preferred.
 2. If the slot numbers differ by at most `Δ = 5` slots, compare the VRF tiebreakers[^vrf-tpraos-vs-praos], preferring lower values, otherwise, no preference.

Again, this relation is reflexive, antisymmetric and total, but non-transitive:

|              | A | B | C |
| ------------ | - | - | - |
| Chain length | l | l | l |
| Slot         | 0 | 3 | 6 |
| VRF          | 3 | 2 | 1 |

 - `A < B` due to the VRF tiebreaker, as the slots 0 and 3 are within `Δ = 5`.
 - `B < C` due to the VRF tiebreaker, as the slots 3 and 6 are within `Δ = 5`, again.
 - `C = A` as the slots 6 and 0 differ by more than `Δ = 5`.

So we have `A < B < C = A < B < C = ...`.

## History

The relevant changes to the chain order occurred in these PRs in chronological order:

 - [ouroboros-network#2108](https://github.com/IntersectMBO/ouroboros-network/pull/2108) added the opcert number comparison step as the first tiebreaker (before that, only chain length was used).
 - [ouroboros-network#2195](https://github.com/IntersectMBO/ouroboros-network/pull/2195) added the VRF tiebreaker, but lexicographically *before* the opcert number tiebreaker, in contrast to the status quo.
   This means that the order still was total at this point, also see below.
 - [ouroboros-network#2348](https://github.com/IntersectMBO/ouroboros-network/pull/2348) mostly did a change unrelated to the purpose of this document, but crucially, it swapped the order in which the VRF and opcert number comparison are done, introducing the current non-transitivity.

## Impact of non-transitivity

Fundamentally, the Praos protocol allows ties to be broken arbitrarily.
Given that we always first compare by chain length, the fundamental Consensus properties (like Common Prefix) should not be affected by the non-transitivity.

Still, there are some potential complications of non-transitivity:

 - Implementation-related.
    - Even though the non-transitivity gives rise to "cycles" like `A < B < C < A < B < C < ...` as seen above, it is guaranteed that we will never end up changing our selection like this, eg as blocks that already are in the VolatileDB are not added again.
    - We currently call `sortBy` from `base` in chain selection.
      The docs of `sortBy` state:

      > The relation is also expected to be transitive: if it is not then `sortBy` might fail to find an ordered permutation, even if it exists.

      According to some preliminary property tests, however, this is not the case for our particular non-transitive order, but it is certainly a weird state of affairs.
      For example, do we have to worry that GHC changes its implementation of `sortBy` in a way that interacts non-trivially with our non-transitive order?
    - Also, the reliance on `sortBy` is not crucial, we see several ways how to change our code such that it would do something well-defined even for non-transitive tiebreakers.
 - Reasoning-related.
    - Having a non-transitive order could turn out to be very confusing for reasoning about anything that is related to the chain order.
      This is because transitivity is so ingrained when thinking about orders that it is hard to not implicitly assume.
    - Certain "obvious" properties suddenly fail to hold, for example:

      > Repeatedly observing the selection of a node over a period of time yields a sequence that is strictly improving for every two adjacent observations of the selection.

      For example, two parties might observe two different subsequences of selections of a node over time (eg via ChainSync), and disagree whether every selection is strictly better than the one they observed immediately before.

      In particular, this might have practical effects for diffusion pipelining, which requires objectivity in this case.

## Possible solutions

One solution is to change the chain order in such a way that it is transitive and still has (mostly) the same "features" as before.

### Regarding opcert numbers

There is a simple fix for the non-transitivity caused by the opcert numbers: swap the orders in which opcert numbers and VRFs are compared, back to what it was before [ouroboros-network#2348](https://github.com/IntersectMBO/ouroboros-network/pull/2348).
The resulting order is transitive, hence total.

The motivation for the opcert number tiebreaker is described in the ["Design Specification for Delegation and Incentives in Cardano"][ledger-readme].
We describe our understanding of this use case for the opcert number tiebreaker:

Consider an attacker who corrupts an SPO's node with a specific issuer identity, including the hot key, but not the cold key.
They can now mint blocks on behalf of this issuer identity, deciding which transactions are included in the block, potentially spoiling the reputation of the SPO in the process.
(Rewards still go to the account of the issuer identity, which the attacker can not access.)
The attacked SPO can now issue a new hot key with a higher opcert number, and start a new node with it installed.
Now, in every slot where this issuer identity is elected, both the attacker and the attacked SPO can mint a block.

 - If the SPO mints a strictly longer or shorter chain, the opcert tiebreaker does not matter, citing from the aforementioned specification:
   > Note that nodes take the precedence amongst operational key certificates into account only after comparing the length of the chains.
   > When the node is already up to date and receives two conflicting blocks that add to its current chain, the length will of course always be the same.
   > But this rule is important: if we did not compare the lengths of the chains before giving preference to the block with the newer operational certificate, it would be possible to force a node to do a rollback of arbitrary length, by sending it a block from a past slot, signed using a newer certificate than the block that the node already has in its chain for that slot.
   > This would open up an attack where a stake pool operator could force nodes to do arbitrary rollbacks.
 - If the SPO and attacker instead mint chains of the same length, the opcert number tiebreaker gives precedence to the SPO as they have the higher opcert number.
   Note that this is the usual case when two nodes are elected in the same slot.

An important observation is that the VRF is the same for both blocks, as the VRF does not depend on the hot key.
This means _swapping_ the order of the opcert number and the VRF tiebreaker also satisfies the requirements of the opcert number mechanism as just described (again, this _was_ the original implementation before [ouroboros-network#2348](https://github.com/IntersectMBO/ouroboros-network/pull/2348)).

Note that there is a difference: With the current chain order, a block `B` with a higher opcert number will be preferred over an attacker's block `B'` with the same block number even if it is in a different slot, as opposed to a random tiebreak using the VRF when using the proposed order with these two tiebreakers swapped to restore transitivity.
However, this difference is irrelevant since the scenario implies that one party can just mint in the larger of the two slots on top of the other block, superseding the tiebreaker due to having a longer chain.

Remarks:
 - Given that the VRF also has all the nice properties (in particular [collision resistance][vrf-rfc-collision]) of a cryptographic hash, we could also elide the check that the issuers are the same.
   Concretely, it is computationally infeasible to construct two headers that do not agree on both their slot and issuer, but do agree on their VRF.

   The resulting chain order would then look like this:

    1. Chain length, preferring longer chains.
    2. VRF tiebreaker[^vrf-tpraos-vs-praos], preferring lower values.
    3. Opcert number, preferring higher values.

   As this is a lexicographic combination of total orders, it is a total order, so in particular transitive.
 - Both we and (more importantly) the Networking team are unaware of the opcert number mechanism ever being actually used, in particular as various entities pay close attention to blocks that are issued by the same pool in the same slot (this happened in the past, but likely for other reasons).

### Regarding Duncan's rule

We propose the following chain order with similar properties as Duncan's rule:

 1. Chain length, preferring longer chains.
 2. Slot number, preferring earlier slots.
 3. VRF tiebreaker[^vrf-tpraos-vs-praos], preferring lower values.
 4. Opcert number, preferring higher values.

As this is a lexicographic combination of total orders, it is a total order, so in particular transitive.

A few remarks about this rule:

 - SPOs can't influence in which slots they are elected, just as they can't influence their VRF tiebreakers.

   For example, if node `X` mints block `B` in slot `s` and node `X'` mints `B'` in slot `s+1` with `blockNo(B) = blockNo(B')`, then with both Duncan's rule and the status quo, the VRF tiebreaker would decide probabilistically who wins the "battle" and will hence likely be extended by future blocks.
   With the rule proposed here, however, `B` will always win against `B'`. Note that any node will on average end up in the role of `X` just as often as in that of `X'`.
 - The proposed rule has the potential advantage of favoring blocks that (due to their earlier slot) had more time to diffuse through the network and hence reach the next block issuer.
 - Arrival times don't matter, so we keep the benefit of discouraging centralization that using VRFs as the sole tiebreaker (apart from opcert numbers) had.
 - If a node mints a block `B`, and another block mints another block several (eg `Δ = 5` slots) later, but does not extend `B` as it should have (empirically, this seems to be due to underresourced nodes/relays), then we will now prefer `B`.
   This was one of the motivations of Duncan's tiebreaker, compared to the status quo.

[^vrf-tpraos-vs-praos]: TPraos used the leader VRF, while Praos uses the VRF prior to range extension, see [ouroboros-network#4051](https://github.com/IntersectMBO/ouroboros-network/issues/4051), but this shouldn't matter for this discussion.

[^lexicographic-ish]: Usually, one only considers the lexicographic order constructed out of orders that are at least partial. However, the order "compare opcert numbers when the issuers are identical, otherwise, consider equal" on pairs of issuer identities and opcert numbers is not a partial order as it is non-transitive. Still, the same general principle applies.

[ledger-readme]: https://github.com/IntersectMBO/cardano-ledger/blob/master/README.md
[vrf-rfc-collision]: https://www.rfc-editor.org/rfc/rfc9381.html#name-trusted-uniqueness-and-trus
