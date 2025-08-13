# Basics of Ouroboros Praos in Cardano

2024 November | nick.frisby@iohk.io | nicolas.frisby@moduscreate.com

## The Main Theorem

If both of the following hold

- {PraosGrinding}.
  The adversary is not expending an enormous amount of hashing power[^GrindingPrereq] (ie hashes per second), on the order of magnitude of the the entire contemporary Bitcoin network.

- {PraosHonestNetwork}.
  There is always a set X (that may vary with time) of honest Cardano mainnet nodes that satisfy all of the following.

    - {PraosSelection}.
      Each node in X promptly selects the best valid chain that has propagated to it.

    - {PraosSemisynchrony}.
      The best selection among X will always propagate to all of X within much less than 20 seconds, despite the adversary's best effort to delay it.

    - {PraosHonestMinting}.
      At the onset of a slot that elects some honest stake pools, for each such pool, exactly one node in X promptly mints exactly one block that extends the node's current selection.
      That mechanism is the only way that honest stake pools with more than half of the (effective[^GrindingWiggle]) delegated stake mints blocks.

    - {PraosEpochStructure}.
      The on-chain protocol state maintains an evolving nonce that accumulates every block's election proof.
      The elections during epoch E on a chain are determined by the value of that evolving nonce that was snapshotted on that chain at least 129600 slots (ie 36 hr)[^GenesisBump] before E and a stake distribution that was snapshotted on that chain at least 172800 slots (ie 48 hours) before the nonce was snapshotted.

    - {PraosProtocolStability}.
      Similarly, the protocol itself (eg parameters) cannot vary on a single chain with less than 129600 slots (ie 36 hr)^[GenesisBump] of warning.

then the Praos paper proves a vanishingly small failure probability for each of the following.

- {PraosCommonPrefix}.
  The 2161st youngest block on any selection in X is already and always will be on every other selection in the (evolving) X.
  (Seek out the pre-computed table of worst-case _settlement times_ in other Cardano documentation.)

- {PraosChainGrowth}.
  The 2161st youngest block on any selection in X is not older than 129600 slots (ie 36 hr).
  (When all stake is behaving honestly, the age of the 2161st youngest block will be approximately 43200 slots (ie 12 hr).)

- {PraosChainQuality}.
  Every span of 43200 slots (ie 12 hr) on a selection in X contains at least one block minted by an honest node.

[^GrindingPrereq]: Remark.
    The PhD dissertation https://eprint.iacr.org/2021/1698 established that an adversary with less than roughly 9.5% stake will not successfully grind, regardless of hashing power.
    Generally, though, more hashing power or more stake would both benefit the adversary.

[^GrindingWiggle]: Elaboration.
    A grinding adverary is effectively amplifying their portion of the stake.
    The Praos security argument requires that even their _effective_ relative stake is below half.

[^GenesisBump]: Warning.
     Ouroboros Genesis increases this by 43200 slots (ie 12 more hours).

## Grinding Attack

In a {GrindingAttack}, the adversary attempts to choose the nonce for some epoch.
Suppose the following holds simultaneously, where C is the best selection in X and C^2160 is C less its 2160 youngest blocks.

- C has between 1 and 2160 blocks after the slot that snapshots the target nonce.

- The adversary has some private chains that are longer than C and fit the schema "C^2160 … H? … D …" where H is the youngest honest block on this chain (or else C^2160 if none) and D is the youngest adversarial block on this chain that's before the snapshot.
  (The schema requires C^2160 because of PraosCommonPrefix and/or PraosEpochStructure.)

The adversary can harvest a combinatorial number of nonces from each such chain with a different H.
The method is to drop any adversarial block after H up to and including D, but not so many that the result is not longer than C anymore, else X would ignore the resulting chain.
Crucially, each such chain snapshots a different nonce.
And preferably the chain still includes at least one block after the snapshot slot, so that extensions of it don't alter the nonce.

The adversary has to expend their hashing power to calculate those nonces and additional compute power to evaluate them to find the most preferable nonce (eg the one that maximizes how many of the 432000 slots in the next epoch elect the adversary) --- that additional compute power has not yet been accounted for in any grinding analysis.
If the adversary finds a chain with a good nonce, it can release it to X, thereby improving the nonce for the target epoch.
And it can do so again, if it can find a chain that both has a better nonce and is even longer.

However, the grinding calculations can only happen in a particular time interval.
Each such chain includes some honest blocks, at least C^2160 and more than that unless the adversary is gaining elections faster than C is gaining blocks.
Therefore, these calculations cannot start until the wall clock is somewhat close to the snapshot slot.
The calculations also have a deadline, since the chain C is growing during them.
If C^2160 reaches the intersection of C and some attacking chain or C just becomes longer than the best extension of that attacking chain the adversary can muster, X will never switch to that attacking chain.

The duration of the grinding interval depends on various factors, but generally becomes longer if the honest chain is growing slower or the adversary is gaining elections faster and/or already had a big lead.
If the adversary wanted to remain covert, it'd need to continue contributing (most of) its elections to X's selections.
On Cardano, that typically means the time interval would not last significantly more than 12 hr.
Moreover, Praos would have to explicitly fail for the interval to last more than 36 hr, according to PraosChainGrowth.
(TODO that previous sentence doesn't consider that the adversary might be able to start grinding early.)
If the adversary doesn't have much stake, then it's extremely unlikely they'll have many excess elections.
That both decreases their search space and also reduces the interval since it makes them more dependent on orphaned honest blocks, which prevent the calculations from starting earlier than those orphans arrive.

(TODO integrate the fact that 100 blocks already generates enough chains to require years of hashes to compute the nonces?)

## Protocol and Leader Schedule Stability

The delays required by PraosEpochStructure and PraosProtocolStability, when combined with PraosCommonPrefix and PraosChainGrowth, ensure that all of X will always be executing the same protocol with the same the leader schedule for the current epoch.
This does not actually seem to be a fundamental requirement for the protocol itself (since the headers/blocks that use some nonce identify their preceding chain) --- and it is in particular not crucial to the preceding GrindingAttack explanation.

Instead, the Praos authors merely relied on this property to tackle the Praos proofs.
It similarly simplifies all reasoning about the protocol unless Praos has already failed in some way.
That includes third-party (community) tooling, etc.
So these two requirements provides some convenience to developers at the acceptable cost of a lower bound on governance delays.

Notably, these requirements enable the Header-Body Split optimization: the honest node can validate a block header even before having fetched or validated the preceding 2160 blocks.
(Seek out more details in other Cardano Consensus documentation.)

## Unspecified Behaviors When Praos Fails

PraosCommonPrefix is both trivial and crucial to enforce within the node---without it, the node would have to process/retain incoming data regardless of how historical it is (eg headers/blocks).

PraosChainGrowth, on the other hand, has been indirectly assumed in various places (notably PraosEpochStructure and PraosProtocolStability), but is not enforced anywhere.
It would also be trivial to enforce, but it has so far been considered unnecessary and even potentially inconvenient.

For example, suppose Amazon had a very bad outage for couple days, and the Cardano network's "X" nodes _almost_ satisfied PraosChainGrowth but came up just one block short.
If the node were to enforce PraosChainGrowth (eg it refused to select chains that violate it), then the community would have no choice but to invoke the off-chain disaster recovery plan, which has significant instantaneous costs and long-term reputational costs.
Perhaps it's better for nodes to make and best effort and possibly limp along in that scenario, crossing sparse intervals of chains as best it can (if the adversary allows it).

On the other hand, some unavoidable disasters could knock out the (public) infrastructure Cardano relies on, such that there can be no X.
EG a https://en.wikipedia.org/wiki/Carrington_Event solar storm could partition the Cardano network such that no set of connected nodes mints on behalf of a third of the delegated stake.
A PraosChainGrowth violation would be inevitable in that scenario.
In such an extreme scenario, it's easy to assume the nodes' behavior is irrelevant.
However, today's node can proceed (adversary permitting) as long as there's at least _one_ block in every 36 hr period.
Who is to say the Cardano community would never want the option to preserve that weak history of the chain instead of invoking the disaster recovery plan?

One major downside to the current scenario is that almost no specifications for nodes, community tooling, etc were ever intended to scope over behaviors when Praos fails.
Thus some simplifications/optimizations/etc are likely to fail in surprising ways during disasters, including those that are fundamentally unavoidable.
This indirectly suggests that the community itself might effectively be partitioned --- whether that was immediately recognized or only manifested in surprising disagreements among tooling after some delay --- if the historical Cardano chain itself violated PraosChainGrowth.

Thus, the _most careful_ option would be to enforce PraosChainGrowth in the node.
This does force the community to execute the disaster recovery plan even in "slight" disasters.
But doing so ensures that essentially all of the Cardano infrastructure can _safely_ continue to take PraosChainGrowth and PraosChainQuality for granted.

(Like PraosChainGrowth, PraosChainQuality is similarly assumed at least in PraosEpochStructure, but it inherently cannot be enforced.)

## Protocol Refinements/Extensions

Subsequent work on the Ouroboros protocols refines/extends the Praos theorem.

- PraosSemisynchrony indirectly precludes X from containing nodes that are still syncing.
  Praos itself does not provide any protections for nodes to catch up to the honest network, ie to (re)join X, it merely ensure the protocol can benefit from nodes that manage to do so.
  As of the Ouroboros Genesis protocol, X still excludes syncing nodes, but now an honest node will eventually be able to (re)join X after it's had access to X for long enough.
  The specific Cardano implementation of Genesis additionally requires the syncing node is never eclipsed during its sync.

- Ouroboros Peras sometimes expedites settlement times[^SettlementHashingPower] such that {PerasCommonPrefix} and {PerasChainGrowth} will reduce the 2161 count of PraosCommonPrefix and PraosChainGrowth in intervals during which Peras voting was not disrupted.

- The count of 2160 blocks was chosen for Cardano's PraosCommonPrefix etc in order to absorb the risk of the GrindingAttack.
  If that attack were somehow mitigated, the count could be reduced.
  Improved anti-grinding measures (eg involving a _verifiable delay function_ in the evolving nonce) will therefore also expedite settlement[^SettlementHashingPower].
  The 2161 count will likely not be as reduced as it is in Peras, but it will be reduced _always_ instead of _sometimes_.

- At the very least, PraosHonestMinting requires an accurate wall clock.
  Most cardano nodes currently use the Network Time Protocol (NTP) for that, but that's regarded as a vulnerability.
  Ouroboros Chronos replaces NTP with a bespoke on-chain clock.
  Even without the on-chain mechanics, Chronos informs how the Praos node should handle blocks that arrive well before the onset of their slot, for example.

[^SettlementHashingPower]: Remark.
    Expediting settlement also reduces the search space and grinding interval of the GrindingAttack.
