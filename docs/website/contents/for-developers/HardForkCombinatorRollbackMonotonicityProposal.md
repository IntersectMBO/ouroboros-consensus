# DRAFT NUMBER 2

# Introduction

The Babbage->Conway transition on an earlier iteration of SanchoNet revealed a bug in the Consensus layer by which the Hard Fork Combinator transitioned to Conway because the relevant Babbage protocol parameter update proposal received sufficient votes but then that same proposal wasn't actually enacted: the major protocol version remained unchanged even after Conway started.

Troubleshooting this bug and moreover determining how to fix it in such a way that makes similar mistakes less likely in the future has lead to the following mini-framework for understanding the HFC's behavior on a particular chain.

So many dimensions arose from investiating such a relatively simple misbehavior for two reasons.
First, the mechanisms implementing these policies are---at least in the current implementation---somewhat interdependent.
Second, the HFC (including its instantiation for Cardano) remains underdocumented since back when it was originaly developed under time-pressure, and our team no longer includes its inventor.
The Consensus Team learns new things and unfortunately re-learns some old things every time they dive in to alter it.
(Optimistically, Nicolas Frisby feels they currently understand it now better than they have ever before.)

## Dimensions

The following discrete variables characterize dimensions of the HFC design space.
In this context, the _determination_ is the `singleEraTransition` method: it's how an era's ledger state determines when the era ends.

- DST - Determination is Sensitive to Ticks  - whether ticking the ledger state can change its determination of the end of its era: Sensitive vs Insensitive

- CCT - Criss-Crossed Ticks - whether an era's tick rules might be used to tick across slots that are in _other_ eras: Allowed vs Disallowed

- PIL - Potential to Ignore the Ledger - whether the HFC can ignore the ledger's hard fork-related governance: Possible vs Impossible

- DSR - Determination is Sensitive to Rollback - for chains C and D where C < D and len(C) <= len(C /\ D) + k, whether the current era of C can have a determined end on C but an undetermined or different end on D: Sensitive vs Insensitive

- ZEE - Zero Epoch Eras - whether an era (in its entirety) can contain zero epochs: Allowed vs Disallowed

- ZBE - Zero Block Eras - whether an era (in its entirety) can contain zero blocks: Allowed vs Disallowed

- QSR - Queries are Sensitivity to Rollback - same as DSR but for HFC-related Node-To-Client queries instead of for validity of blocks: Sensitive vs Insensitive

Note the following relations.

- ZEE=Allowed implies ZBE=Allowed.
- DSR=Insensitive implies QSR=Insensitive.

## Table for the Current Cardano HFC

The following table and associated notes classifies the current HFC implementation in terms of these dimensions.

| DST | CCT | PIL | DSR | ZEE | ZBE | QSR |
| --- | --- | --- | --- | --- | --- | --- |
| I   | A   | P 1 | I   | 2   | 3   | I   |

- (1) It's Possible, but only if there's a Chain Growth violation at a particular time during the epoch.

- (2) The current HFC's behavior is inconsistent without making further assumptions.
  Overall, ZEE is allowed, but it might cause some unexpected behavior.
  In particular, forecasts always anticipate that there is at least one safe zone of slots in an era (which is not true for eras with zero epochs).
  That mismatch could manifest as incorrect time-slot translations eg (within ledger rules execution, but not within Node-To-Client query reponses).

- (3) ZBE is allowed when ticking a ledger state; the HFC will invoke multiple translation functions in sequence.
  But cross-era forecasting cannot skip eras, so no headers after an epoch with zero blocks could be validated, and so blocks could not progatate in the net.
  Moreover, today's node would not even mint blocks, since the leadership check (at least currently) relies on forecasting.

## Table for an Particular Hypothetical Cardano HFC

The following table and associated notes classifies an hypothetical HFC implementation in terms of these dimensions.
Nicolas Frisby anticipates this implementation would be easier to understand, less likely to surprise users, more general (ie place fewer requirements on the block types), and not require excessive code changes.

| DST | CCT | PIL | DSR | ZSE | ZBE | QSR |
| --- | --- | --- | --- | --- | --- | --- |
| S   | D   | I   | S 2 | D   | D 1 | I 3 |

- (1) ZBE would be achievable for a prefix of eras when constructing the initial HFC ledger state (in the `ProtocolInfo` passed to the Consensus layer), but otherwise the HFC as a block type does not need to support ZBE at all.

- (2) Esgen points out that perhaps DSR could/should still be Insensitive, and that wouldn't obviously lose too much simplicity.

- (3) Having QSR=Insensitive despite DSR=Sensitive requires an additional mechanism.
  In this design point, that is anticipated to be constraining these queries to only be answered via an immutable ledger state.

## Disclaimer

There may be other points in the design space worth considering.
(Or even other dimensions, of course.)
In particular, DST=Insensitive and PIL=Impossible could be achieved by changing the Cardano ledger's governance to match the existing Cardano HFC logic.
Specifically, if the ledger did not enact governance actions unless there have actually been k+1 blocks since they were ratified even despite possible Chain Growth violations, then the Cardano HFC would never override the Cardano ledger.

However, Frisby thinks such block counting (and the related "double-stability" in the existing ledger rules) is strictly undue complexity that should be removed instead of propagated even deeper.

---

# DRAFT NUMBER 1

## Introduction

This document motivates a simplification of the HFC, that would also alleviate the "double stability" constraint on the ledger's governance rules.

Specifically, it suggests that the HFC should not be vetoing the ledger's decision to transition to the next era while that decision is not yet immutable.
This veto can only happen in the presence of a Chain Growth violation, but it's confusing none-the-less and requires extra implementation complexity in the HFC.

The suggested simplification will necessarily change some behaviors, but notably the behaviors that principally motivated the HFC's block counting in the first place could be preserved by only answering time-related queries using the immutable ledger state.

## Though Experiment: Predictions within a Simplified Node

Consider a simplified node.

- Only node-to-node protocols: no queries, etc.
- Pure Praos: no hard fork combinator, no Transitional Praos, etc.

### Question 1

Which aspects of the protocol and ledger's future behavior must this node be able to predict from a given ledger state and why?

- Assume it's well-connected and running in a healthy net.
- Assume k > 0.
- Let C be the current best valid chain in the net.
- Let D be the node's currently selected chain, and assume it's worse than C.
- Let L be the ledger state resulting from the youngest block that is on both C and D, aka their intersection.
- Let R count the number of blocks after L on D.
  Observe that there must be more than R blocks after L on C, since C is better than D.
  (This isn't necessarily true, due to tiebreakers.
  However, even with tiebreakers, this remains true in the worst case, which is what ultimately matters.)
- Common Prefix ensures R <= k.
- L must contain enough information for the node to correctly judge the validity of the k+1th block after L on C.
- Otherwise the Header-Body Split would prevent the node from adopting C, even though doing so requires at most rolling back k blocks.

That is a complete answer.
In particular, there is nothing else this simplified node needs to predict.

If we assume Chain Growth in addition to Common Prefix, then we can further refine the answer.

- Chain Growth ensures that every window of scg (ie "the s parameter of the papers' CG property", aka "one stability window") slots will contain at least k blocks on C.
- Because of the Header-Body Split, L must contain enough information for the node to correctly judge the validity of any header that is no more than 3k/f after the first block after L on C.

Note that, if necessary, L can be ticked in order to validate the first header after L on C, without requiring any predictions.
In the absence of the corresponding block body---ie because of the Header-Body Split---the assessment of subsequent headers on C will require predictions.
Thus a prediction range of at least 3k/f ensures at least k blocks after the first header, and hence k+1 blocks in total, as necessary.

### Question 2

What behaviors could arise only if Chain Growth were violated?

- For example, suppose R=k and the k+1th block after L on C was more than 3k/f slots after the first block after L on C.
- In this case, the node might not be able to switch from D to C despite R <= k.
  It can't switch to C without downloading the R+1 blocks, which it can't do without validating the R+1 headers, which it can't do without predicting more than 3k/f slots after the first header, which it might not be able to do.

For example, suppose that L is the result of a block in relative slot number 7k/f - 2 within some epoch (ie "slightly more than scg before the end of the epoch").
Because there might be a block in relative slot 7k/f - 1---which would change the nonce used for the next epoch---L is unable to validate headers in the next epoch.
If the first header after L on C is indeed in slot 7k/f - 1, then Chain Growth ensures the k+1th header after L on C will still be in this same epoch, and so L can validate it.
If the first header after L on C is instead after slot 7k/f - 1, then ticking L to that first header will tick it past slot 7k/f - 1, and so determines the next epoch's nonce.
That's likely sufficient for the next k headers, unless there is an extremely severe Chain Growth violation.
(In the non-simplified node, the prediction is always restricted to exactly 3k/f slots, despite the data in question actually advancing an entire epoch at a time.
Thus regardless of where the first header is, any Chain Growth violation in the 3k/f slots after it would prevent switch to C when R=k.)

That is a complete answer, but there are a few particulars worth emphasizing.

- No other behavior of the simplified node will change due to a Chain Growth violation.
  (This is actually false, but only because we use predictions in the leadership check.
  One option to make this true is to instead use ticking in the leadership check.
  Another option is to also characterize here how Chain Growth violations affect the predication-based leadership check, which is even milder than the above: it only manifests when there are zero blocks in some 3k/f slot window, not merely any number of blocks less than k.)
- It's worth explicitly stating that there's never a risk of the node incorrectly judging the validity of a header.
  At worst, the node won't be able to make any judgement of the header at all, since it will recognize that doing so correctly would require information that it cannot predict.
- It's entirely possible the node's behavior wouldn't even reveal that there was a Chain Growth violation!
  It's only guaranteed to fail if the correct Ouroboros Praos behavior were to require a k-deep roll back, which itself is an event may remain extremely rare despite whatever conditions caused the Chain Growth violation.
  For example, suppose 90% of stake suddenly shut down their nodes, but somehow the remaining 10% was still well-connected and there remained a sufficiently dominant honest majority.
  With just 10% of stake, it's almost certain that there would be less than k blocks in 3k/f slots, ie a Chain Growth violation.
  However, it's also almost certain there would be hundreds of blocks in every window of 3k/f slots.
  And, since the remaining nodes are still sufficiently well-connected, Praos's natural short forks would indeed remain short.
  Thus R would remain much smaller than k, such that any node that needed to switch from D to C would still be able to validate R+1 headers after L on C despite not being able to predict more than 3k/f after the first header after L on C.

## Back to Reality: the Cardano Node

First, instead of merely pure Praos, consider a node running Cardano's hard fork combinator and its history of eras from Byron through Babbage.
The answer to Question 1 actually does not change.
One crucial reason is because Cardano sets the hard fork combinator safe zone of each era to that era's stability window.
If the safe zone were instead less than one stability window, then L does not necessarily contain enough information to validate the R+1th header after L on C, _even without_ a Chain Growth violation.
Specifically, the node will need to know if the chain will actually be in whichever era that header claims to be in, and it wouldn't necessarily be able to answer that question more than a safe zone after the first header after L on C.

The answer to Question 2 does change: there is one additional change in behavior possibly enabled by a Chain Growth violation.
Unlike the ledger, the hard fork combinator actually counts blocks instead of only slots.
If a hypothetical roll back of k-many blocks could possibly change the ledger-based governance decision that was intended to trigger the era transition---which is true if Chain Growth was (severely) violated in the last two stability windows before the intended era transition---then the hard fork combinator will not transition to the next era.
(For this reason, the voting deadline was moved from 7k/f to 4k/f (ie two stablity windows before the end) in the ledger governance rules, so that the hard fork combinator's final decision would be known at least a stability window (and so the safe zone) before the epoch transition.)
Awkwardly, the ledger will still update the major version of the protocol version protocol parameter, but the chain will remain in the previous era, which many users and developers would find counter-intuitive.

So this incoherence can arise only due to a Chain Growth violation (and a specific and severe one, at that), which means the hard fork combinator itself changes the answer to Question 2.
Otherwise, the behavior will be the same as the simplified node.
The risk of this awkward incoherence is more easily motivated below in the context of the other simplification.

Second, the real node also answers queries over a node-to-client protocol.
This simplification changes neither the answer to Question 1 nor to Question 2 compared to the simplified node.
However, it would add an additional difference to Question 2 were it not for the hard fork combinator counting blocks instead of slots.

Here is today's Cardano node's full list of queries, including the not-yet-released Conway.

- Queries for any block

        ++GetSystemStart  :: Query blk SystemStart
        **GetChainBlockNo :: Query blk (WithOrigin BlockNo)
        GetChainPoint     :: Query blk (Point blk)

- Queries for the hard fork combinator

        *+GetEraStart    :: EraIndex (x : y : xs) -> BlockQuery (HardForkBlock (x : y : xs)) (Maybe Bound)
        *+GetInterpreter :: BlockQuery (HardForkBlock (x : y : xs)) (History.Interpreter (x : y : xs))
        GetCurrentEra    :: BlockQuery (HardForkBlock (x : y : xs)) (EraIndex (x : y : xs))

- Queries for the Byron era

        GetUpdateInterfaceState :: BlockQuery ByronBlock UPI.State

- Queries for each era after Byron

        GetLedgerTip                            :: BlockQuery (ShelleyBlock proto era) (Point (ShelleyBlock proto era))
        GetEpochNo                              :: BlockQuery (ShelleyBlock proto era) EpochNo
        GetNonMyopicMemberRewards               :: Set (Either SL.Coin (SL.Credential 'SL.Staking (EraCrypto era))) -> BlockQuery (ShelleyBlock proto era) (NonMyopicMemberRewards (EraCrypto era))
        GetCurrentPParams                       :: BlockQuery (ShelleyBlock proto era) (LC.PParams era)
        GetProposedPParamsUpdates               :: BlockQuery (ShelleyBlock proto era) (SL.ProposedPPUpdates era)
        GetStakeDistribution                    :: BlockQuery (ShelleyBlock proto era) (SL.PoolDistr (EraCrypto era))
        GetUTxOByAddress                        :: Set (SL.Addr (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)
        GetUTxOWhole                            :: BlockQuery (ShelleyBlock proto era) (SL.UTxO era)
        GetFilteredDelegationsAndRewardAccounts :: Set (SL.Credential 'SL.Staking (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))
        GetGenesisConfig                        :: BlockQuery (ShelleyBlock proto era) (CompactGenesis (EraCrypto era))
        GetRewardProvenance                     :: BlockQuery (ShelleyBlock proto era) (SL.RewardProvenance (EraCrypto era))
        GetUTxOByTxIn                           :: Set (SL.TxIn (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (SL.UTxO era)
        GetStakePools                           :: BlockQuery (ShelleyBlock proto era) (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))
        GetStakePoolParams                      :: Set (SL.KeyHash 'SL.StakePool (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (Map (SL.KeyHash 'SL.StakePool (EraCrypto era)) (SL.PoolParams (EraCrypto era)))
        GetRewardInfoPools                      :: BlockQuery (ShelleyBlock proto era) (SL.RewardParams, Map (SL.KeyHash 'SL.StakePool (EraCrypto era)) (SL.RewardInfoPool))
        GetPoolState                            :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era))) -> BlockQuery (ShelleyBlock proto era) (SL.PState era)
        GetStakeSnapshots                       :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era))) -> BlockQuery (ShelleyBlock proto era) (StakeSnapshots (EraCrypto era))
        GetPoolDistr                            :: Maybe (Set (SL.KeyHash 'SL.StakePool (EraCrypto era))) -> BlockQuery (ShelleyBlock proto era) (SL.PoolDistr (EraCrypto era))
        GetStakeDelegDeposits                   :: Set (StakeCredential (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (Map (StakeCredential (EraCrypto era)) Coin)
        GetConstitution                         :: BlockQuery (ShelleyBlock proto era) (Maybe (LC.Constitution era))
        GetGovState                             :: BlockQuery (ShelleyBlock proto era) (LC.GovState era)
        GetDRepState                            :: Set (SL.Credential 'DRepRole (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (Map (SL.Credential 'DRepRole (EraCrypto era)) (SL.DRepState (EraCrypto era)))
        GetDRepStakeDistr                       :: Set (LC.DRep (EraCrypto era)) -> BlockQuery (ShelleyBlock proto era) (Map (LC.DRep (EraCrypto era)) Coin)
        GetCommitteeState                       :: BlockQuery (ShelleyBlock proto era) (SL.CommitteeState era)

The queries marked with `*` and/or `+` are the only queries whose responses exhibit weak monotinicity as the node improves its selection by switching/extending chains.
For example, once `GetChainBlockNo` becomes `NotOrigin x`, it never afterwards return `NotOrigin y` where `y < x`.
Similarly, once `GetEraStart` for some era of interest becomes `Just x`, it will remain `Just x`.

- `++` means the monotonicity is absolute (eg it's a constant value for this blockchain).
- `**` means the monotonicity could only be spoiled if something like data corruption caused the node to roll back some blocks without simultaneously adding at least that many blocks.
- `*+` means the monotonicity could only be spoiled if the node somehow performed a switch that rolled back more than k blocks.
   Call this _k-rollback-monotonicity_.

If the hard fork combinator were to always agree with the ledger---thereby avoiding the awkward surprise arising possible after a Chain Growth violation---then it would have to count only slots as the ledger does and not blocks.
As a result, `GetEraStart` and `GetInterpreter` would no longer exhibit weak monotonicity.
Specifically, a particular Chain Growth violation could cause `GetEraStart` to yield `Just x` in one moment and then `Nothing` or `Just y` where `x /= y` at a later moment, despite the node never rolling back more than k blocks.
(`GetInterpreter` would be more intricate to describe, but the nub is exactly the same.)

It is remarkable that every query without a marker is not weakly monotonic, and no one expects them to be!
For example, there's no surprise that the UTxO changes when the node switches chains.
However, it was decided that `GetEraStart` and `GetInterpreter` needed to exhibit this monotonicity because their replies include information about _time_.
For example, if a user needed to know at what UTC time a specific slot would begin, the Cardano CLI would determine that using the `GetInterpreter` query.
And because `GetInterpreter` is weakly monotonic, the Cardano CLI will reply with "unable to translate that slot to a UTC time" until the UTC time is both predictable from the ledger state answering the query and moreover guaranteed to be the same for every ledger state the node could subsequently select unless it somehow rolls back more than k blocks.
This decision may seem even more reasonable when you consider that the opposite question is handled the same exact way: the node will not let the user translate a UTC time to a slot until that answer is also k-rollback-monotonic.
(Frisby's inference is that UTC time seems so fundamental that it's easy to wrongly assume that the translation back and forth between slots and UTC times does not depend on the evolution of the chain.)
Requiring this monotonicity for `GetEraStart` and `GetInterpreter` avoids that confusion from manifesting.

## Proposal: Best of Both Worlds

Consider the following alteration of the hard fork combinator.

- Remove the hard fork combinator's second trigger constraint, such that it transitions to the next era exactly when the ledger governance updates the major version of the protocol version protocol parameter (mutatis mutandi as of Conway).
- Only answer `GetEraStart` and `GetInterpreter` queries using an immutable ledger state.
- (The ledger rules could then return the voting deadline to 7k/f; one stability window before the epoch transition would be sufficient again.)

This would have the exact same answers to Question 1 and Question 2 as does the simplified node, and the Note-To-Client time translation queries would still exhibit k-rollback-monotonicity.

There are three main benefits for the Consensus Team.

- The hard fork combinator would no longer need the block-counting logic.
- The Consensus Team would no longer have to justify the "double-stability" for the ledger governance rules.
- Justifying the k-rollback-monotonicity of the time translation queries would be easier, since the immutable tip is more familiar than the double-stability window and hidden internal detail of the hard fork combinator.

There are two possible downsides to this change.

- It would now be the case that someone's node may forge a block in a new era at one moment and then forge a block in the previous era at a later moment.
  In a sense, scrutiny of "which eras my node has forged blocks in" or of "which eras has my node validated blocks in" etc is comparable to the `GetEraStart` query.
  But the result of that scrutiny will no longer be k-rollback-monotonic.
  On the other hand, the blocks or headers being scrutinized are are not themselves k-rollback-monotonic, by the very definition of _roll back_ and _blockchain_.
  And so there is much less potential for such non-k-rollback-monotonicity to surprise than there would be if the queries about UTC times were not k-rollback-monotonic.
- Perhaps the community would prefer for the era transition to not happen if there was a severe Chain Growth violation immediately before it.
  With today's hard fork combinator, the era transition would not happen, and with the proposed hard fork combinator the era transition would happen.
  The choice between those two is somewhat moot, since the disaster recovery plan involves off-chain cooperation that will essentially roll back far enough to fill in the missed slots with empty blocks such that Chain Growth is no longer violated.
  In either case, the chosen depth of that roll back can determine whether the originally-intended era transition does take place.
  If they simply fill in the missing slots without changing the relevant votes, then both repaired chains will end up in the new era.
  To prevent that they'd need to roll back at least to before the voting deadline (4k/f today, 7k/f in the proposal) in the epoch (which may be earlier than the Chain Growth violation started) so they could change some votes to prevent the original decision to transition eras at the end of that epoch.
  The proposed change only makes a difference before the disaster recovery or if the disaster recovery is never done (eg establishing a checkpoint shortly afterward could safely guard that sparse part of the chain instead of having to time travel and fill it in).
  For example, would it be better Public Relations if the transition _didn't_ happen when planned (as in today's node)?
  Or would it be better if the transition happened when planned _despite_ there being a conspicuous disaster immediately prior to the transition (as in the proposed node)?
