This document contains a loosely organized list of small summaries of realizations we had while working on the code base.

We do eventually want to integrate these lessons learned into more coherent and targetted documents.
But step one one is to record them at all; this file is supposed to make that easy.
Step two will be to occasionally extract cohesive insights from this bag, creating new documents or refining old ones accordingly.

## Why doesn't Ledger code ever return `PastHorizonException`?

One of the `HardForkBlock` combinator's major responsibilities is providing an `EpochInfo` to the ledger code.
This `EpochInfo` uses the `Either` monad to return `Right` only when the query can be answered with certainty.
For more information on when that is, see the [Consensus Report](https://ouroboros-consensus.cardano.intersectmbo.org/pdfs/report.pdf) 
and the recordings of Edsko's presentations at the Weekly IOG Seminar([1](https://drive.google.com/file/d/1m_jKQM_gxBm0ctLqIq9NGj5_nXPI66Su/view),[2](https://drive.google.com/file/d/1QIJ-VBlj-txB6K6E7DIEnY5TzaD89qQm/view)).

However, most of the the ledger code that interacts with the given `EpochInfo` assumes it cannot fail by using `epochInfoPure`.

```haskell
data Globals = Globals { epochInfo :: !(EpochInfo (Either Text)), ... }

epochInfoPure :: Globals -> EpochInfo Identity
epochInfoPure = hoistEpochInfo (either (throw . EpochErr) pure) . epochInfo
```

Thus, it is the responsibility of the calling code (eg the Consensus code) to check that the `HardForkBlock`-derived `EpochInfo` will not fail when its invoking the ledger rules.
One example we've been looking at recently is the invocation of the `TICKF` rule, which is the `ledgerViewForecastAt` definition below.

```haskell
data Forecast a = Forecast {
      forecastAt  :: WithOrigin SlotNo

      -- Precondition: @At s >= forecastAt@
    , forecastFor :: SlotNo -> Except OutsideForecastRange (Ticked a)
    }

class ... => LedgerSupportsProtocol blk where
  ...
  ledgerViewForecastAt ::
       HasCallStack
    => LedgerConfig blk
    -> LedgerState blk
    -> Forecast (LedgerView (BlockProtocol blk))


instance ... => LedgerSupportsProtocol (ShelleyBlock (TPraos crypto) era) where
  ...
  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for ->
    if
        | NotOrigin for == at ->
            return $ SL.currentLedgerView shelleyLedgerState
        | for < maxFor        -> return $ futureLedgerView for
        | otherwise           -> throwError OutsideForecastRange { ... }
    where
      ShelleyLedgerState {shelleyLedgerState} = ledgerState

      globals = shelleyLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      at      = ledgerTipSlot ledgerState

      futureLedgerView :: SlotNo -> SL.LedgerView (EraCrypto era)
      futureLedgerView =
        either
          (\e -> error ("futureLedgerView failed: " <> show e))
          id
          . SL.futureLedgerView globals shelleyLedgerState

      maxFor :: SlotNo   -- Exclusive upper bound
      maxFor = addSlots swindow $ succWithOrigin at
```

When we returned to this code for the first time in a while, we thought it was odd that the code both handles an `Either` in the return type of `SL.futureLedgerView` and also does its own `for < maxFor` check; can't the Ledger code instead return `Left` whenever `for >= maxFor`?

We looked at the upstream code to investigate; how easily could we arrange that?
The answer is: it could be done, but the core architecture of the State Transition System code in the core Ledger library does not currently naturally allow for that (at least in our subjective opinion of _naturally_ and specifically for the `TICKF` example).
At the moment, any rule that can _fail_ must also provide a "default" value for the rest of the rule to use (see the `Predicate` constructor of the `Clause` GADT, specifically its argument of type `a`).
For the Ledger's code implementing the `TICKF` rule to observe the `Either` layer of the `EpochInfo` would require that the rule has some way to continue when the `EpochInfo` query (used to determine if the requested forecast crosses an epoch transition) fails.

It is not obvious how to do that when `EpochInfo` returns `Left`.
While it may be possible to work something out, such as perhaps the `TICKF` rule emits a new `PredicateFailure` and simply leaves the `NewEpochState` unchanged, no idea seems natural to us.
They all involve creating an incorrect `LedgerView` and then only throwing it away at the last moment.
And notably, the rest of the rule could emit additional errors, all of which would be presumably spurious given that we already know the very first check (ie querying the `EpochInfo`) failed.

So instead, for now at least, the Consensus code must do its own checking before invoking Ledger code that relies on the `EpochInfo` never failing.

- By design, any invocation of Ledger code that causes the `EpochInfo` to return `Left` would be a _bug_, with no obvious way to recover.
  Thus this isn't a particularly burdensome requirement; not much extra code (such as `for < maxFor` above).
  And also thus: throwing these exceptions from pure code is reasonable choice.

- We asked the Ledger team, and they didn't immediately reject the possibility of enriching the base monad's feature set to include short-circuiting failure (eg adding an `ExceptT` in the Ledger rules) for use in computations such as the given `EpochInfo Either` where it would make sense to immediately abort the rule computation.

A couple more observations:

- The ways the `TICKF` rule can currently fail are all sanity checks.
  In particular, if they fail, then there's no way this chain could ever successfully cross the epoch boundary, not via `TICKF` nor via the full `TICK`.
  This justifies the use of `error` in the `where`-clause `futureLedgerView` above---there is no useful way to recover from this error; the node is doomed to never tick past the epoch boundary until an operator manually truncates its chain so it can switch to a better one (if one exists :fingers-crossed:) that isn't doomed.

- At least one part of the ledger _does_ use the `EpochInfo Either` as a test: the validation of the `ValidityInterval` of a transaction that contains a Plutus script.
  The code here accommodates the inflexibility of the `Predicate` rule by using an additional `whenFailureFree` combinator to skip over invalid tx, thus avoiding the computation that would require the result of the `EpochInfo` that instead returns `Left`.
  (TODO ... Frisby wonders if every use of the `epochInfo` could do that same thing).

So, either by mimicking the approach of the existing `ValidityInterval` validation logic or by altering the STS innards to allow short-circuiting failure, we could reify the `EpochInfo` failures into the `PredicateFailure` hierarchy, and thereby leverage the types to force each invocation of the Ledger API to independently handle the possibility of `PastHorizonException`s.
But it's not obvious that that is definitely worth the extra complexity it would introduce on the Ledger code.

## Why use the Honest Chain Growth window as the Ledger's Stability Window?

Suppose we have selected a different chain than our peer and that our selected chain has L blocks after the intersection and their selected chain has R blocks after the intersection.

REQ1: If k<L, we must promptly disconnect from the peer (b/c of Common Prefix violation and Limit on Rollback).

REQ2: If L≤k (see REQ1) and L<R, we must validate at least L+1 of their headers, because Praos requires us to fetch and select the longer chain, and validating those headers is the first step towards selecting those blocks.
(This requirement ignores tiebreakers here because the security argument must hold even if the adversary wins a tiebreaker.)

The most demanding case of REQ2 is L=k: at most we'll need to validate k+1 of the peer's headers.
Thus using HCG window as Stability Window ensures that forecasting can't disrupt REQ2 when the peer is serving honest blocks.
(We can tick the intersection ledger state to their first header, and then forecast 3k/f from there which by HCG will get us at least the remaining k headers if they're serving an honest chain.)

## How does cross-era forecasting work?

When we talk about forecasting, we mean about the process of trying to get a ledger view from a ledger state for a given slot. This ledger view can then be used to verify the validity of headers in that slot that live on the same chain as the original ledger state.

Hence, in the context of the HFC which has to support forecasts across era boundaries, forecasting can be thought of to have type
```haskell
   SlotNo
-> LedgerState blk
-> Either OutsideForecastRange (LedgerView (BlockProtocol blk'))
```
(in reality, there is an intermediate `Forecast` type, see `ledgerViewForecastAt`).

The HFC implements forecasting like this (for the actual implementation, see `Ouroboros.Consensus.HardFork.Combinator.Ledger`, in particular `oneForecast`):

 - Due to stability requirements of the safe zone, the HFC can always determine whether the desired slot is still in the current or already in the next era when it is within one safe zone of the current ledger state.
 - If the HFC can not yet know the era of the slot, it fails with `OutsideForecastRange`.
 - If the slot is in the current era, the HFC delegates forecasting to the underlying block type.
 - If the slot is in the next era, the HFC calls a user-provided function to forecast:

   ```haskell
   newtype CrossEraForecaster state view x y = CrossEraForecaster {
         crossEraForecastWith ::
              Bound    -- 'Bound' of the transition (start of the new era)
           -> SlotNo   -- 'SlotNo' we're constructing a forecast for
           -> state x
           -> Except OutsideForecastRange (Ticked (view y))
       }
   ```
   (which is available via a field of `hardForkEraTranslation` in `CanHardFork`).

Hence, the HFC fully offloads the task to work out a safe way to do cross-era forecasting to the user, in particular, the task of determining how far ahead one should be able to forecast.

In our case, there are two cases of era transitions:

 - **Intra-Shelley:** These are trivial to support, as there are almost no changes regarding forecasting, so we can simply forecast starting in the old era and then convert the resulting `LedgerView` to the new era.
    - The `LedgerView` actually only depends on the `ConsensusProtocol`, which only changed from Alonzo/TPraos to Babbage/Praos (Vasil HF), and even there, the translation only consists of un- and rewrapping (see `translateLedgerView`).
    - The stability window/forecasting range also stayed the same so far, but there already is existing logic to handle changes there, see the usage of the very conservative `crossEraForecastBound` in `forecastAcrossShelley`. (We definitely will want to revisit that in case we actually ever do a change here.)

 - **Byron-to-Shelley:** This is implemented in `crossEraForecastByronToShelleyWrapper`, and exploits the fact that the ledger view for the first Shelley epoch is independent of the Byron ledger state, and can be constructed just using the static Shelley ledger config.

   Additionally, it allows you to forecast up to one full stability window into Shelley (which is much larger than the stability window of Byron, from the ledger state for `2k` slots vs from the ledger state to the epoch transition and then an additional `3k/f` from there).

Future work could include making the HFC itself handle more details of cross-era forecasting, in particular around determining safe forecast range bounds in case of a changing stability window.

## Which slots/times/epochs can we translate?

In early May 2023, we spent the Consensus Office Hours discussing a node test of the `slot-number` CLI command, which translates a slot to a UTC time.
In particular, the tester's question was essentialy "When is the slot-number command expected to fail?".
The answer is ultimately not too complicated, and generalizes to other translations between slots, epochs, and times.
But this is the nth time we've answered it (sometimes we asked ourselves), and each time required a substantial effort to re-learn it so that we could answer confidently.
This topic definitely deserves better docs/dedicated tests/etc---but we're (as always) strapped for time, so this section is stop-gap measure (that's the point of this whole file as a staging ground).
This explanation summarizes abstractly and then points to key parts of the implementation.

It's worth noting that this capability is not used only for queries.
In particular, these translations are how the node determines it leads the current slot.

### The HFC "Shape" and the HFC "Summary"

The Hard Fork Combinator lets us specify eras separately and compose them into a single type of chain (ie a "block type") that can fork through some prefix of those eras.
Any piece of code supports some sequence of eras.
For example, the original Cardano code only supported Byron.
At some point we added Shelley to the code.
And at some later point we added Allegra.
And so on.
It's notable that the Cardano chain hard forked into the latest era before the addition of each subsequent era.
To be pedantically concrete: the code supported only Byron and the chain was in Byron, then the code also supported Shelley, then the chain forked into Shelley (hurray!), then the code also supported Allegra, then the code forked into Allegra, and so on.
In general, it's possible the code could know about an arbitrary number of future eras before forking into them.
Or it could know about a future era and we could change our minds about it, deleting that era without ever forking into it.
But, so far, on Cardano the code has always been either zero or one eras ahead of the whole network's chain (ie "the Cardano chain").
Even so, a new node syncing today will at some point have only synced enough of the chain to be in, say, Mary, even though the code knows about multiple eras after Mary.
So, despite Cardano doing the simplest possible thing with respect to the whole network's chain, new (or even just very stale) nodes will exercise the more general possibilities (ie the code knowing about multiple eras beyond the tip of the node's current selection).

Thus, at any given time the code supports some non-empty sequence of eras.
And, at that same time, all valid Cardano chains and their prefixes (aka all valid Cardano blocks) will inhabit some prefix of that sequence---in the absence of catastrophic release and/or operator failure.

A Hard Fork Combinator ledger state provides enough information to determine which era it is in.
It must also determine (ie record) when that era and every preceding era started.
Trivially, this also determines when all the _preceding_ eras ended (ie when the successor started).
Thus, every ledger state determines the start time of `N` eras and the end time of the preceding `N-1` eras (note: there's one more start time than end times) for some `N`.
Every `N` will be at least 1, at most the length of the sequence of eras the code supports, and subsequent ledger states on a chain cannot have a lesser `N`.
For the real Cardano chain, it's possible the ledger state itself inhabits either either the `N-1`th or the `N`th era, since it it _might_ also determine when its own era will end.
However, beyond the concrete case of Cardano, it's possible that a ledger state could already know when eras after its own will end.

### Aside: Warning

(Skip this section on your first read.)

That invariant about how many era starts/ends a ledger state determines was coyly worded to exclude one perhaps-surprising exception: the code allows for an era to be specified to never end.
The above invariants remain true, but can be tightened in this case.
Specifically, when the ledger state reaches the era that is specified to never end, it is thereafter always definitely in the `N`th era (not the `N-1`th), and there will never be a greater `N`.
(The code may support eras after an unending one, but that would be an awkard/wasteful choice.)
No Cardano era is specified to never end, and we currently do not foresee every doing so.
The possibility of an never-ending era is only supported for tests and for reuse by other blockchain implementors that are not concerned with extensibility.

### The Wrinkle with Time

For interpreting slot/time/epoch translation queries, the key motivating fact is that each era can have a different size of epoch (in slots) and/or a different length of slot (a time duration).
Thus, we cannot handle a translation query unless we know which era its argument---be it a slot, an epoch, or a UTC time---is in and the durations of all preceding eras, or, equivalently, the start times of all eras up-to-and-including the one the argument is in.

Each query is handled by some piece of code and with respect to some ledger state.
The code (including config files) determines the supported eras (including their epoch size and slot length), and the ledger state determines which prefix of eras has known start times.
The known start times are enough to recognize when the given argument is during some era with a known end time, in which case the translation succeeds.
(The translation starts from the chain's start time, adds the duration of all preceding eras, and then adds the relative time since the beginning of the argument's era.)
Otherwise, we need more information to know whether the given argument is during the last era with a known start time, ie the first era with an unknown end time.
When the query argument is after that start time, the fact that the era's end is not yet known means that the query argument is either in that era or instead in a future era with a possibly different epoch size/slot length, and so the correct translation may differ from the hypothetical response derived from the assumption the query argument precedes the first unknown end time.

For a ledger state in the first era with an unknown end time (ie the last era with a known start time), the code relies an a lower bound for that end time, ie the soonest that era could possibly end.
If a query argument handled by that ledger state precedes that bound, then the time-slot translation requested by that query succeeds, otherwise it fails.
For this purpose, the code (including config files) cannot support an era unless it also knows a "safe zone" for that era, where the safe zone (along with any additional assumptions the Consensus code makes) is enough information to determine the bound induced by a given ledger state.

In general, it would be possible to design a system such that even the final ledger state in an era did not determine when that era would end.
That'd be an empty safe zone, consisting of zero slots.
For example, perhaps the first block of an era identifies itself as such simply by setting some field in its header.
For that kind of chain, some ledger states would not provide enough information to do any translations whatsoever of slots/times/epochs _after_ that ledger state's tip.
(Note that if the first block of an era declares itself as such, then the node that minted that block has some how already decideded to transition to the next era.
Hence the correspondingly hypothetical leadership check would have known whether it's about to forge a block in the current era or the next, and so it would still be able to determine the slot of the wallclock _according to the era of the block it's planning to forge_.)

For the case of Cardano, however, other concerns (not directly related to slot/epoch/time translations) already require a degree of "stability" from the ledger rules to be above some minimum.
This "stability" is a stronger requirement than the safe zone, so, for the sake of simplicity, we take that same minimum (aka one _stability window_) to be the safe zone.
Thus the safe zone is `2k` slots for Byron and `3k/f` slots for every Shelley era.

Recap:

- Consider some Cardano ledger state.
  There is a first era that has an unknown end time according to that ledger state, which is also the last era with a known start time.
- Even though the end time is unknown, it cannot be sooner than one safe zone after the known start time.
- Moreover, if the ledger state also inhabits this era, the era cannot end sooner than one safe zone after the slot this ledger state was most recently ticked to.
- Overall, it can be defined as: the _safe zone_ of an era is a lower bound on the number of slots that exist before the end of that era and after the latest-possible ledger state that does not determine the end of that era.

### Aside: The Necessary Stability of Header Validity

[Esgen suggests that this could eventually be migrated to section 24.5 ("Eliminating safe zones") of the Consensus and Storage Layer Report (or the succeeding section).]

(Skip this section on your first several reads.)

- Headers declare what era they are in.

- The stake distribution of the next epoch is determined at least one stability window before it starts, regardless of whether that next epoch is in the next era.

In conjunction, a header that satisifes its declared era's rules can be validated as such by some ledger state even if there is an era transition between the two.
Today's code handles this by ensuring that any such era transition is necessarily already known by the ledger state, but that's not strictly necessary.
Relaxing that constraint (eg admitting an empty safe zone) introduces more complexity, which Cardano has so far eschewed.
For example, there is no guarantee that the ledger state the header actually extends (ie the one resulting from ticking the ledger state as of the application of the previous block into this header's slot) is indeed in the era the header claims to be in; so it could satisfy the rules of its claimed era but still actually be an invalid header (when that claim is a lie).
Moreover, era transitions are not the only updates that can affect the validity of headers.
One excellent example is the the Transitional Praos (aka "TPraos") decentralization parameter `d` (any protocol parameter change requires a version change, but it could be minor---only major version changes can possibly cause proper era transitions).
Another example is that no part of the design beyond (probably wise) convention is preventing a so-called "intra-era" fork from influencing the header validity.

Continuing this hypothetical: an adversary could lie about what era their header inhabits in order to send an apparently valid header that is actually revealed as invalid once the intervening blocks are available---obviously that's undesirable, but is it truly problematic?
We generally consider that the Header-Body Split requires a ledger state can correctly determine the validity of any header within one stability window of the (last applied block of the) ledger state.
But that's just an ideal case; we could relax that, if we had a reason to embrace the additional complexity it involves.
The actual fundamental requirement derived from Praos is two-fold.

- No false alarms for honest headers: an honest header (which is by definition actually valid) must never appear as invalid.

- The only possible harm of missed alarms (an adversary's invalid block appearing to have a valid header) are DoS-vectors preventing the node from concluding the honest header's are attractive enough to fetch the honest blocks as promptly as Δ requires.

- (Compare that pair to the "Why use the Honest Chain Growth window as the Ledger's Stability Window?" question in `HardWonWisdom.md`.)

Roughly: the fundamental rule is to not (at all) underestimate the honest chain growth and to not excessively over-estimate the adversarial chain growth.

Consider some examples.

- Even in this laxer rule, the real `d` parameter cannot change within the stability window, because if it does change, then honest headers that are in an overlay slot according to the reduced `d` might not be in an overlay slot according to the unreduced `d`, and that would lead to false negatives for honest (overlay) headers.

- It would be possible to have defined the overlay schedule such that the overlaidness of a slot was monotonic in `d` (thereby preventing the above counterexample).
  We didn't do it that way; even if we had, then there still would have been false alarms for honest headers, because some honest headers in slots that were actually not overlaid would be rejected since the incorrectly greater `d` would interpret those slots as overlaid.
  So the `d` parameter is something that even the laxer rule could not accomodate (even when assuming it can't increase, because the its overall influence on honest headers is not monotonic).

- As of epoch 257, `d=0` (ie no slots are overlaid).
  Since then, no change to the Cardano protocol parameters (including era transitions) has changed the (average) number of slots any given issuer leads during any given interval of slots.
  Also since then, every change that has at all affected the validity of headers has been an era transition.
  Therefore, _none of those subsequent changes truly required a stability window of warning_.
  Honest headers after such change would correctly identify themselves as such by claiming the new era, and therefore would be correctly validated.
  An adversarial header, in this hypothetical, could have falsely claimed to be from the new era; it would not be identified as actually invalid until its block arrived.
  But the crucial insight is that such adversarial headers are still limited by the adversary's election rate, and so probably could not prevent the node from fetching the honest blocks---unless the next era has _abruptly_ and _drastically_ the number of slot each stake pool leads.

Recall that that discussion was merely theoretical, to better delineate the fundamental requirements.
The real Cardano ledger always has and still does ensure that header validity is fully determined at least one stability window in advance.
We have had no motivating reason to eliminate that simplification, as of yet; so far we don't anticipate one arising soon.
(Currently, the protocol parameter changes are even known _two_ stability windows early!
But some factors are still just singly stable, eg the stake distribution is only frozen one stability window before the next epoch.)

(TODO What about the config file overrides that result in empty epochs?.
I'm actually unsure how that's working now... does it only work if a whole prefix of the static era sequence is skipped?
EG you can't skip only the 3rd era?)

### Aside: Warning

(Skip this section on your first read.)

That reasoning implies some non-empty suffix of the ledger states in an era will each determine the exact end time of that era.
It's technically possible a ledger state could also determine the end time of subseqeuent eras, but the Cardano ledger rules do not permit that.
On `mainnet` Cardano, only blocks in an era can cause that era to end (technically false, but it's the right sentiment).
This also implies each era with a known end time on a `mainnet` chain must contain some blocks on that chain.
On the other hand, the Consensus Layer configuration inputs do permit overriding the era transition rules for the sake of testing, in which case a ledger state (including the initial ledger state at genesis!) could anticipate the end time of several subsequent eras and some eras could be empty (eg main use case is skipping from genesis to some later Shelley era ASAP, in which case all the intervening eras have no blocks but also have no slots!).

### Aside: Warning

(Skip this section on your first several reads.)

There is one last notable caveat at this abstract level.
The Byron and Shelley ledgers already had some logic for the evolution of proposals that can change the protocol (eg incur an era transition).
When designing and implementing the Hard Fork Combinator for use at the Byron-to-Shelley transition, Edsko de Vries reasoned through that the state machines of that logic needed to be adjusted to ensure "double stability".
The best resource we have for motivating this constraint is [Edsko de Vries' IOG seminar from 2020 June on the HFC and time](https://drive.google.com/file/d/1QIJ-VBlj-txB6K6E7DIEnY5TzaD89qQm/view).

- At the 16m04s mark he motivates that the forewarning must suffice for at least k+1 headers.

- At the 22m42s mark he starts explaining the Byron proposal state machine, and this ends up motivating "stably stable", aka "doubly stable", aka "double stability", aka "stable stability".
  (This recording was made before the Byron-to-Shelley fork happened.)

- From 30m46s to 38m52s, he explains the justification for cross-chain translations (and off-handedly that it's technically optional).

For example, [Shelley the `PPUP` ledger rule](https://github.com/IntersectMBO/cardano-ledger/blob/180271602640bcac1214084b6de61d0468332f00/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Ppup.hs#L192) requires update proposals to be settled at least two stability windows before the end of the epoch (ie `6k/f`, not just `3k/f`).
(That links to the tip of the `master` branch at the time of writing this, although this constraint is not new.)

(TODO Frisby wonders whether, instead of double-stability in the ledger rules, we could instead only draw conclusions that should not be subject to roll back (such as time translations) from the youngest immutable ledger state.)

### Answer to the Question

The ultimate answer to the Office Hours question is that the query will fail if and only if its argument is beyond the HFC's current lower bound for the next unknown era end (whether that's the current era or the next).
On Cardano, that lower bound is the end of the ledger state's current epoch UNLESS

- the ledger state is within 3k/f slots of the end of its epoch
- and/or the ledger state has already determined that its era ends at the end of its epoch (ie the necssary votes are in by the `4k/f`th slot, and there are `k` subsequent blocks after that slot, which tends to happen shortly after the `5k/f`th slot)

in which case the lower bound is instead the subsequent epoch boundary.

### Implementation Pointers

The key corresponding parts of the implementation are as follows.

- The `ouroboros-consensus:Ouroboros.Consensus.HardFork.History.Qry` module defines `Interpreter`, `Qry`, and `interpretQuery`.
  Both `Interpeter` and `Qry` are opaque types, so in reality only whatever pre-defined queries that module exports, such as `slotToWallclock`, are available.

- These are ultimately used for every slot/epoch/time translation: `EpochInfo` passed to the ledger, the leadership check in a block-producing node, and all Consesus queries (see `ouroboros-consensus:Ouroboros.Consensus.HardFork.Combinator.Ledger.Query.GetInterpreter`).

- Every downstream use, such as the `slot-number` CLI command ultimately boils down to issuing the `GetInterpreter` query to the node itself and then invoking `interpretQuery`.

- Crucially, that interpretation can fail when the given interpreter cannot answer the given query, which is precisely the topic of the Office Hours question.

- Remark.
  A significantly fresher `Interpreter` will be able to answer more queries than a stale `Interpreter`.
  The `ouroboros-consensus:Ouroboros.Consensus.HardFork.History.Caching` module captures that idea.

- The `ouroboros-consensus:Ouroboros.Consensus.HardFork.History.EraParams.EraParams.EraParams` captures the data underlying the code's (including the configration files') support for an era.
  The (start/end) transition points are represented in triplicate: as a slot, as an epoch, and as a relative time (where `0` is `cardano-base:Cardano.Slotting.Time.SystemStart`, which is a UTC time).

- The interpretation only depends on the eras' start and end bounds, not on the safe zone.
  The safe zone is instead used in an preceding step to safely determine the least possible value of the first unknown end bound.
  This determination is implemented by the `ouroboros-consensus:Ouroboros.Consensus.HardFork.Combinator.State.reconstructSummaryLedger` function.

## The hard fork combinator interface

To run the Consensus Layer on a block type `blk` requires the following (whether or not it's a `HardForkBlock`).

- Values for various tracers, ChainDB options, etc, as required by `Ouroboros.Consensus.Node.run`.
- A `ProtocolInfo blk` value, which is `TopLevelConfig blk` as well as initial state of the ledger and protocol.
- Instances of `ConsensusProtocol (BlockProtocol blk)`, `LedgerSupportsProtocol blk`, and so on (see the `RunNode blk` class).

Suppose there are two blocks types `F` and `G` for which the above is already available (everything will generalize to cases with more than two eras via simple replication).
To run the Consensus Layer instead on the hard fork combinator that sequences `F` into `G` requires the following additional information about `F` and `G`.

- A value of `EraParams` for `F` and for `G` (ie `Shape [F, G]`), which decomposes as the following for `F` and `G` individually.
    - The duration of every slot in the era as a [`NominalDiffTime`](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Clock.html#t:NominalDiffTime).
    - The duration of every epoch in the era as a number of slots.
    - The era's _safe zone_ is intimately related to the `singleEraTransition` function introduced below, as explained in the next subsection.
      The safe zone cannot be 0 because---as discussed elsewhere---that would imply a stability window of 0, which is impossible.
- An instance of `CanHardFork [F, G]`, which decomposes as follows.
    - A function `LedgerConfig F -> LedgerConfig G -> LedgerState F -> Bound {- starting slot&epoch&time-since-genesis of G -} -> SlotNo -> Except OutsideForecastRange (LedgerView G)`.
      This function will be used for forecasting the ledger view across the era transition; see "How does cross-era forecasting work?" above.
    - A function `ConsensusConfig F -> ConsensusConfig G -> EpochNo {- start of G -} -> ChainDepState F -> ChainDepState G`.
      This function will be applied to the chain-dependent state immediately after the last `F` header.
      Then the protocol rules for `G` will be used to tick that state to the slot of the first `G` header.
      This use of `G`'s ticking rules instead of `F`'s to cross the end of the last epoch of `F` is subtle and somewhat counter-intuitive.
      (We haven't yet had any concrete issues here, unlike with the `LedgerState` issue linked just below.
      However, we did only realize recently that the extra entropy parameter of Transitional Praos should have been considered when translating to Praos at the Alonzo->Babbage transition.)
    - A function `LedgerConfig F -> LedgerConfig G -> EpochNo {- start of G -} -> LedgerState F -> LedgerState G`.
      This function will be applied to the ledger state immediately after the last `F` block.
      Then the ledger rules for `G` will be used to tick that state to the slot of the first `G` block.
      This use of `G`'s ticking rules instead of `F`'s to cross the end of the last epoch of `F` is subtle and somewhat counter-intuitive (eg see Issue <https://github.com/IntersectMBO/cardano-ledger/issues/3491>).
    - Instances of `SingleEraBlock` for `F` and `G`, which decomposes as the following for `F` and `G` individually.
        - Types that represent the ledger and protocol configuration _without_ any `EpochInfo` (see `HasPartialLedgerConfig` and `HasPartialConsensusConfig`).
        - A function `singleEraTransition :: PartialLedgerConfig blk -> EraParams -> Bound {- era's start -} -> LedgerState blk -> Maybe EpochNo` which must return `Just e` if any extension of this ledger state will end between epoch `e` and its predecessor (ie epoch `e-1` when `0<e` and "genesis" otherwise).
            - As an immediate observation: note that this function is monotonic along the ledger states of an era on a single chain.
              It cannot return `Just` for some ledger state on a chain and then `Nothing` for a later ledger state in the same era on the same chain.
            - Similarly: it must not return an epoch `e` which is less than the epoch in which the era started.
              (See the third subsection for the corner case in which it may return the starting epoch.)
            - The logic used in this function must be invariant with respect to ticking.
              In other words, if the end of the era is unknown or uncertain after the application of a block, then it cannot become certainly known before the next block.
                - This constraint justifies that the function's type signature does not support applying it to a ticked ledger state; if it hypothetically did, then its output would exactly agree with the pre-image of that ticked ledger state.
                  (Implementation detail: a ticked HFC ledger state must include `TransitionInfo` derived from the non-ticked ledger state only because the signature of `singleEraTransition` would not otherwise allow a ticked HFC ledger state's contents to determine the `TransitionInfo`.)
                - Notably, this constraint is trivially true if the function is actually counting blocks in order to achieve its monotonicity with respect to maximum rollbacks without relying on a Chain Growth property.
            - Recall that an era's `singleEraTransition` is intimately related to its safe zone; see the next subsection.

Some notable components are not listed above because they are determined by existing data about `F` and `G`.

- The content of `TopLevelConfig (HardForkBlock [F, G])` other than the `EraParams` values can be completely determined from the `TopLevelConfig F` and `TopLevelConfig G`.
- The initial state of the ledger and protocol can be completely determined from the same for `F`.

### The Safe Zone Semantics

An era's safe zone constrains how its `singleEraTransition` can vary as the chain is extended in that era.
The precisely-worded high-level rule for an era whose safe zone is the positive integer `d` is as follows.

> If a chain's configuration data and blocks so far together determine when the era starts but not when it ends, then at least `d` slots after the latest block (non-EBB) will be in that era.
> (If the latest block is from an earlier era, then those `d` slots begin with the first of the era.
> Otherwise, the latest block is in the era and the `d` slots begin immediately after it.)

Consider an arbitrary `[F, G]` chain with 200 `F` blocks followed by 30 `G` blocks (these counts are only real blocks, excluding any EBBs).
Each block on that chain results in a ledger state in the same era as the block.
In addition, however, there is also a `LedgerState` at the beginning of each era that is not the result of a block from that era.
For `F`, it's the initial ledger state (resulting from the genesis "block") and for `G` it's the translation of the last `F` ledger state to `G`.
Thus, this chain will induce a sequence of 201 `F` ledger states and a sequence of 31 `G` ledger states.
The `F` safe zone constrains the output of `singleEraTransition @F` when it is applied to that each `LedgerState F` in that sequence, and similarly for `G`.
(Note that those sequences _exclude_ ledger states that arise from any EBBs interspersed among the real blocks.)

Specifically, for the sequence of ledger states within an era, if the era has a safe zone of `d`, then its `singleEraTransition` function cannot return `Nothing` for a ledger state in that era and `Just e` for the next ledger state in that era unless the epoch `e` is still far enough in the future.
Note that the second ledger state in that specification necessarily arises from a block in the era, since only the first ledger state in an era does not arise from a block in that era.
Let `y` be that block; the exact safe zone constraint is `slot(y) + d < firstSlot(e)`.
(Lastly, recall once more that the ledger state sequence in question is intentionally excluding any arising from EBBs.)

That interpretation of the safe zone constraint explicitly only considers the evolution of `singleEraTransition` _within a single era_.
In particular, it did not at all constrain the output of `singleEraTransition` for the initial ledger state of the era.
The rest of this subsection discusses that, including notably that that initial output is always `Nothing` in the fundamental use case, in which the end of an era cannot be known before any of its block exist.

In the fundamental motivating use case of the HFC, `singleEraTransition` will return `Nothing` for the first ledger state of each era, and subsequent governance events within that era on the chain will cause it to return `Just` for the remaining suffix of that chain's ledger states in this era.
However, there are a total of three possible trajectories of the `singleEraTransition` within an era on some chain.

- EXTENSIBLE trajectory.
  It starts with `Nothing` and ends with `Just`, as in the fundamental motivating use case.
  Note that once it becomes `Just` it will always remain `Just` (with the same epoch number `e`) for the rest of this chain, by the monotonicity of `singleEraTransition` within an era of some chain.
  This is the only case in which the above single-era interpretation of the safe zone applies, because it's the only case that involves both `Nothing` and `Just` in the same era.
- NEVER trajectory.
  It starts a `Nothing` and will necessarily remain `Nothing` on the entire chain.
  For example, `singleEraTransition _cfg _params _start _st = Nothing` is a compatible definition.
  In this case, any safe zone value would satisfy the high-level rule, because the era never ends---the implementation names this an _indefinite_ safe zone.
- IMMEDIATE trajectory.
  It starts as `Just e`.
  Note that it will always remain `Just e` for the rest of this chain, by the monotonicity of `singleEraTransition` within an era of some chain.
  The IMMEDIATE trajectory can arise in two ways.
    - Case IMMEDIATE-INDEPENDENT.
      It's possible that the configuration data determines the end of the era, entirely independent of the preceding chain.
      For example, `singleEraTransition cfg _params _start _st = Just $ foo cfg + 42` is a valid definition.
      In this case, any safe zone value would satisfy the high-level rule, because the end of the era is determined even before the start is.
      (TODO the HFC is "technically incorrect" because it assumes the safe zone applies even in this IMMEDIATE-INDEPENDENT trajectory, but only in the corner case when the era does not contain even one safe zone of sots, eg it contains no slots, as in the next subsection.)
    - Case IMMEDIATE-DEPENDENT.
      It's possible that the preceding chain before the era determines its end.
      No known chain does this.
      It is technically allowed by today's HFC, but only by accident.
      There are two notable subcases; see the next subsection.
        - Subcase IMMEDIATE-DEPENDENT-BOUND.
          The determination instead depends only when the previous era ended.
          This is less obviously exotic.
          For example, the era could specify that it lasts exactly 10 epochs, regardless of when it starts.
          In this case, any safe zone value would satisfy the high-level rule, because the end of the era is determined exactly when the start is.
          (TODO the HFC is "technically incorrect" because it assumes the safe zone applies even in this IMMEDIATE-DEPENDENT-BOUND trajectory, but only in the corner case when the era does not contain even one safe zone of sots, eg it contains no slots, as in the next subsection.)
        - Subcase IMMEDIATE-DEPENDENT-TRANSLATION.
          If the determination depends on the result of translating some parts of the last ledger state in the previous era, then the blocks of one era are directly determining the end of a later era.
          This seems incompatible with the fundamental motivation of the HFC.
          This case has an intricate relationship with the safe zone.
          Either the output epoch is at least a safe zone after the start of the era, or else this era starts in epoch 0 and all previous eras both start and end in epoch 0, since that guarantees that the initial ledger state is immediately available, so that the era's end will be determined when the start is, thereby trivially satisfying the high-level safe zone rule.
          Note that this prefix of eras therefore contains no slots, which is the topic of the next subsection.

Every era of Cardano mainnet today uses the EXTENSIBLE trajectory.
However, other Cardano chains used for testing and/or benchmarking instead use the IMMEDIATE era for some prefix of eras in order to "skip" them, starting the chain in a later era (see the next subsection).
Lastly, due to bugs, some mainnet eras were previously using the NEVER trajectory by accident (eg see PR <https://github.com/IntersectMBO/ouroboros-network/pull/3754>).

The intended use of each trajectory is as follows.

- Use EXTENSIBLE for eras that are expected to end on the chain, even if they could not actually end today (eg even if their current `singleEraTransition` implementation will never return `Just`).
  For example, this could be `F` and `G` in `[F, G]`, if the plan is for this chain to later transition out of `G` into an `H` era that doesn't exist _yet_.
  In this way the HFC combinator enables today's chain to eventually transition seamlessly into eras that do not yet exist.
  The core idea is that an `[F, G]` chain should still specify a finite safe zone for `G` despite it currently being the last era in the sequence, since the blocks created by nodes running `[F, G]` will eventually be validated by future nodes running `[F, G, H ...]`.
- Use IMMEDIATE only for eras that ended in the past or could end on the chain today (ie next era's implementation is already deployed).
  For example this could be `F` in `[F, G]` but not `G`, since there's not yet a next era in the implementation.
- Use NEVER only for what is known to be the final era the chain will ever have.
  For example this could be `G` in `[F, G]` but not `F`, but only if the plan is for this chain to never transition out of `G`.
  It's always safe to use the EXTENSIBLE trajectory instead of the NEVER trajectory, but the NEVER trajectory will allow the HFC to predict eg slot-and-time translations into the indefinite future.

The HFC interface currently requires the user to specify the `EraParams`'s safe zone and the `singleEraTransition` function separately.
As such, it is possible to accidentally specify an indefinite safe zone alongside a `singleEraTransition` that sometimes returns `Just`.
It's also possible to accidentally use the NEVER trajectory for what is not actually the final era.
These violations do not necessarily lead to irrevocable disaster, but they are likely to cause confusion and/or require eg some patches to the the ledger rules of preceding eras when releasing a new one (eg see PR <https://github.com/IntersectMBO/cardano-ledger/pull/2785>).

### Eras that contain no slots

The HFC interface and contract together permit `singleEraTransition cfg eraParams start st` to return `Just (boundEpoch start)`.
In that case, the era contains no slots on the chain.
Since it therefore contains no blocks, `st` must be the era's initial ledger state, and so this equality rules out the EXTENSIBLE trajectory.
It also cannot be the NEVER trajectory, since `singleEraTransition` is returning a `Just` at all.
Therefore this equality is only possible for an era using the IMMEDIATE trajectory.

As explained above, the constraint induced by any safe zone value is trivially satisfied for the IMMEDIATE-INDEPENDENT and IMMEDIATE-DEPENDENT-BOUND cases.
In the IMMEDIATE-DEPENDENT-TRANSLATION case, however, `singleEraTransition` cannot be applied without the initial ledger state of the era, and so the end can't be determined before that initial ledger state is.
Since in general some chains in the preceding non-empty era will determine the start of this era before they determine its initial ledger state, the IMMEDIATE-DEPENDENT-TRANSLATION case only necessarily satisfies the safe zone when some prefix of the era sequence contains no slots.
That's when the era's initial ledger state is necessarily available as soon as its start is determined (since it's just a sequence of translations starting from the chain's genesis "block").
Therefore that's the only circumstance in which the IMMEDIATE-DEPENDENT-TRANSLATION case is allowed to return `Just (boundEpoch start)`.

In summary, `singleEraTransition cfg eraParams start st` can only return `Just (boundEpoch start)` either if `singleEraTransition cfg eraParams start undefined` would also (ie in the IMMEDIATE-DEPENDENT-BOUND case) or for a prefix of the eras, since that ensures each era's initial ledger state is also determined (ie in the IMMEDIATE-DEPENDENT-TRANSLATION case).

It's unclear if it's even desirable for the HFC to allow eras that contain no slots, with one exception: the Cardano system-level benchmarks already rely on this behavior in order to skip a prefix of the chain's eras.
Indeed, skipping a prefix of eras seems like the most natural use case, since the "genesis block" itself is already not an actual block.
Moreover, this is the exact case in which the era's initial ledger state is definitely also determined when its start is determined.

But there is likely a more direct way to support this "skipping" use case.
Specifically, it seems possible and desirable to remove the IMMEDIATE trajectory from the HFC interface, so that the HFC only ever applies `singleEraTransition` when there is a block in the era.
Something along the lines of "if an era's end is determined before its first block exists, then the era must end before slot 0".
Or more directly: require that `singleEraTransition` returns `Nothing` for the initial ledger state of every era and also provided a separate and isolated means of constructing an initial hard fork `LedgerState` that is in a later era (instead of only the first era).

TODO The current code, Edsko's chapter in the report, and the "the precisely-worded high-level rule" for the safe zone semantics sometimes applies the safe zone from the first slot of the era.
Perhaps this suggests that the translated-but-not-actually-ticked state is indeed kind of summary of everything that came before, a la the "genesis block"?
In which case it does in some sense require ticking at least parts of the last ledger state of the previous era across the epoch boundary (as in the minimal Babbage->Conway bugfix PR <https://github.com/IntersectMBO/ouroboros-consensus/pull/366>).

## Ledger predictions: time translation, forecasting, stability window, safe zone, double stability

We often mix these concepts up, so this is an attempt to relate them all to each other in one place.

- A _stability window_ is defined as the number of slots in which the Chain Growth property of Ouroboros Praos implies the best chain in the network will grow by at least `k` blocks.
- By design, the Cardano ledger also ensures that the leader schedule is known at least one stability window in advance.
  See "Why use the Honest Chain Growth window as the Ledger's Stability Window?".
- Within a single era, we use one stability window as the upper bound on how far the forecast logic is willing to predict.
  Indeed, one stability window is the greatest value such that there are no arguments for which the forecast logic would be unable to predict that far.
  But, for some arguments, the forecast logic could correctly predict up to one epoch farther than that, if we allowed that.
  But we don't; the `ledgerViewForecastAt` will refuse to forecast beyond one stability window, even when it could.
  (For example, if that logic always forecasted as far as it could, then the number of headers held in-memory by ChainSync while syncing would oscillate significantly.)
- When forecasting across eras with different stability windows, we need a different limit; see "How does cross-era forecasting work?".
- For Cardano, the time translation's safe zone is also set to the value of one stability window.
  The value is equivalent, since they both are predicting things that change at the epoch boundary and only do so when the change is known at least one stability window in advance.
- However, there is one more subtlety.
    - Forecasting predicts essentially the next epoch's leader schedule.
      The ledger rules do determine that schedule after the last block that is at least `3k/f` slots before the epoch transition.
    - Similarly, time translation is predicting the next epoch's era according to the hard fork combinator.
      For the Cardano eras, the hard fork combinator will transition to the next era at an epoch transition if two conditions are met.
      First, the ledger rules must increase the major protocol version protocol parameter at that epoch transition.
      Second, there must exist at least `k` blocks before the epoch transition and after the ledger state in which that protocol parameter update became certain.
    - For this reason, the ledger's governance rules require that all votes for changing protocol parameters must be cast at least `6k/f` slots before the epoch transition.
      Thus, Chain Growth ensures that `k` blocks will extend the latest-possible ledger state that determines whether the protocol major version will change, and the `k`th such block will still be at least `3k/f` slots before the epoch transition.
      And so the safe zone of `3k/f` is respected, since it's ultimately the existence of that `k`th block that determines whether the hard fork combinator will actually transition to the next era.
    - The arithmetic is `stability window + safe zone = 6k/f`, but the value could be different if the safe zone weren't equivalent to one stability window.
      The safe zone can't be less than a stability window, because then the leader schedule also couldn't be known at least a stability window before the epoch transition (the next era might have a different leadership schedule).
      But the safe zone could be greater than a stability window (eg maybe requiring the ledger rules end voting `7k/f` before the epoch transition).
    - The above terminology doesn't match Conway's new governance rules, but the nub is the same.
- In contrast to the forecast logic, the time translation logic does do translations beyond the safe zone when it's able to.
