This document contains a loosely organized list of small summaries of realizations we had while working on the code base.

We do eventually want to integrate these lessons learned into more coherent and targetted documents.
But step one one is to record them at all; this file is supposed to make that easy.
Step two will be to occasionally extract cohesive insights from this bag, creating new documents or refining old ones accordingly.

## Why doesn't Ledger code ever return `PastHorizonException`?

One of the `HardForkBlock` combinator's major responsibilities is providing an `EpochInfo` to the ledger code.
This `EpochInfo` uses the `Either` monad to return `Right` only when the query can be answered with certainty.
For more information on when that is, see the Consensus Report and the recordings of Edsko's presentations at the Weekly IOG Seminar.

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
              return
            $ TPraos.TickedPraosLedgerView
            $ SL.currentLedgerView shelleyLedgerState
        | for < maxFor        -> return $ futureLedgerView for
        | otherwise           -> throwError OutsideForecastRange { ... }
    where
      ShelleyLedgerState {shelleyLedgerState} = ledgerState

      globals = shelleyLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      at      = ledgerTipSlot ledgerState

      futureLedgerView :: SlotNo -> Ticked (SL.LedgerView (EraCrypto era))
      futureLedgerView =
        either
          (\e -> error ("futureLedgerView failed: " <> show e))
          TPraos.TickedPraosLedgerView
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

When we talk about forecasting, we mean about the process of trying to get a ticked ledger view from a ledger state for a given slot. This ledger view can then be used to verify the validity of headers in that slot that live on the same chain as the original ledger state.

Hence, in the context of the HFC which has to support forecasts across era boundaries, forecasting can be thought of to have type
```haskell
   SlotNo
-> LedgerState blk
-> Either OutsideForecastRange (Ticked (LedgerView (BlockProtocol blk')))
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
    - The `LedgerView` actually only depends on the `ConsensusProtocol`, which only changed from Alonzo/TPraos to Babbage/Praos (Vasil HF), and even there, the translation only consists of un- and rewrapping (see `translateTickedLedgerView`).
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
The ledger state _might_ also determine when the ledger state's own era will end.
Thus, every ledger state determines the start time of `N` eras and the end time of the preceding `N-1` eras (note: there's one more start time than end times), where the ledger state inhabits either the `N-1`th or the `N`th era.
Subsequent ledger states may have greater `N`s, but the invariant remains true.
Every `N` will be at least 1, at most the length of the sequence of eras the code supports, and subsequent ledger states on a chain cannot have a lesser `N`.

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
Thus, we cannot handle a translation query unless we know which era its argument---be it a slot, an epoch, or a UTC time---is in and the durations of all preceding eras, or, equivalently, the start times of all eras up-to-and-including the argument is in.

Each query is handled by some piece of code and with respect to some ledger state.
The code (including config files) determines the supported eras (including their epoch size and slot length), and the ledger state determines which prefix of eras has known start times.
The known start times are enough to recognize when the given argument is during some era with a known end time, in which case the translation succeeds.
(The translation starts from the chain's start time, adds the duration of all preceding eras, and then adds the relative time since the beginning of the argument's era.)
Otherwise, we need more information to know whether the given argument is during the last era with a known start time, ie the first era with an unknown end time.
When the query argument is after that start time, the fact that the era's end is not yet known means that the query argument maybe either be in that era or instead be in a future era with a different epoch size/slot length, and so the correct translation may differ from the hypothetical response derived from the assumption the query argument precedes the first unknown end time.

For the first era with an unknown end time (ie the last era with a known start time), the code relies an a lower bound for that end time, ie the soonest that era could possibly end (aka "the horizon" or "the forecast horizon").
If the query argument precedes that horizon, then the translation succeeds, otherwise it fails.
For this purpose, the code (including config files) cannot support an era unless it also knows a "safe zone" for that era, where the safe zone (along with any additional assumptions the Consensus code makes) is enough information to determine the horizon induced by a given ledger state.

In general, it would be possible to design a system such that even the final ledger state in an era did not determine when that era would end.
That'd be an empty safe zone, consisting of zero slots.
For example, perhaps the first block of an era identifies itself as such simply by setting some field in its header.
For that kind of chain, some ledger states would not provide enough information to do any translations whatsoever of slots/times/epochs _after_ that ledger state's tip.

For the case of Cardano, however, other concerns (not directly related to slot/epoch/time translations) already require a seperate cutoff, similar to the horizon, to be above some minimum.
For the sake of simplicity, we take that same minimum to be the safe zone.
As a result, the safe zone is equal to one _stability window_, which is `2k` slots for Byron and `3k/f` slots for every Shelley era.

### Aside: The Necessary Stability of Header Validity

[Esgen suggests that this could eventually be migrated to section 24.5 ("Eliminating safe zones") of the Consensus and Storage Layour Report (or the succeeding section).]

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

For example, [Shelley the `PPUP` ledger rule](https://github.com/input-output-hk/cardano-ledger/blob/180271602640bcac1214084b6de61d0468332f00/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Ppup.hs#L192) requires update proposals to be settled at least two stability windows before the end of the epoch (ie `6k/f`, not just `3k/f`).
(That links to the tip of the `master` branch at the time of writing this, although this constraint is not new.)

(TODO Frisby wonders whether, instead of double-stability in the ledger rules, we could instead only draw conclusions that should not be subject to roll back (such as time translations) from the youngest immutable ledger state.)

### Answer to the Question

The ultimate answer to the Office Hours question is that the query will fail if and only if its argument is beyond the conservative estimate for the end of the first era without a known end time.
That estimate will usually be the start of the least epoch that begins more than `6k/f` slots after the tip of ledger state that was used to answer the query.

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

## The HFC and `EpochInfo`s

One of the `HardForkBlock` combinator's major responsibilities is providing an `EpochInfo` to the ledger code.
What are the constraints on that `EpochInfo`?

The HFC uses the underlying blocks' ledger rules in the following ways.

- within a single era
    - via the Consensus Layer's typical interface
        - forecasting from a nonticked ledger state to a ledger view of a slot
        - using that ledger view to tick a chain-dependent state to the same slot
        - applying a header to a ticked chain-dependent state
        - ticking a ledger state to a slot
        - applying a block/transaction to a ticked ledger state
        - † chain selection
    - via the additional `CanHardFork` interface (specifically via `SingleEraBlock`)
        - † determining whether a nonticked ledger state already determines when its era will end
- across each era transition
    - via the additional `CanHardFork` interface
        - forecasting from a nonticked ledger state to a ledger view of a slot in the next era
        - translating a nonticked chain-dependent state to the next era
        - translating a nonticked ledger state to the next era
        - translating a transaction to the next era
- across one or more era transitions
    - via the additional `CanHardFork` interface
        - † chain selection

Crucially, _all_ of those ledger rules entrypoints require an `EpochInfo` (supplifed via either `ConsensusConfig` or `LedgerConfig`), except for the two prefixed with a †.
What are the constraints on that `EpochInfo` in each entrypoint?

- within a single era
    - _forecasting from a nonticked ledger state to a ledger view of a slot_.
      This function must already throw an exception when the given slot is more than on stability window ahead of the nonticked ledger state.
      Thus the `EpochInfo` must be defined at least up to that horizon.
    - _using that ledger view to tick a chain-dependent state to the same slot_
      This function does not require anything beyond the constraint from the forecast that created the ledger view.
    - _applying a header to a ticked chain-dependent state_
      This function does not require anything beyond the constraint from the forecast that created the ledger view that was used to create the ticked chain-dependent state.
    - _ticking a ledger state to a slot_.
      This is the most involved and enlightening case; see the detailed discussion below.
    - _applying a block/transaction to a ticked ledger state_.
      For example, Plutus requires interpreting a transaction's slot-based validity interval as the UTC times of it two endpoints.
      TODO If I recall corectly, the current bound on those slots is unfortunately defined as "whatever the HFC supportss"; what is the _intended_ bound on those slots?
- across each era transition
    - _forecasting from a nonticked ledger state to a ledger view of a slot in the next era_.
      TODO ???
    - _translating a nonticked chain-dependent state to the next era_.
      TODO ???
    - _translating a nonticked ledger state to the next era_.
      TODO ???
    - _translating a transaction to the next era_.
      TODO ???

Consider again the rule for _ticking a ledger state to a slot_.
The only information the HFC has that a ledger state does not is the (static) configuration data.
In the current architecture, though, that configuration data includes an `EpochInfo`, and so it's possible this ledger rule necessarily scrutinizes the given `EpochInfo`.
Still, there is a minimum amount of scrunity that should suffice in the theoretical extreme.
Specifically, it should be sufficient for this `EpochInfo` to be defined only up to the slot of the nonticked ledger state, since, given the rest of the configuration data and the entire ledger state, the rule should be able to extrapolate any `EpochInfo` beyond that.
On the other hand, separation of concerns suggests that the ticking rule should not need to do that extrapolation, since it would overlap with the responsibility of the `SingleEraBlock` instance.

Recall that this ticking rule is not responsible for the next era.
It would therefore be reasonable for the rule to assume that the destination slot is in the same era as the nonticked ledger state.
(TODO the tick-then-translate would directly violate that.
Other similar assumptions would be violated by translate-then-tick, eg the destination slot is the same era as the most recent block.
See the aside below.)
Under that assumption, the HFC should be able to define the `EpochInfo` at least up to the destination slot.

TODO conclusion: the `SingleEraBlock` rule should be able to do extrapolation when being used to prepare the `EpochInfo` for a ticking rule; ie it should be able to leverage the known-absence of subsequent blocks up to some slot

*Aside*.
Moreover, the ticking rule and the `SingleEraBlock` instance are explicitly not responsible for next era.
However, the current HFC interface---before PR #358---forces the HFC to use this intra-era ticking rule to implement cross-era ticks.
EG if the ticking rule did ask the `EpochInfo` about the given ledger state's slot and then extrapolate, the HFC's current "translate*-then-tick" scheme for cross-era ticks would be incoherent: the translated-but-not-ticked ledger state would be asking for information about a slot that is not in that ledger state's era!
The "tick-then-translate*" scheme would similarly be incoherent, since the first era's tick rule would be extrapolating about slots not in the next era.
Decomposing the cross-era ticks to end at era boundaries would avoid this incoherence, as in the "tick-then-translate-then-tick" scheme, regardless of which tick actually _crossed_ the epoch boundary.
