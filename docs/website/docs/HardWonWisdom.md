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
  ... I wonder if every use of the `epochInfo` could do that same thing.

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
