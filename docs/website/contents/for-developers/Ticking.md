# Ticking

"Tick" is a common term in discrete systems for time passing.
Within Ouroboros, time is divided into slots, and to _tick_ a state X means to advance X from one slot to a later slot.
In the Cardano system, we tick a ledger state, a protocol state, or both at once (see `ExtLedgerState`).

The fundamental use case for ticking is to advance the ledger state between one block and the next on that same chain.
In Ouroboros Praos, one block and the next will be separated by zero or more slots; usually several, since Praos relies on time passing between blocks arising to limit short forks.
However, applying blocks is not the only reason the Consensus Layer ticks.
If it were, then ticking wouldn't need to be a distinguished step: it'd merely be the first of however many "hidden" steps are involved in applying a block.
Instead, ticking is used in a few places, some of which don't involve immediately subsequent header/block application.

- Ticking advances protocol state between headers.
- Ticking advances ledger states between blocks (via `ExtLedgerState`, this also ticks the protocol state).
- The leadership check ticks the protocol state from the selected chain's tip to the wall clock's current slot when checking whether this stake pool should mint a block.
- The mempool ticks the selected chain's ledger state ahead by exactly one slot, since the hypothetical next block to extend the selection has to be strictly younger than the selection.
  (Only Byron's Epoch Boundary Blocks can share their predecessor's slot, but EBBs also don't contain transactions.)
- Today's mint also ticks the ledger state up to the current slot, to determine the transaction limits (size, ExUnits, etc) for the new block.

The ledger rules for ticking involve some crucial concerns: deadlines come and go (despite being enforced in block applications), epochs begin and end, protocol parameters change, and even the ledger rules themselves might change.

## Forecasting and the Forecast Range

_Forecasting_ is an alternative to ticking that also represents the passage of time.
There are two key differences between ticking and forecasting.

- Ticking is only sound if the intervening slots are do not contain a block; it is erroneous to tick _past_ a block on the chain.
- Forecasting does not produce a complete ledger state.
  It specifically only yields the parts of the future ledger state that are already fully determined _regardless of blocks you're forecasting past_.

In Cardano, the only thing we ever need to forecast is the part of the ledger state that is necessary to validate a header (see `LedgerView`).
This can be fully determined regardless of intervening blocks because the ledger rules ensure the relevant parts of the ledger state do not change without a minimum duration of forewarning (eg by retaining occasional snapshots of this data).
That minimum duration of stability is therefore _the forecast range_, beyond which the code cannot necessarily do correct forecasting.

Thus forecasting from an (unticked) ledger state in slot X to slot Y -- within the forecast range --- must be equivalent to ticking from X to Y and then "forecasting" from Y to Y.
That'd be visualized as a square commuting diagram.
Note that ticking from X to Y is usually only sound if there's no blocks in that slot interval, but the part of the ledger that we forecast is stable, and so cannot be affected by those blocks, and so the ticking is sound in the specific context of characterizing forecasting.

## `Ticked` data family

We introduced the `Ticked` data family to ensure that the Consensus Layer ticks exactly once between blocks.
The abstract interface maps an unticked ledger state to a `Ticked` ledger state, and applying a header/block is the _only_ way to map a `Ticked` ledger state to an unticked ledger state.

By encoding this alternation between ticking and application in the types of the abstract interface, we prevent ourselves from accidentally ticking too few or too many times.

## Partiality of Ticking

In our code base, ticking is a total function.
Forecasting, on the other hand, is partial, since it is able to reject requests that violate the forecast range.
(It's important to note that such a request is not doomed: the node might just need to process another block or two before the slot of interest will be within the forecast range of its selected chain's tip.)

However, the Consensus Layer itself will never need to tick farther than the forecast range.
The ledger rules might actually be able to do so, since it's mostly just a matter of the state _not_ changing.
It could plausibly cause problems within the ledger rules if an entire epoch has no blocks, for example, so there's definitely no guarantee that the resulting ledger state would actually be useful.
But again: the Consensus Layer will never request such a tick.

The inductive argument is that all the ticking the Consensus Layer does is ultimately guarded by some forecast, unless someone has altered the node's files between executions (or if there's a bad bug somewhere else).

- The mempool only ticks by a single slot, so any non-degenerate forecast range supports that.
- The leadership check immediately aborts if it cannot forecast the `LedgerView`.
  The mint only proceeds if the leadership succeeded.
- The ChainSync client forecasts the `LedgerView` before it applies any header.
- The ChainSel logic only applies blocks whose headers arrived via a ChainSync client or were minted by this node.
  The node also processes blocks from its on-disk storage when initializing, but those blocks were either minted by this node, acquired by this node via ChainSync (and BlockFetch), written by another trusted node (eg via Mithril) between executions, or written by some other means that is not explicitly unsupported.

Thus, it's reasonable for the ticking function to have a total type because none of its calls within the node can fail; it's always guarded by a forecast.

## Cross-Era Ticking

The only time we have ever envisioned wanting to tick a ticked ledger state --- despite our current types preventing it --- is in the Hard Fork Combinator.

In particular, the HFC must tick from one era into the next era when appropriate.
In today's code, the HFC achieves this by translating the ledger state after the final block of an era into the next era, and then ticking that ledger state across the era boundary.
But that involves a ledger state of the next era existing in a slot before that era starts, which is confusing and has lead to some errors in the past.
Conceptually, it'd be much clearer to tick the ledger state up to the era boundary, then translate it, and then tick the rest of the way.
But our current `Ticked` typing disallows that kind of incremental ticking, unless the translation's type does unticking, and that also risks confusion.

It's worth noting that the HFC already uses a custom interface for forecasting across eras.
We could [introduce a similar interface for ticking across eras](https://github.com/IntersectMBO/ouroboros-consensus/issues/345).
The implementation for some era boundary, which is lower-level than the `Ticked`-based interface, could use the less surprising tick-translate-tick scheme.

Moreover, that cross-era ticking would still be guarded by cross-era forecasting, and so it would be bounded by the cross-era forecast range (which is not necessarily equivalent to either era's internal forecast range).
In particular, the cross-era forecast range cannot cross multiple era boundaries, which is justified by lower bounds on how much warning there is within an era ahead of the next era transition.
Thus the HFC only requires a cross-era ticking function from each era to the next, rather than to even later eras.

## Bounding Ticking and Forecasting Computations

On a single valid chain, the Consensus Layer will never tick across the same slots multiple times.
However, Ouroboros involves short forks and potentially invalid blocks.
On the other hand, even for a single chain, the Consensus Layer would forecast across the same slots multiple times: it does it each time the wall clock enters a new slot as part of the leadership check, and it does so each time any peer sends it a header.

Ideally all ticks would be inexpensive, but the occasional spike is managable, since short forks are short lived.
An expensive forecast, on the other hand, can be disastrous.
If the leadership check's forecast takes too long, then the node might be late to produce its block.
If that forecast takes longer than a slot, then the node might not even check every slot, which is unacceptable.

For these reasons, the ledger rules contain certain optimizations that either prevent forecasts from ever paying certain costs or else ensure that forecasting across some particular slot is only expensive the first time it happens and subsequent forecasts will then avoid those costs via memoization.
