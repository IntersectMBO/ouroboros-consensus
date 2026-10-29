# TICKF Transition

<a id="sec:tickf-trans"></a>

The Tick Forecast Transition ($\mathsf{TICKF}$) performs some chain level
upkeep. Its state consists of the epoch specific state `NewEpochState` necessary 
for the $\mathsf{NEWEPOCH}$ transition and its signal is the current slot.

<!--
```agda
{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Types.Epoch
open import InterfaceLibrary.Ledger

module Spec.TickForecast
  (crypto : Crypto)
  (es     : _) (open EpochStructure es)
  (li     : LedgerInterface crypto es) (let open LedgerInterface li)
  where

open import Ledger.Prelude

```
-->

<a id="fig:ts-types:tickf"></a>

*Tick Forecast transitions*

<!--
```agda
data
```
-->

```agda
  _⊢_⇀⦇_,TICKF⦈_ : ⊤ → NewEpochState → Slot → NewEpochState → Type
```

The transition $\mathsf{TICKF}$ is shown below. Part of the upkeep
is updating the genesis key delegation mapping according to the future delegation mapping using
the helper function `adoptGenesisDelegs`. The transition invokes one subrule, $\mathsf{NEWEPOCH}$, which
performs any state change needed if it is the first block of a new epoch.

<!--
```agda
private variable
  nes nes′ : NewEpochState
  s        : Slot

data _⊢_⇀⦇_,TICKF⦈_ where
```
-->

<a id="fig:ts-rules:tickf"></a>

```agda
  Tick-Forecast :
    let forecast = adoptGenesisDelegs nes′ s
    in
    ∙ _ ⊢ nes ⇀⦇ epoch s ,NEWEPOCH⦈ nes′
    ────────────────────────────────
    _ ⊢ nes ⇀⦇ s ,TICKF⦈ forecast
```

[Specification contents](../../SPECIFICATION.md)
