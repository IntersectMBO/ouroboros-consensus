# Ledger Interface

This section describes the interface exposed by the Ledger Layer which is
used by the Consensus Layer.

<!--
```agda
{-# OPTIONS --safe #-}

-- TODO: The following should be likely located in Common.
open import Ledger.Prelude
open import Ledger.Crypto
open import Ledger.Types.Epoch

module InterfaceLibrary.Ledger
  (crypto : Crypto)
  (es     : _) (open EpochStructure es)
  where

open import Ledger.PParams using (PParams)
open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDelegatedStake)

```
-->

<!--
```agda
record LedgerInterface : Type₁ where
  field
```
-->

```agda

    NewEpochState         : Type
    getPParams            : NewEpochState → PParams
    getEpoch              : NewEpochState → Epoch
    getPoolDelegatedStake : NewEpochState → PoolDelegatedStake
    adoptGenesisDelegs    : NewEpochState → Slot → NewEpochState
    _⊢_⇀⦇_,NEWEPOCH⦈_     : ⊤ → NewEpochState → Epoch → NewEpochState → Type
```

<!--
```agda
    ⦃ Computational-NEWEPOCH ⦄ : Computational _⊢_⇀⦇_,NEWEPOCH⦈_ String
```
-->

<a id="fig:interface:ledger"></a>

[Specification contents](../../SPECIFICATION.md)
