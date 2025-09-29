\begin{code}[hide]
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

\end{code}

\begin{figure*}[h]
\begin{code}[hide]
record LedgerInterface : Type₁ where
  field
\end{code}
\begin{code}
    NewEpochState         : Type
    getPParams            : NewEpochState → PParams
    getEpoch              : NewEpochState → Epoch
    getPoolDelegatedStake : NewEpochState → PoolDelegatedStake
    adoptGenesisDelegs    : NewEpochState → Slot → NewEpochState
    _⊢_⇀⦇_,NEWEPOCH⦈_     : ⊤ → NewEpochState → Epoch → NewEpochState → Type
\end{code}
\begin{code}[hide]
    ⦃ Computational-NEWEPOCH ⦄ : Computational _⊢_⇀⦇_,NEWEPOCH⦈_ String
\end{code}
\caption{Ledger interface}
\label{fig:interface:ledger}
\end{figure*}
