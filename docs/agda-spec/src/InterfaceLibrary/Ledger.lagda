\begin{code}[hide]
{-# OPTIONS --safe #-}

-- TODO: The following should be likely located in Common.
open import Ledger.Prelude
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
import Spec.VDF
open import Spec.BaseTypes using (Nonces)

module InterfaceLibrary.Ledger
  (crypto : Crypto)
  (nonces : Nonces crypto) (open Nonces nonces)  
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : Spec.VDF.Discriminant crypto nonces) → Set )
  (setupVDF : (G : Set) → (Spec.VDF.VDF crypto nonces {G}))
  -- TODO implement nonce combination with epoch number
  (G : Set) 
  (_*ᵍ_ : G × G → G) 
  (idᵍ : G) 
  (defaultNonce : Nonce)
  where

open import Ledger.PParams crypto es ss using (PParams)
open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDistr)
open import Spec.UpdateNonce crypto nonces es setupVDFGroup setupVDF G _*ᵍ_ idᵍ defaultNonce 

\end{code}

\begin{figure*}[h]
\begin{code}[hide]
record LedgerInterface : Type₁ where
  field
\end{code}
\begin{code}
    BlockBody          : Type
    NewEpochState      : Type
    getPParams         : NewEpochState → PParams
    getEpoch           : NewEpochState → Epoch
    getPoolDistr       : NewEpochState → PoolDistr
    adoptGenesisDelegs : NewEpochState → Slot → NewEpochState
    _⊢_⇀⦇_,NEWEPOCH⦈_  : ⊤ → NewEpochState → Epoch → NewEpochState → Type
    getPhalanxCommand  : BlockBody → UpdateNonceCommand
\end{code}
\begin{code}[hide]
    ⦃ Computational-NEWEPOCH ⦄ : Computational _⊢_⇀⦇_,NEWEPOCH⦈_ String
\end{code}
\caption{Ledger interface}
\label{fig:interface:ledger}
\end{figure*}
