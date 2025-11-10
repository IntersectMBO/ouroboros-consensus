\subsection{$\mathsf{TICKF}$ Transition}
\label{sec:tickf-trans}

The Tick Forecast Transition ($\mathsf{TICKF}$) performs some chain level
upkeep. Its state is shown in Figure~\ref{fig:ts-types:tickf} and consists of
the epoch specific state \afld{NewEpochState} necessary for the $\mathsf{NEWEPOCH}$ transition
and its signal is the current slot.
\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Ledger.Script
open import Ledger.Types.Epoch
open import InterfaceLibrary.Ledger
open import Ledger.Crypto
open import Spec.BaseTypes using (Nonces)
open import Ledger.Prelude
import Spec.VDF

module Spec.TickForecast
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)  
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : Spec.VDF.Discriminant crypto nonces) → Set )
  (setupVDF : (G : Set) → (Spec.VDF.VDF crypto nonces {G}))
  -- TODO temporary parameters (required because of UpdateNonce)
  (G : Set) 
  (_*ᵍ_ : G × G → G) 
  (idᵍ : G) 
  (defaultNonce : Nonce)
  (defaultSlot : Slot)
  (li     : LedgerInterface crypto nonces es ss setupVDFGroup setupVDF G _*ᵍ_ idᵍ defaultNonce ) (let open LedgerInterface li)
  where


\end{code}

\begin{figure*}[h]
\emph{Tick Forecast transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,TICKF⦈_ : ⊤ → NewEpochState → Slot → NewEpochState → Type
\end{code}
\caption{Tick forecast transition system types}
\label{fig:ts-types:tickf}
\end{figure*}

The transition $\mathsf{TICKF}$ is shown in Figure~\ref{fig:ts-rules:tickf}. Part of the upkeep
is updating the genesis key delegation mapping according to the future delegation mapping using
the helper function \afld{adoptGenesisDelegs}. One sub-transition is done: The $\mathsf{NEWEPOCH}$
transition performs any state change needed if it is the first block of a new epoch.

\begin{figure*}[h]
\begin{code}[hide]
private variable
  nes nes′ : NewEpochState
  s        : Slot

data _⊢_⇀⦇_,TICKF⦈_ where
\end{code}
\begin{code}
  Tick-Forecast :
    let forecast = adoptGenesisDelegs nes′ s
    in
    ∙ _ ⊢ nes ⇀⦇ epoch s ,NEWEPOCH⦈ nes′
    ────────────────────────────────
    _ ⊢ nes ⇀⦇ s ,TICKF⦈ forecast
\end{code}
\caption{Tick forecast transition system rules}
\label{fig:ts-rules:tickf}
\end{figure*}
