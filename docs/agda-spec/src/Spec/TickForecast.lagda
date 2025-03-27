\subsection{$\mathsf{TICKF}$ Transition}
\label{sec:tickf-trans}

The Tick Forecast Transition ($\mathsf{TICKF}$) performs some chain level
upkeep. Its state is shown in Figure~\ref{fig:ts-types:tickf} and consists of
the epoch specific state \afld{NewEpochState} necessary for the $\mathsf{NEWEPOCH}$ transition
and its signal is the current slot.
\begin{code}[hide]
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
