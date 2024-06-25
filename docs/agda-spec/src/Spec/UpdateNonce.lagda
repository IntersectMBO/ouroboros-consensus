\subsection{$\mathsf{UPDN}$ Transition}
\label{sec:update-nonces-trans}

The Update Nonce Transition ($\mathsf{UPDN}$) updates the nonces until the randomness gets fixed.
The environment is shown in Figure~\ref{fig:ts-types:updnonce} and consists of
the block nonce \afld{η}.
The state is shown in Figure~\ref{fig:ts-types:updnonce} and consists of
the candidate nonce \afld{ηc} and the evolving nonce \afld{ηv}.

\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)

module Spec.UpdateNonce
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  where

open import Ledger.Prelude

\end{code}

\begin{figure*}[h]
\emph{Update Nonce environments}
\begin{code}
record UpdateNonceEnv : Type where
  constructor ⟦_⟧ᵘᵉ
  field η : Nonce -- new nonce
\end{code}
\emph{Update Nonce states}
\begin{code}
record UpdateNonceState : Type where
  constructor ⟦_,_⟧ᵘˢ
  field ηv : Nonce -- evolving nonce
        ηc : Nonce -- candidate nonce
\end{code}
\emph{Update Nonce transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,UPDN⦈_ : UpdateNonceEnv → UpdateNonceState → Slot → UpdateNonceState → Type
\end{code}
\caption{Update Nonce transition system types}
\label{fig:ts-types:updnonce}
\end{figure*}

The transition rule $\mathsf{UPDN}$ takes the slot \aarg{s} as signal and is shown in 
Figure~\ref{fig:ts-rules:updatenonce}. There are
two different cases for $\mathsf{UPDN}$: one where \aarg{s} is not yet
\randomnessStabilisationWindow\footnote{Note that in pre-Conway eras \stabilityWindow
was used instead of \randomnessStabilisationWindow.} slots from the beginning
of the next epoch and one where \aarg{s} is less than \randomnessStabilisationWindow
slots until the start of the next epoch.

Note that in the first rule, the candidate nonce \aarg{ηc} transitions to
\aarg{ηv} \agdaNonceOp \aarg{η}, not \aarg{ηc} \agdaNonceOp \aarg{η}. The reason for this is that even
though the candidate nonce is frozen sometime during the epoch, we want the two
nonces to again be equal at the start of a new epoch.

\begin{figure*}[h]
\begin{code}[hide]
private variable
  s       : Slot
  η ηv ηc : Nonce

data _⊢_⇀⦇_,UPDN⦈_ where
\end{code}
\begin{code}
  Update-Both :
    ∙ s + RandomnessStabilisationWindow < firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηv ⋆ η ⟧ᵘˢ

  Only-Evolve :
    ∙ s + RandomnessStabilisationWindow ≥ firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηc ⟧ᵘˢ
\end{code}
\caption{Update Nonce transition system rules}
\label{fig:ts-rules:updatenonce}
\end{figure*}
