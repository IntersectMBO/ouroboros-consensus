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
  (grindingf : Nonce → Nonce)
  where

open import Ledger.Prelude

\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Update Nonce environments}
\begin{AgdaSuppressSpace}
\begin{code}
record UpdateNonceEnv : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_⟧ᵘᵉ
  field
\end{code}
\begin{code}  
    η : Nonce -- new nonce
\end{code}
\end{AgdaSuppressSpace}
\emph{Update Nonce states}
\begin{AgdaSuppressSpace}
\begin{code}
record UpdateNonceState : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_⟧ᵘˢ
  field
\end{code}
\begin{code}
    pre-ηc : Nonce -- evolving pre-nonce
    ηv : Nonce -- evolving nonce
    ηc : Nonce -- candidate nonce
\end{code}
\end{AgdaSuppressSpace}
\emph{Update Nonce transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,UPDN⦈_ : UpdateNonceEnv → UpdateNonceState → Slot → UpdateNonceState → Type
\end{code}
\end{AgdaAlign}
\caption{Update Nonce transition system types}
\label{fig:ts-types:updnonce}
\end{figure*}

The transition rule \aarg{UPDN} takes the slot \aarg{s} as signal and is shown in 
Figure~\ref{fig:ts-rules:updatenonce}. There are
three different cases for \aarg{UPDN}, all three of which always evolve the evolving nonce \aarg{ηv}
by combining it with the VRF result \aarg{η} from the block header :

\begin{itemize}
\item[(i)] \aarg{New-PreN}, where \aarg{s} is exactly the \aarg{RandomnessStabilisationWindowPlusOne}
slots until the start of the next epoch. A new pre-nonce is set equal to the (evolved) evolving nonce.
The candidate nonce is set to the value of the current pre-nonce updated by the anti-griding function.

\item[(ii)] \aarg{Update-All}, where \aarg{s} is not yet
\aarg{RandomnessStabilisationWindowPlusOne}\footnote{Note that in pre-Conway eras \stabilityWindow
was used instead of \aarg{RandomnessStabilisationWindowPlusOne}.} slots from the beginning
of the next epoch. The pre-nonce \aarg{pre-ηc}
is evolved by the anti-gringing function \aarg{grindingf}.
The candidate nonce \aarg{ηc} also transitions to
\aarg{grindingf~pre-ηc}. The reason for this is that even
though the candidate nonce is frozen sometime during the epoch, we want these two
nonces to be equal during this (first) part of a new epoch. 

\item[(iii)] \aarg{Keep-PreN}, where \aarg{s} is strictly less than the \aarg{RandomnessStabilisationWindowPlusOne}
slots before the start of the next epoch. The pre-nonce \aarg{pre-ηc} is updated by 
the anti-grinding function to \aarg{grindingf~pre-ηc}. The candidate nonce \aarg{ηc} remains fixed.
\end{itemize}

\begin{figure*}[h]
\begin{code}[hide]
private variable
  s              : Slot
  η ηv ηc pre-ηc : Nonce

data _⊢_⇀⦇_,UPDN⦈_ where
\end{code}
\begin{code}
  -- applies at : one slot right before candidate becomes fixed
  -- new pre-nonce from evolving nonce 
  -- candidate nonce set to final update of current pre-nonce
  New-PreN :
    ∙ s + RandomnessStabilisationWindowPlusOne ≡ firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ pre-ηc , ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηv ⋆ η , grindingf pre-ηc ⟧ᵘˢ

  -- applies at : all other slots before candidate is fixed
  -- pre-nonce updated with grindingf
  -- candidate nonce set the same as pre-nonce
  Update-All :
    ∙ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ pre-ηc , ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ grindingf pre-ηc , ηv ⋆ η , grindingf pre-ηc ⟧ᵘˢ

  -- applies at : within RandomnessStabilisationWindowPlusOne of next epoch
  -- pre-nonce updated with grindingf
  -- candidate nonce kept constant
  Keep-PreN :
    ∙ s + RandomnessStabilisationWindowPlusOne > firstSlot (sucᵉ (epoch s))
    ────────────────────────────────
    ⟦ η ⟧ᵘᵉ ⊢ ⟦ pre-ηc , ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ grindingf pre-ηc , ηv ⋆ η , ηc ⟧ᵘˢ
\end{code}
\caption{Update Nonce transition system rules}
\label{fig:ts-rules:updatenonce}
\end{figure*}
