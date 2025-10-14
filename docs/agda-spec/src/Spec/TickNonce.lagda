\subsection{$\mathsf{TICKN}$ Transition}
\label{sec:tick-nonce-trans}

The Tick Nonce Transition ($\mathsf{TICKN}$) is responsible for updating the epoch nonce and the
previous epoch's hash nonce at the start of an epoch. Its environment is shown in
Figure~\ref{fig:ts-types:ticknonce} and consists of the candidate nonce \afld{ηc}
and the previous epoch's last block header hash as a nonce \afld{ηph}.
Its state consists of the epoch nonce \afld{η₀} and the previous epoch's last block header hash nonce \afld{ηh}.

\begin{code}[hide]
-- {-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Spec.BaseTypes using (Nonces)

module Spec.TickNonce
  (crypto : _) (open Crypto crypto)
  (es     : _) (open EpochStructure es)
  (nonces : Nonces crypto) (open Nonces nonces)
  where

open import Ledger.Prelude

\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Tick Nonce environments}
\begin{AgdaSuppressSpace}
\begin{code}
record TickNonceEnv : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_⟧ᵗᵉ
  field
\end{code}
\begin{code}
    ηc  : Nonce     -- candidate nonce
    ηph : Nonce     -- previous header hash as nonce
\end{code}
\end{AgdaSuppressSpace}
\emph{Tick Nonce states}
\begin{AgdaSuppressSpace}
\begin{code}
record TickNonceState : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_⟧ᵗˢ
  field
\end{code}
\begin{code}  
    η₀ : Nonce -- epoch nonce
    ηh : Nonce -- nonce from hash of previous epoch's last block header
\end{code}
\end{AgdaSuppressSpace}
\emph{Tick Nonce transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,TICKN⦈_ : TickNonceEnv → TickNonceState → Bool → TickNonceState → Type
\end{code}
\end{AgdaAlign}
\caption{Tick Nonce transition system types}
\label{fig:ts-types:ticknonce}
\end{figure*}

The signal to the transition rule $\mathsf{TICKN}$ is a marker indicating
whether we are in a new epoch. If we are in a new epoch, we update the epoch
nonce and the previous hash. Otherwise, we do nothing. The $\mathsf{TICKN}$ rule
is shown in Figure~\ref{fig:ts-rules:ticknonce}.

\begin{figure*}[h]
\begin{code}[hide]
private variable
  ηc ηph η₀ ηh : Nonce

data _⊢_⇀⦇_,TICKN⦈_ where 
\end{code}
\begin{code}
  Not-New-Epoch :
    ────────────────────────────────
    ⟦ ηc , ηph ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ false ,TICKN⦈ ⟦ η₀ , ηh ⟧ᵗˢ

  New-Epoch :
    ────────────────────────────────
    ⟦ ηc , ηph ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ true ,TICKN⦈ ⟦ ηc ⋆ ηh , ηph ⟧ᵗˢ
\end{code}
\caption{Tick Nonce transition system rules}
\label{fig:ts-rules:ticknonce}
\end{figure*}
