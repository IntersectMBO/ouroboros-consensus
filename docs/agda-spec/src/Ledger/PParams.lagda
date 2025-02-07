\section{Protocol Parameters}
\label{sec:protocol-parameters}
This section defines the adjustable protocol parameters of the Cardano consensus.
These parameters are used in block header validation and can affect various features
of the system, such as maximum and minimum sizes of certain components, and more.
\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Data.Product.Properties
open import Data.Nat.Properties using (m+1+n≢m)

open import Tactic.Derive.DecEq
open import Tactic.Derive.Show

open import Ledger.Prelude

module Ledger.PParams where

private variable
  m n : ℕ
\end{code}

\begin{figure*}[ht]
\begin{AgdaMultiCode}
\begin{code}
ProtVer : Type
ProtVer = ℕ × ℕ
\end{code}
\begin{code}[hide]
instance
  Show-ProtVer : Show ProtVer
  Show-ProtVer = Show-×
\end{code}
\begin{code}
data pvCanFollow : ProtVer → ProtVer → Type where
  canFollowMajor : pvCanFollow (m , n) (m + 1 , 0)
  canFollowMinor : pvCanFollow (m , n) (m , n + 1)
\end{code}
\end{AgdaMultiCode}
\caption{Definitions related to protocol parameters}
\label{fig:protocol-parameter-defs}
\end{figure*}

\begin{figure*}[ht]
\begin{AgdaMultiCode}
\begin{code}
data PParamGroup : Type where
  NetworkGroup : PParamGroup
record PParams : Type where
\end{code}
\begin{code}[hide]
  field
\end{code}
\emph{Network group}
\begin{code}
    maxHeaderSize : ℕ
    maxBlockSize  : ℕ
    pv            : ProtVer
\end{code}
\end{AgdaMultiCode}
\caption{Protocol parameter definitions}
\label{fig:protocol-parameter-declarations}
\end{figure*}
\begin{figure*}
\begin{AgdaMultiCode}
\begin{code}
positivePParams : PParams → List ℕ
positivePParams pp = maxHeaderSize ∷ maxBlockSize ∷ []
\end{code}
\begin{code}[hide]
  where open PParams pp
\end{code}
\begin{code}

paramsWellFormed : PParams → Type
paramsWellFormed pp = 0 ∉ fromList (positivePParams pp)
\end{code}
\begin{code}[hide]
paramsWF-elim : (pp : PParams) → paramsWellFormed pp → (n : ℕ) → n ∈ˡ (positivePParams pp) → n > 0
paramsWF-elim pp pwf (suc n) x = z<s
paramsWF-elim pp pwf 0 0∈ = ⊥-elim (pwf (to ∈-fromList 0∈))
  where open Equivalence
\end{code}
\end{AgdaMultiCode}
\caption{Protocol parameter well-formedness}
\label{fig:protocol-parameter-well-formedness}
\end{figure*}
\begin{code}[hide]
instance
  unquoteDecl DecEq-PParams        = derive-DecEq
    ((quote PParams , DecEq-PParams) ∷ [])
  unquoteDecl Show-PParams        = derive-Show
    ((quote PParams , Show-PParams) ∷ [])
\end{code}

\PParams contains parameters used in the Cardano consensus, which we group according
to the general purpose that each parameter serves.

\begin{itemize}
  \item \NetworkGroup: parameters related to the network settings;
\end{itemize}

Figure~\ref{fig:protocol-parameter-declarations} also defines the
function \paramsWellFormed. It performs some sanity checks on protocol
parameters.

\begin{code}[hide]
instance
  pvCanFollow? : ∀ {pv} {pv'} → Dec (pvCanFollow pv pv')
  pvCanFollow? {m , n} {pv} with pv ≟ (m + 1 , 0) | pv ≟ (m , n + 1)
  ... | no ¬p    | no ¬p₁   = no $ λ where canFollowMajor → ¬p  refl
                                           canFollowMinor → ¬p₁ refl
  ... | no ¬p    | yes refl = yes canFollowMinor
  ... | yes refl | no ¬p    = yes canFollowMajor
  ... | yes refl | yes p    = ⊥-elim $ m+1+n≢m m $ ×-≡,≡←≡ p .proj₁
\end{code}
