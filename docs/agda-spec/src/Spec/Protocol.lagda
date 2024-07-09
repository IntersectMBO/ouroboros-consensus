\subsection{$\mathsf{PRTCL}$ Transition}
\label{sec:protocol-trans}

The Protocol Transition ($\mathsf{PRTCL}$) calls the transition $\mathsf{UPDN}$ to update the evolving
and candidate nonces, and checks the operational certificate with $\mathsf{OCERT}$. Its environment is shown in
Figure~\ref{fig:ts-types:prtcl} and consists of:

\begin{itemize}
  \item The stake pool stake distribution \afld{pd}.
  \item The epoch nonce \afld{η₀}.
\end{itemize}

Its state is shown in Figure~\ref{fig:ts-types:prtcl} and consists of
\begin{itemize}
  \item The operational certificate issue number mapping \afld{cs}.
  \item The evolving nonce \afld{ηv}.
  \item The candidate nonce for the next epoch \afld{ηc}.
\end{itemize}

\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Data.Rational.Ext

module Spec.Protocol
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)  
  (bs     : BlockStructure crypto nonces es ss) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (rs     : _) (open RationalExtStructure rs)
  where

open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDistr; lookupPoolDistr)
open import Spec.OperationalCertificate crypto nonces es ss bs af
open import Spec.UpdateNonce crypto nonces es
open import Spec.BaseTypes crypto using (OCertCounters)
open import Data.Rational as ℚ using (ℚ; 0ℚ; 1ℚ)
open import Ledger.Prelude
open Ledger.Prelude.ℤ using (pos)

\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Protocol environments}
\begin{AgdaSuppressSpace}
\begin{code}
record PrtclEnv : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_⟧ᵖᵉ
  field
\end{code}
\begin{code}  
    pd  : PoolDistr -- pool stake distribution
    η₀  : Nonce     -- epoch nonce
\end{code}
\end{AgdaSuppressSpace}
\emph{Protocol states}
\begin{AgdaSuppressSpace}
\begin{code}
record PrtclState : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_⟧ᵖˢ
  field
\end{code}
\begin{code}
    cs : OCertCounters -- operational certificate issues numbers
    ηv : Nonce         -- evolving nonce
    ηc : Nonce         -- candidate nonce
\end{code}
\end{AgdaSuppressSpace}
\emph{Protocol transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,PRTCL⦈_ : PrtclEnv → PrtclState → BHeader → PrtclState → Type
\end{code}
\end{AgdaAlign}
\caption{Protocol transition system types}
\label{fig:ts-types:prtcl}
\end{figure*}

\begin{figure*}[h]
\emph{Protocol helper functions}
\begin{code}
hBLeader : BHBody → ∃[ n ] n < 2 ^ 512 -- [0, 2^512)
hBLeader bhb = serHashToℕ (hash (encode "L" ∥ encode vrfRes))
  where open BHBody bhb

hBNonce : BHBody → Nonce
hBNonce bhb = serHashToNonce (hash (encode "N" ∥ encode vrfRes))
  where open BHBody bhb

checkLeaderVal : ∃[ n ] n < 2 ^ 512 → PosUnitInterval → ℚ → Type
checkLeaderVal (certℕ , certℕprf) (f , posf , f≤1) σ =
   if f ≡ 1ℚ then ⊤ else λ{f≢1ℚ} →
     let
       p     = pos certℕ ℚ./ (2 ^ 512)
       p≢1ℚ  = ↥p<↧p⇒p≢1 {p} (n<m⇒↥[n/m]<↧[n/m] certℕprf)
       q     = 1ℚ ℚ.- p
       1-f≥0 = p≤1⇒1-p≥0 f≤1
       1-f≢0 = p≢1⇒1-p≢0 f≢1ℚ
       instance
         q≢0ℚ  = ℚ.≢-nonZero (p≢1⇒1-p≢0 p≢1ℚ)
         1-f>0 = ℚ.positive (≤∧≢⇒< 1-f≥0 $ ≢-sym 1-f≢0)
       c = ln (1ℚ ℚ.- f)
     in
       ℚ.1/ q < exp ((ℚ.- σ) ℚ.* c)

vrfChecks : Nonce → PoolDistr → PosUnitInterval → BHBody → Type
vrfChecks η₀ pd f bhb =
  case lookupPoolDistr pd hk of
    λ where
      nothing            → ⊥
      (just (σ , vrfHK)) →
           vrfHK ≡ hash vrfVk
         × verify vrfVk seed (vrfPrf , vrfRes)
         × checkLeaderVal (hBLeader bhb) f σ
  where
    open BHBody bhb
    hk = hash issuerVk
    seed = slotToSeed slot XOR nonceToSeed η₀
\end{code}
\caption{Protocol transition system helper functions}
\label{fig:ts-funs:prtcl}
\end{figure*}

In Figure~\ref{fig:ts-funs:prtcl} we define a function \afun{vrfChecks} which performs all the VRF related checks
on a given block header body.
In addition to the block header body \aarg{bhb}, the function requires the epoch nonce \aarg{η₀},
the stake distribution \aarg{pd} (aggregated by pool), and the active slots coefficient \aarg{f} from the protocol
parameters. The function checks:

\begin{itemize}
\item The validity of the proofs \afld{vrfPrf} for the leader value and the new nonce.
\item The verification key \aarg{vrfHK} is associated with relative stake \aarg{σ} in the stake distribution.
\item The \afun{hBLeader} value of \aarg{bhb} indicates a possible leader for
  this slot. The function \afun{checkLeaderVal}, defined in Figure~\ref{fig:ts-funs:prtcl}, performs this check.
\end{itemize}

The function \afun{vrfChecks} has the following predicate failures:

\begin{enumerate}
\item If the VRF key is not in the pool distribution, there is a \emph{VRFKeyUnknown} failure.
\item If the VRF key hash does not match the one listed in the block header,
  there is a \emph{VRFKeyWrongVRFKey} failure.
\item If the VRF generated value in the block header does not validate
  against the VRF certificate, there is a \emph{VRFKeyBadProof} failure.
\item If the VRF generated leader value in the block header is too large
  compared to the relative stake of the pool, there is a \emph{VRFLeaderValueTooBig} failure.
\end{enumerate}

\begin{figure*}[h]
\begin{code}[hide]
private variable
  pd               : PoolDistr
  cs cs′           : OCertCounters
  ηv ηc ηv′ ηc′ η₀ : Nonce
  bh               : BHeader

data _⊢_⇀⦇_,PRTCL⦈_ where
\end{code}
\begin{code}
  Evolve-Prtcl :
    let (bhb , σ) = bh; open BHBody bhb
        η = hBNonce bhb
    in
    ∙ ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ slot ,UPDN⦈ ⟦ ηv′ , ηc′ ⟧ᵘˢ
    ∙ dom (pd ˢ) ⊢ cs ⇀⦇ bh ,OCERT⦈ cs′
    ∙ vrfChecks η₀ pd ActiveSlotCoeff bhb
    ────────────────────────────────
    ⟦ pd , η₀ ⟧ᵖᵉ ⊢ ⟦ cs , ηv , ηc ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ cs′ , ηv′ , ηc′ ⟧ᵖˢ
\end{code}
\caption{Protocol transition system rules}
\label{fig:ts-rules:prtcl}
\end{figure*}

The transition rule $\mathsf{PRTCL}$ is shown in Figure~\ref{fig:ts-rules:prtcl}.
