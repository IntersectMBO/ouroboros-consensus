\subsection{$\mathsf{PRTCL}$ Transition}
\label{sec:protocol-trans}

The Protocol Transition ($\mathsf{PRTCL}$) calls the transition $\mathsf{UPDN}$ to update the evolving
and candidate nonces, and checks the operational certificate with $\mathsf{OCERT}$. Its environment is shown in
Figure~\ref{fig:ts-types:prtcl} and consists of:

\begin{itemize}
  \item The stake pool stake distribution \afld{pd}.
  \item The epoch nonce \afld{η-epoch}.
\end{itemize}

Its state is shown in Figure~\ref{fig:ts-types:prtcl} and consists of
\begin{itemize}
  \item The operational certificate issue number mapping \afld{cs}.
  \item The pre-nonce \afld{pre-η-candidate}.  
  \item The evolving nonce \afld{ηv}.
  \item The candidate nonce for the next epoch \afld{η-candidate}.
\end{itemize}

\begin{code}[hide]
{-# OPTIONS --safe #-}

open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Data.Rational.Ext
open import Ledger.Prelude
import Spec.VDF

module Spec.Protocol
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)  
  (bs     : BlockStructure crypto nonces es ss) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (rs     : _) (open RationalExtStructure rs)
  -- TODO instantiate thes with correct VDF setup!
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : Spec.VDF.Discriminant crypto nonces) → Set )
  (setupVDF : (G : Set) → (Spec.VDF.VDF crypto nonces {G}))
  -- TODO temporary parameters (required because of UpdateNonce)
  (G : Set) 
  (_*ᵍ_ : G × G → G) 
  (idᵍ : G) 
  (defaultNonce : Nonce)
  where

open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDistr; lookupPoolDistr)
open import Spec.OperationalCertificate crypto nonces es ss bs af
open import Spec.BaseTypes crypto using (OCertCounters)
open import Data.Rational as ℚ using (ℚ; 0ℚ; 1ℚ)
open Ledger.Prelude.ℤ using (pos)
open import Spec.UpdateNonce crypto nonces es setupVDFGroup setupVDF G _*ᵍ_ idᵍ defaultNonce

\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Protocol environments}
\begin{AgdaSuppressSpace}
\begin{code}
record PrtclEnv : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_⟧ᵖᵉ
  field
\end{code}
\begin{code}  
    η-epoch  : Nonce     -- epoch nonce
    sₗ   : Slot
    param : Parametrized
\end{code}
\end{AgdaSuppressSpace}
\emph{Protocol states}
\begin{AgdaSuppressSpace}
\begin{code}
record PrtclState : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_,_⟧ᵖˢ
  field
\end{code}
\begin{code}
    cs     : OCertCounters -- operational certificate issues numbers
    pre-η-candidate : Nonce
    ηstate : UpdateNonceState -- Nonce state 
    candidate-η     : Nonce         -- candidate nonce
\end{code}
\end{AgdaSuppressSpace}
\emph{Protocol transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,PRTCL⦈_ : PrtclEnv → PrtclState → (BHeader × UpdateNonceCommand) → PrtclState → Type
\end{code}
\end{AgdaAlign}
\caption{Protocol transition system types}
\label{fig:ts-types:prtcl}
\end{figure*}

\begin{figure*}[h]
\begin{AgdaSuppressSpace}
\emph{Protocol helper functions}
\begin{code}
hBLeader : BHBody → Certifiedℕ
hBLeader bhb = serHashToℕ (hash (encode "L" ∥ encode vrfRes))
  where open BHBody bhb

hBNonce : BHBody → Nonce
hBNonce bhb = serHashToNonce (hash (encode "N" ∥ encode vrfRes))
  where open BHBody bhb

\end{code}
\begin{AgdaAlign}
\begin{code}
checkLeaderVal : Certifiedℕ → PosUnitInterval → ℚ → Type
checkLeaderVal (certℕ , certℕprf) (f , posf , f≤1) σ =
   if f ≡ 1ℚ then ⊤ else
\end{code}
\begin{code}[hide]
     λ{f≢1ℚ} →
\end{code}
\begin{code}
       let
         p = pos certℕ ℚ./ (2 ^ 512)
         q = 1ℚ ℚ.- p
\end{code}
\begin{code}[hide]       
         p≢1ℚ  = ↥p<↧p⇒p≢1 {p} (n<m⇒↥[n/m]<↧[n/m] certℕprf)
         1-f≥0 = p≤1⇒1-p≥0 f≤1
         1-f≢0 = p≢1⇒1-p≢0 f≢1ℚ
         instance
           q≢0ℚ  = ℚ.≢-nonZero (p≢1⇒1-p≢0 p≢1ℚ)
           1-f>0 = ℚ.positive (≤∧≢⇒< 1-f≥0 $ ≢-sym 1-f≢0)
\end{code}
\begin{code}
         c = ln (1ℚ ℚ.- f)
       in
         ℚ.1/ q ℚ.< exp ((ℚ.- σ) ℚ.* c)

\end{code}
\end{AgdaAlign}
\begin{code}
vrfChecks : Nonce → PoolDistr → PosUnitInterval → BHBody → Type
vrfChecks η-epoch pd f bhb =
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
    seed = slotToSeed slot XOR nonceToSeed η-epoch
\end{code}
\end{AgdaSuppressSpace}
\caption{Protocol transition system helper functions}
\label{fig:ts-funs:prtcl}
\end{figure*}

In Figure~\ref{fig:ts-funs:prtcl} we define a function \afun{vrfChecks} which performs all the VRF related checks
on a given block header body.
In addition to the block header body \aarg{bhb}, the function requires the epoch nonce \aarg{η-epoch},
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
  pd                              : PoolDistr
  cs cs'                          : OCertCounters
  η-candidate η-candidate' η-epoch : Nonce 
  pre-η-candidate pre-η-candidate' : Nonce
  ηstate ηstate' : UpdateNonceState 
  bh                              : BHeader
  sₗ : Slot
  params : Parametrized
  phalanxCommand : UpdateNonceCommand

data _⊢_⇀⦇_,PRTCL⦈_ where
\end{code}
\begin{code}
  Evolve-Prtcl :
    let (bhb , σ) = bh; open BHBody bhb
        η = hBNonce bhb
    in
    ∙ ⟦ sₗ , slot , η-epoch , params ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , η-candidate ) ⇀⦇ (sₗ , phalanxCommand) ,UPDN⦈ ( pre-η-candidate' , ηstate' , η-candidate' ) 
    ∙ dom (pd ˢ) ⊢ cs ⇀⦇ bh ,OCERT⦈ cs'
    ∙ vrfChecks η-epoch pd ActiveSlotCoeff bhb -- the new η-epoch comes from the candidate nonce composed with the new chain tip (hash of incoming block header/block no/slot no)
    ────────────────────────────────
    ⟦ η-epoch , sₗ , params ⟧ᵖᵉ ⊢ ⟦ cs , pre-η-candidate , ηstate , η-candidate ⟧ᵖˢ ⇀⦇ (bh , phalanxCommand) ,PRTCL⦈ ⟦ cs' , pre-η-candidate , ηstate' , η-candidate' ⟧ᵖˢ
\end{code}
\caption{Protocol transition system rules}
\label{fig:ts-rules:prtcl}
\end{figure*}

The transition rule $\mathsf{PRTCL}$ is shown in Figure~\ref{fig:ts-rules:prtcl}.
