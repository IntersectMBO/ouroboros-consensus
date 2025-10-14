\subsection{$\mathsf{CHAINHEAD}$ Transition}
\label{sec:chainhead-trans}

The Chain Head Transition rule ($\mathsf{CHAINHEAD}$) is the main rule of the blockchain layer
part of the STS. It calls $\mathsf{TICKF}$, $\mathsf{TICKN}$, and $\mathsf{PRTCL}$,
as sub-rules.

Its state is shown in Figure~\ref{fig:ts-types:chainhead} and consists of
the epoch specific state \afld{NewEpochState} and its signal is a block header. Its
state is shown in Figure~\ref{fig:ts-types:chainhead} and it consists of the following:

\begin{itemize}
  \item The operational certificate issue number map \afld{cs}.
  \item The epoch nonce \afld{η₀}.
  \item The pre-nonce \afld{pre-ηc}.
  \item The evolving nonce \afld{ηv}.
  \item The candidate nonce \afld{ηc}.
  \item The previous epoch hash nonce \afld{ηh}.
  \item The last header hash \afld{h}.
  \item The last slot \afld{sℓ}.
  \item The last block number \afld{bℓ}.
\end{itemize}

\begin{code}[hide]
-- {-# OPTIONS --safe #-}

open import InterfaceLibrary.Ledger
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions 
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Data.Rational.Ext
import Spec.VDF
open import Ledger.Prelude

module Spec.ChainHead
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)  
  (li     : LedgerInterface crypto es ss) (let open LedgerInterface li)
  (rs     : _) (open RationalExtStructure rs)  
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : Nonce) → Set )
  (setupVDF : (G : Set) → (Spec.VDF.VDF crypto nonces {G}))
  -- TODO implement nonce combination with epoch number
  (combinEIN : Epoch → Nonce → Nonce)
  -- TODO temporary parameters (required because of UpdateNonce)
  (G : Set) 
  (_*ᵍ_ : G × G → G) 
  (idᵍ : G) 
  (defaultNonce : Nonce)
  (defaultSlot : Slot)
  (bs     : BlockStructure crypto nonces es ss setupVDFGroup setupVDF combinEIN G _*ᵍ_ idᵍ defaultNonce) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  where

open import Spec.BaseTypes crypto using (OCertCounters)
open import Spec.TickForecast crypto es ss li
open import Spec.TickNonce crypto es nonces
open import Spec.Protocol crypto nonces es ss rs setupVDFGroup setupVDF combinEIN G _*ᵍ_ idᵍ defaultNonce bs af
open import Ledger.PParams crypto es ss using (PParams; ProtVer)
open import Ledger.Prelude
open import Spec.UpdateNonce crypto nonces es setupVDFGroup setupVDF combinEIN G _*ᵍ_ idᵍ defaultNonce 
\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Chain Head environments}
\begin{code}
ChainHeadEnv = NewEpochState
\end{code}
\emph{Chain Head states}
\begin{AgdaSuppressSpace}
\begin{code}
record LastAppliedBlock : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_⟧ℓ
  field
\end{code}
\begin{code}  
    bℓ : BlockNo    -- last block number
    sℓ : Slot       -- last slot
    h  : HashHeader -- latest header hash

record ChainHeadState : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_,_,_,_,_⟧ᶜˢ
  field
\end{code}
\begin{code}  
    cs     : OCertCounters          -- operational certificate issue numbers
    η₀     : Nonce                  -- epoch nonce
    pre-ηc : Nonce                  -- pre-nonce
    ηv     : Nonce                  -- evolving nonce
    ηc     : Nonce                  -- candidate nonce
    ηh     : Nonce                  -- nonce from hash of last epoch’s last header
    lab    : Maybe LastAppliedBlock -- latest applied block
\end{code}
\end{AgdaSuppressSpace}
\emph{Chain Head transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,CHAINHEAD⦈_ : ChainHeadEnv → ChainHeadState → BHeader → ChainHeadState → Type
\end{code}
\emph{Chain Head helper functions}
\begin{code}
chainChecks : ℕ → ℕ × ℕ × ProtVer → BHeader → Type
chainChecks maxpv (maxBHSize , maxBBSize , protocolVersion) bh =
  m ≤ maxpv × headerSize bh ≤ maxBHSize × bodySize ≤ maxBBSize
  where
    m = proj₁ protocolVersion
    open BHBody (proj₁ bh)

getLastSlot : Maybe LastAppliedBlock → Slot 
getLastSlot nothing = defaultSlot
getLastSlot (just lab) = lab .LastAppliedBlock.sℓ

lastAppliedHash : Maybe LastAppliedBlock → Maybe HashHeader
lastAppliedHash nothing               = nothing
lastAppliedHash (just ⟦ _ , _ , h ⟧ℓ) = just h

prtlSeqChecks : Maybe LastAppliedBlock → BHeader → Type
prtlSeqChecks nothing                    bh = ⊤
prtlSeqChecks lab@(just ⟦ bℓ , sℓ , _ ⟧ℓ) bh = sℓ < slot × bℓ + 1 ≡ blockNo × ph ≡ prevHeader
  where
    open BHBody (proj₁ bh)
    ph = lastAppliedHash lab
\end{code}
\end{AgdaAlign}
\caption{Chain Head transition system types and functions}
\label{fig:ts-types:chainhead}
\end{figure*}

The transition checks the following things
(via the functions \afun{chainChecks} and \afun{prtlSeqChecks} from Figure~\ref{fig:ts-types:chainhead}):
\begin{itemize}
\item The slot in the block header body is larger than the last slot recorded.
\item The block number increases by exactly one.
\item The previous hash listed in the block header matches the previous
  block header hash which was recorded.
\item The size of the block header is less than or equal to the maximal size that the
  protocol parameters allow for block headers.
\item The size of the block body, as claimed by the block header, is less than or equal to the
  maximal size that the protocol parameters allow for block bodies.
\item The node is not obsolete, meaning that the major component of the
  protocol version in the protocol parameters is not bigger than the constant \afld{MaxMajorPV}.
\end{itemize}

\begin{figure*}[h]
\begin{code}[hide]
private variable
  nes forecast                               : NewEpochState
  cs cs′                                     : OCertCounters
  η₀ pre-ηc pre-ηc′ ηv ηc ηh η₀′ ηv′ ηc′ ηh′ : Nonce
  lab                                        : Maybe LastAppliedBlock
  bh                                         : BHeader
  sig1ago sig2ago : UpdateNonceCommand
  2ago-ηstate  1ago-ηstate 1ago-ηstate' 2ago-ηstate' : UpdateNonceState 

data _⊢_⇀⦇_,CHAINHEAD⦈_ where
\end{code}
\begin{code}
  Chain-Head :
    let (bhb , _) = bh; open BHBody bhb
        e₁   = getEpoch nes
        e₂   = getEpoch forecast
        ne   = (e₁ ≠ e₂)
        pp   = getPParams forecast; open PParams
        nₚₕ  = prevHashToNonce (lastAppliedHash lab)
        pd   = getPoolDistr forecast
        lab′ = just ⟦ blockNo , slot , headerHash bh ⟧ℓ
        sₗ = getLastSlot lab 
    in
    ∙ prtlSeqChecks lab bh
    ∙ _ ⊢ nes ⇀⦇ slot ,TICKF⦈ forecast
    ∙ chainChecks MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh
    ∙ ⟦ ηc , nₚₕ ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ ne ,TICKN⦈ ⟦ η₀′ , ηh′ ⟧ᵗˢ
    ∙ ⟦ pd , η₀′ , sₗ ⟧ᵖᵉ ⊢ ⟦ cs , 2ago-ηstate , 1ago-ηstate , ηc ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ cs′ , 2ago-ηstate' , 1ago-ηstate' , ηc′ ⟧ᵖˢ
    ────────────────────────────────
    nes ⊢ ⟦ cs  , η₀  , pre-ηc , ηv  , ηc  , ηh  , lab  ⟧ᶜˢ ⇀⦇ bh ,CHAINHEAD⦈
          ⟦ cs′ , η₀′ , pre-ηc′ , ηv′ , ηc′ , ηh′ , lab′ ⟧ᶜˢ

\end{code}
\caption{Chain Head transition system rules}
\label{fig:ts-rules:chainhead}
\end{figure*}

The transition rule $\mathsf{CHAINHEAD}$ is shown in Figure~\ref{fig:ts-rules:chainhead} and
has the following predicate failures:
\begin{enumerate}
\item If the slot of the block header body is not larger than the last slot,
  there is a \emph{WrongSlotInterval} failure.
\item If the block number does not increase by exactly one, there is a \emph{WrongBlockNo} failure.
\item If the hash of the previous header of the block header body is not equal
  to the last header hash, there is a \emph{WrongBlockSequence} failure.
\item If the size of the block header is larger than the maximally allowed size,
  there is a \emph{HeaderSizeTooLarge} failure.
\item If the size of the block body is larger than the maximally allowed size,
  there is a \emph{BlockSizeTooLarge} failure.
\item If the major component of the protocol version is larger than \afld{MaxMajorPV},
  there is a \emph{ObsoleteNode} failure.
\end{enumerate}
