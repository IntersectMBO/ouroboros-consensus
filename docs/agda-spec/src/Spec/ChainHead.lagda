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
  \item The evolving nonce \afld{ηv}.
  \item The candidate nonce \afld{ηc}.
  \item The previous epoch hash nonce \afld{ηh}.
  \item The last header hash \afld{h}.
  \item The last slot \afld{sℓ}.
  \item The last block number \afld{bℓ}.
\end{itemize}

\begin{code}[hide]
{-# OPTIONS --safe #-}

open import InterfaceLibrary.Ledger
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Types.Epoch
open import Data.Rational.Ext

module Spec.ChainHead
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (bs     : BlockStructure crypto nonces es) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (li     : LedgerInterface crypto es) (let open LedgerInterface li)
  (rs     : _) (open RationalExtStructure rs)  
  where

open import Spec.BaseTypes crypto using (OCertCounters; PoolDistr)
open import Spec.TickForecast crypto es li
open import Spec.TickNonce crypto es nonces
open import Spec.Protocol crypto nonces es bs af rs
open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDelegatedStake)
open import Ledger.PParams using (PParams; ProtVer)
open import Ledger.Prelude
open import Data.Rational using (ℚ; normalize)
open import Data.Product renaming (map₁ to ×-map₁)

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
  constructor ⟦_,_,_,_⟧ℓ
  field
\end{code}
\begin{code}
    bℓ  : BlockNo           -- last block number
    sℓ  : Slot              -- last slot
    h   : HashHeader        -- latest header hash
    aeb : Maybe AnnouncedEB -- endorser block announced by the last block
\end{code}
\end{AgdaSuppressSpace}
\begin{AgdaSuppressSpace}
\begin{code}
record ChainHeadState : Type where
\end{code}
\begin{code}[hide]
  constructor ⟦_,_,_,_,_,_⟧ᶜˢ
  field
\end{code}
\begin{code}  
    cs  : OCertCounters          -- operational certificate issue numbers
    η₀  : Nonce                  -- epoch nonce
    ηv  : Nonce                  -- evolving nonce
    ηc  : Nonce                  -- candidate nonce
    ηh  : Nonce                  -- nonce from hash of last epoch’s last header
    lab : Maybe LastAppliedBlock -- latest applied block
\end{code}
\end{AgdaSuppressSpace}
\emph{Chain Head transitions}
\begin{code}[hide]
data
\end{code}
\begin{code}
  _⊢_⇀⦇_,CHAINHEAD⦈_ : ChainHeadEnv → ChainHeadState → BHeader → ChainHeadState → Type
\end{code}
\end{AgdaAlign}
\caption{Chain Head transition system types}
\label{fig:ts-types:chainhead}
\end{figure*}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Chain Head helper functions}
\begin{code}
extractPoolDistr : PoolDelegatedStake → PoolDistr
extractPoolDistr pds = mapValues (×-map₁ stakeToProportion) pds
  where
    totalStake : Coin
    totalStake = ∑[ c ← mapValues proj₁ pds ] c

    stakeToProportion : Coin → ℚ
    stakeToProportion c = case totalStake of λ where
      0         → normalize 0 1
      t@(suc n) → normalize c t

chainChecks : ℕ → ℕ × ℕ × ProtVer → BHeader → Type
chainChecks maxpv (maxBHSize , maxBBSize , protocolVersion) bh =
  m ≤ maxpv × headerSize bh ≤ maxBHSize × bodySize ≤ maxBBSize
  where
    m = proj₁ protocolVersion
    open BHeader; open BHBody (bh .body)

lastAppliedHash : Maybe LastAppliedBlock → Maybe HashHeader
lastAppliedHash nothing                   = nothing
lastAppliedHash (just ⟦ _ , _ , h , _ ⟧ℓ) = just h

prtlSeqChecks : Maybe LastAppliedBlock → BHeader → Type
prtlSeqChecks nothing                         bh = ⊤
prtlSeqChecks lab@(just ⟦ bℓ , sℓ , _ , _ ⟧ℓ) bh = sℓ < slot × bℓ + 1 ≡ blockNo × ph ≡ prevHeader
  where
    open BHeader; open BHBody (bh .body)
    ph = lastAppliedHash lab

certificationDelay : Slot -- 3·Lhdr + Lvote + Ldiff
certificationDelay = Lhdr + Lhdr + Lhdr + Lvote + Ldiff

certChecks : Maybe LastAppliedBlock → Bool → Slot → Type
certChecks _                               false _ = ⊤
certChecks nothing                         true  _ = ⊥
certChecks (just ⟦ _ , _  , _ , nothing ⟧ℓ) true  _ = ⊥
certChecks (just ⟦ _ , sℓ , _ , just _  ⟧ℓ) true  s = sℓ + certificationDelay ≤ s
\end{code}
\end{AgdaAlign}
\caption{Chain Head transition system functions}
\label{fig:ts-funs:chainhead}
\end{figure*}

The transition checks the following things
(via the functions \afun{chainChecks}, \afun{prtlSeqChecks} and \afun{certChecks}
from Figure~\ref{fig:ts-funs:chainhead}):
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
\item If the block header claims to certify an endorser block, then the last applied
  block announced one, and enough slots have elapsed since it did.
\end{itemize}

The last of these checks deserves comment, since it is the only one concerned with
endorser blocks. A block header carries a flag \afld{certifiedEB} recording whether the
block certifies the endorser block announced by its predecessor; the hash of that
endorser block is not repeated in the header, as it is already available from the
predecessor's \afld{announcedEB} field. Consequently the last applied block must
retain what it announced, which is why \afld{LastAppliedBlock} carries an
\afld{aeb} field.

A certificate may only be included at least $3 L_\text{hdr} + L_\text{vote} +
L_\text{diff}$ slots after the block that announced the endorser block~\cite{cip_164},
so that the endorser block has had time to reach the whole network. Because a header
that sets \afld{certifiedEB} obliges the corresponding body to carry a matching
certificate, this constraint can be checked on the header alone --- exactly the kind
of check that Property~\ref{prop:header-only-validation} exists to license.

Note that the \afld{size} component of \afld{announcedEB} is deliberately \emph{not}
checked here. As specified in \cite{cip_164}, an incorrect announced size invalidates
neither the header nor the block; it merely costs the block producer the votes of
honest nodes, which validate the endorser block itself.

\begin{figure*}[h]
\begin{code}[hide]
private variable
  nes forecast                : NewEpochState
  cs cs′                      : OCertCounters
  η₀ ηv ηc ηh η₀′ ηv′ ηc′ ηh′ : Nonce
  lab                         : Maybe LastAppliedBlock
  bh                          : BHeader

data _⊢_⇀⦇_,CHAINHEAD⦈_ where
\end{code}
\begin{code}
  Chain-Head :
    let 〖 bhb , _ 〗 = bh; open BHBody bhb
        e₁   = getEpoch nes
        e₂   = getEpoch forecast
        ne   = (e₁ ≠ e₂)
        pp   = getPParams forecast; open PParams
        nₚₕ  = prevHashToNonce (lastAppliedHash lab)
        pd   = extractPoolDistr (getPoolDelegatedStake forecast)
        lab′ = just ⟦ blockNo , slot , headerHash bh , announcedEB ⟧ℓ
    in
    ∙ prtlSeqChecks lab bh
    ∙ certChecks lab certifiedEB slot
    ∙ _ ⊢ nes ⇀⦇ slot ,TICKF⦈ forecast
    ∙ chainChecks MaxMajorPV (pp .maxHeaderSize , pp .maxBlockSize , pp .pv) bh
    ∙ ⟦ ηc , nₚₕ ⟧ᵗᵉ ⊢ ⟦ η₀ , ηh ⟧ᵗˢ ⇀⦇ ne ,TICKN⦈ ⟦ η₀′ , ηh′ ⟧ᵗˢ
    ∙ ⟦ pd , η₀′ ⟧ᵖᵉ ⊢ ⟦ cs , ηv , ηc ⟧ᵖˢ ⇀⦇ bh ,PRTCL⦈ ⟦ cs′ , ηv′ , ηc′ ⟧ᵖˢ
    ────────────────────────────────
    nes ⊢ ⟦ cs  , η₀  , ηv  , ηc  , ηh  , lab  ⟧ᶜˢ ⇀⦇ bh ,CHAINHEAD⦈
          ⟦ cs′ , η₀′ , ηv′ , ηc′ , ηh′ , lab′ ⟧ᶜˢ

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
\item If the block header certifies an endorser block but there is no last applied
  block, or the last applied block announced no endorser block, there is a
  \emph{NoEndorserBlockToCertify} failure.
\item If the block header certifies an endorser block but fewer than
  $3 L_\text{hdr} + L_\text{vote} + L_\text{diff}$ slots have elapsed since the last
  applied block, there is a \emph{CertifiedTooEarly} failure.
\end{enumerate}
