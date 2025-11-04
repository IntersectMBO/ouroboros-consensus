\subsection{$\mathsf{UPDN}$ Transition}
\label{sec:update-nonces-trans}

The Update Nonce Transition ($\mathsf{UPDN}$) updates the nonces until the randomness gets fixed.
The environment is shown in Figure~\ref{fig:ts-types:updnonce} and consists of
the block nonce \afld{η}.
The state is shown in Figure~\ref{fig:ts-types:updnonce} and consists of
the candidate nonce \afld{ηc} and the evolving nonce \afld{ηv}. 

\begin{code}[hide]
-- {-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Spec.BaseTypes using (Nonces)
open import Ledger.Types.Epoch using (EpochStructure )
import Spec.VDF
open import Ledger.Prelude
open import Relation.Binary.PropositionalEquality using (inspect; [_])

module Spec.UpdateNonce
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  -- TODO instantiate thes with correct VDF setup!
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : Nonce) → Set )
  (setupVDF : (G : Set) → (Spec.VDF.VDF crypto nonces {G}))
  -- TODO temporary! the group G should always be obtained from the Initialized state!
  (G : Set) 
  (_*ᵍ_ : G × G → G) 
  (idᵍ : G) 
  (defaultNonce : Nonce)
  where

open module VDF' = Spec.VDF crypto nonces

\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Update Nonce states}
\begin{AgdaSuppressSpace}
\begin{code}

-- TODO define 
getInterval : Slot → ℕ
getInterval s = 0


-- TODO define/find
nextSlot : Slot → Slot
nextSlot s = s

record Parametrized : Type where
  constructor ⟦_,_⟧ᵖ
  field
    securityParameter : ℕ -- The security parameter, defining the size (in bits) of the VDF discriminant.
    I : ℕ -- The per-interval VDF iteration count, computed from TΦ and evenly distributed across 82 computation intervals.

record parametrize : Type where
  constructor ⟦_,_⟧ᵖⁱ
  field
    lambda : ℕ
    TΦ : ℕ

-- TODO define
-- TODO this is ℤ in CIP, why is it not Nonce?
combinEIN : Epoch → Nonce → Nonce
combinEIN e n = n

record Initialized : Type where
  constructor ⟦_,_,_,_⟧ⁱ
  field
    parametrized : Parametrized
    discriminant : Nonce -- TODO check type
    epochIdₑ : Epoch -- Numerical identifier for epoch e - the epoch in which initialization happened??
    pre-ηₑ : Nonce 
    
  Gr = setupVDFGroup (parametrized .Parametrized.securityParameter) (combinEIN epochIdₑ pre-ηₑ)  -- VDF group used for exponentiation
  VDFG = setupVDF Gr

-- module _  where 

-- TODO this is temporary - G needs to come from initialization/parametrization
-- remove this, and replace all G's with some function getting it from the state
VDFg = setupVDF G
open import Spec.OutsVec crypto nonces G

record initialize : Type where
  constructor ⟦_⟧ⁱⁱ
  field
    -- commented out fields in this record and others represent ones that are in the state machine diagram in the CIP, but are actually redundant
    -- parametrizedState : Parametrized 
    -- epochIdₑ : ℕ 
    pre-ηₑ : Nonce

record AwaitingComputationPhase  : Type where
  constructor ⟦_⟧ᵃᶜᵖ
  field
    initializedState : Initialized 
    -- currentSlot : Slot this info is always coming from the current block

data AwaitingOrProvided : Set where
  awaiting  : AwaitingOrProvided 
  provided : AwaitingOrProvided


-- represents states : AwaitingAttestedOutput AttestedOutputProvided AwaitingMissingAttestedOutput AttestedMissingOutputProvided 
-- TODO this needs the aggregated output i think?? aggregated output needs to be updated with every new output?
record AwaitingProvided  (awaitingOrProvided : AwaitingOrProvided) (missing : Bool) : Type where
  constructor ⟦_,_,_⟧ᵃᵖ
  field
    initializedState : Initialized 
    -- currentSlot : Slot 
    attestedOutputs : AttestedOutputsM 
    aggOut : G × G × G

record provideAttestedOutput  : Type where
  constructor ⟦_⟧ᵖᵃᵒ
  field
    -- awaitingAttestedOutputState : AwaitingProvided  awaiting  
    φᵢ : G × G

record provideMissingAttestedOutput  : Type where
  constructor ⟦_⟧ᵖᵐᵒ
  field
    -- awaitingMissingAttestedOutputState : AwaitingProvided  awaiting 
    φᵢ : G × G

record Closed  : Type where
  constructor ⟦_,_,_,_⟧ᶜᵈ
  field
    initialized : Initialized 
    attestedOutputs : AttestedOutputsM --  [(y,π)]82
    aggregatedOutput : G × G × G --  (x,y,π)
    ηₑ : Nonce

record UngracefullyClosed  : Type where
  constructor ⟦_,_,_⟧ᵘᶜ
  field
    initialized : Initialized 
    attestedOutputs : AttestedOutputsM --  [(y,π)]82
    pre-ηₑ : Nonce

record AwaitingGracefulClosure  : Type where
  constructor ⟦_,_,_⟧ᵃᵍᶜ
  field
    initialized : Initialized 
    -- currentSlot : Slot 
    attestedOutputs : AttestedOutputsM --  [(y,π)]82
    aggOut : G × G × G

record close  : Type where
  constructor ⟦_⟧ᶜ
  field
    -- awaitingGracefulClosureState : AwaitingGracefulClosure  
    φ : G × G

data UpdateNonceCommand  : Type where
  iNothing : UpdateNonceCommand
  iParametrize : parametrize → UpdateNonceCommand
  iInitialize : initialize → UpdateNonceCommand
  iProvideAttestedOutput : (a : AwaitingOrProvided) → provideAttestedOutput  → UpdateNonceCommand
  iProvideMissingAttestedOutput : provideMissingAttestedOutput  → UpdateNonceCommand 
  iClose : close  → UpdateNonceCommand 
  iTick : UpdateNonceCommand

UpdateNonceSignal = Slot × UpdateNonceCommand

data isPhase : Type where 
  isIGP : isPhase -- isWithinInitializationGracePhase
  isCP : isPhase -- isWithinComputationPhase
  isWithinCurrentInterval : isPhase 
  isWithinNextInterval : isPhase
  isWithinCatchUpPhase : isPhase 
  isClosable : isPhase
  isUngracefullyClosable : isPhase

-- TODO define this correctly
getPhase : Slot → Initialized → isPhase
getPhase s inl = isIGP

-- TODO define
mkInput : ℕ → Epoch → Nonce → ℕ → Maybe G
mkInput lam e pre-η g = nothing

-- TODO define
computeNonce : G → Nonce
computeNonce y = defaultNonce

record UpdateNonceEnv  : Type where
  constructor ⟦_,_,_,_⟧ᵘᵉ
  field
    sₗ : Slot 
    slot : Slot 
    η : Nonce 
    η-epoch : Nonce


\end{code}
\end{AgdaSuppressSpace}
\emph{Update Nonce states}
\begin{AgdaSuppressSpace}
\begin{code}
data UpdateNonceState  : Type where
  sPrePhalanx : UpdateNonceState
  sParametrized : Parametrized → UpdateNonceState
  sInitialized : Initialized  → UpdateNonceState
  sAwaitingComputationPhase : AwaitingComputationPhase → UpdateNonceState
  sAwaitingProvided : (a : AwaitingOrProvided) → (missing : Bool) → AwaitingProvided a missing → UpdateNonceState
  sClosed : Closed  → UpdateNonceState
  sAwaitingGracefulClosure : AwaitingGracefulClosure  → UpdateNonceState
  sUngracefullyClosed : UngracefullyClosed  → UpdateNonceState

-- TODO define
getInlNonce : UpdateNonceState → Maybe Nonce
getInlNonce inl = nothing

-- TODO define 
contains6kof : Slot → Slot → Bool
contains6kof sₗ sₙ = true

-- TODO define 
contains9kof : Slot → Slot → Bool
contains9kof sₗ sₙ = true

\end{code}
\end{AgdaSuppressSpace}
\emph{Update Nonce transitions}
\begin{code}[hide]

data
\end{code}
\begin{code}
  _⊢_⇀⦇_,UPDNONESTREAM⦈_ : (Slot × Slot) → UpdateNonceState  → UpdateNonceSignal  → UpdateNonceState  → Type
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
  -- sₗ is the slot from before the block was applied (when last block was applied)
  -- sₙ is the slot of the current block being applied 
  -- s keeps track of processing intermediate slots when tick rules are being applied
  s sₗ sₙ slot : Slot 
  spr : ℕ -- VDF discriminant size
  cnt : ℕ
  TΦ lam : ℕ
  pz ps : Parametrized
  p : parametrize
  a b c : G
  ds : Nonce
  -- η ηv ηc pre-ηc pre-ηₑ ηₑ candidate-η : Nonce 
  inl : Initialized
  iInl : initialize
  φᵢ φ : G × G
  xᵢ yᵢ πᵢ x y π : G
  I : G
  pins pins' : AttestedOutputsM
  m : Bool
  mx : Maybe G
  newState : UpdateNonceState
  sig : UpdateNonceCommand
  ηstate ηstate' : UpdateNonceState 
  pre-η-candidate pre-η-candidate' : Nonce
  candidate-η candidate-η' : Nonce
  pre-ηₑ η-head η-epoch : Nonce
  ηₑ : Nonce
  cl : close
  aggOut : G × G × G

  -- 3 k/f 
  -- intervals = 12 (hours) * numSecondsInHour
  -- slotToIntervalIndex s = s / 120
  -- convert RandomnessStabilisationWindowPlusOne to intervals also
  -- 9 k/f - beginning of computation phase 
  

-- TODO ⟦ η ⟧ᵘᵉ is not used anywhere - do we want to use the block nonce for extra randomness somewhere?

data _⊢_⇀⦇_,UPDNONESTREAM⦈_ where

  parametrize-r : 
    let 
      I = (VDFg .VDF.iterationsFromDuration TΦ ) / 82
    in
    ∙ ( sₗ , sₙ ) ⊢ (sParametrized ⟦ lam , TΦ ⟧ᵖ ) ⇀⦇ (s , sig) ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ sPrePhalanx ⇀⦇ (s , iParametrize ⟦ lam , TΦ ⟧ᵖⁱ)  ,UPDNONESTREAM⦈ newState

  initialize-pr : 
    let 
      ds = combinEIN (epoch sₙ) pre-ηₑ -- Δchallenge←Hash(bin(epochIde)|pre-ηe)
    in
    ∙ ( sₗ , sₙ ) ⊢ ( sInitialized ⟦ ps , ds , (epoch sₙ) , pre-ηₑ ⟧ⁱ ) ⇀⦇ (s , sig) ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ (sParametrized ps ) ⇀⦇ (s , iInitialize ⟦ pre-ηₑ ⟧ⁱⁱ)  ,UPDNONESTREAM⦈ newState 

  initialize-cl : 
    let 
      ds = combinEIN (epoch sₙ) pre-ηₑ -- Δchallenge←Hash(bin(epochIde)|pre-ηe)
    in
    ∙ ( sₗ , sₙ ) ⊢ ( sInitialized ⟦ ps , ds , (epoch sₙ) , pre-ηₑ ⟧ⁱ ) ⇀⦇ ( s , sig) ,UPDNONESTREAM⦈ newState 
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sClosed ⟦ ⟦ ps , ds , (epoch sₙ) , pre-ηₑ ⟧ⁱ , pins , ( a , b , c ) , ηₑ ⟧ᶜᵈ ) ⇀⦇ (s , iInitialize ⟦ pre-ηₑ ⟧ⁱⁱ)  ,UPDNONESTREAM⦈ newState 

  tick-i : -- (1) <- this number is used to enumerate "tick"-type rules 
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ (sInitialized inl ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-igp : -- (2) TODO getPhase depends only on slot number (and k, which is - or does it come from ledger params? make a note about how much changes if this changes)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState      
    ∙ isIGP ≡ getPhase s inl
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-ic : -- (3) 
    let 
      lam = inl .Initialized.parametrized .Parametrized.securityParameter
      pn = inl .Initialized.pre-ηₑ
    in 
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting false ⟦ inl , allN , ((VDFg .VDF.init) lam pn) ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState   
    ∙ isCP ≡ getPhase s inl
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState

  -- works for missing and non-missing
  provideOut :
    let i = getInterval s 
        I = inl .Initialized.parametrized .Parametrized.I
        lam = inl .Initialized.parametrized .Parametrized.securityParameter
        mxᵢ = mkInput lam (epoch sₙ) pre-ηₑ i  --  Hash(b“challenge"||bin(e)||pre-ηe||bin(i)) fix does this need to take I or (atIndex i pins)?
        pins' = unsafeUpdateAt82 i xᵢ (proj₁ φᵢ) pins
        aggOut' = (VDFg .VDF.aggUpdate) lam (xᵢ , (proj₁ φᵢ)) aggOut
    in
    ∙ (VDFg .VDF.verify) xᵢ (proj₁ φᵢ) I (proj₂ φᵢ) ≡ true
    ∙ mxᵢ ≡ just xᵢ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided m ⟦ inl , pins' , aggOut' ⟧ᵃᵖ ) ⇀⦇ ( s , sig ) ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting m ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , (iProvideAttestedOutput awaiting ⟦ φᵢ ⟧ᵖᵃᵒ ))  ,UPDNONESTREAM⦈ newState 

  tick-ci-p : -- (4)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState  
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-ni-p : -- (5)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState  
    ∙ getPhase s inl ≡ isWithinNextInterval
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-cup-p : --(6)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCatchUpPhase
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState

  tick-ci-a : -- (7)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-cu-a : -- (8)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCatchUpPhase
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-ic-a : -- (9)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState

  tick-cu-mp : -- (10)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-ni-mp : -- (11)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinNextInterval
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-ic-mp : -- (12)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-uc-mp : --(13)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isUngracefullyClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided provided true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState

  tick-ci-m : -- (14)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-c-m : -- (15)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState
    ∙ allJust (proj₁ pins) ≡ true
    ∙ getPhase s inl ≡ isClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState
    
  tick-uc-m : -- (16)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≡ isUngracefullyClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins , aggOut ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState 

  close-rule : 
    let 
      y = proj₁ φᵢ
      ηₑ = computeNonce y -- η e = Hash ( 256 ) ( y ) — Apply the SHA-256 hash function to y 
      lam = inl .Initialized.parametrized .Parametrized.securityParameter
      i = getInterval s 
      I = inl .Initialized.parametrized .Parametrized.I
    in 
    ∙ mx ≡ just x
    ∙ (VDFg .VDF.aggVerify) (x , y , lam) I π ≡ true 
    ∙ ( sₗ , sₙ ) ⊢ ( sClosed  ⟦ inl , pins , (x , y , π) , ηₑ ⟧ᶜᵈ ) ⇀⦇ ( s , sig ) ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , (x , y , π) ⟧ᵃᵍᶜ ) ⇀⦇ (s , iClose  ⟦ φ ⟧ᶜ )  ,UPDNONESTREAM⦈ newState

  tick-gcl : -- (17)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≡ isUngracefullyClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState 

  tick-ngcl : -- (18)
    ∙ s > sₙ
    ∙ ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , iTick)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≢ isUngracefullyClosable
    ────────────────────────────────
    ( sₗ , sₙ ) ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (s , iTick)  ,UPDNONESTREAM⦈ newState

\end{code}
\caption{Update Nonce transition system rules}
\label{fig:ts-rules:updatenonce}
\end{figure*}

\begin{figure*}[h]
\begin{code}
UpdateNonce3EpochState = Nonce × UpdateNonceState × Nonce

data
  _⊢_⇀⦇_,UPDN⦈_ : UpdateNonceEnv → UpdateNonce3EpochState  → Slot × UpdateNonceCommand  → UpdateNonce3EpochState  → Type
\end{code}
\end{AgdaAlign}
\caption{Update Nonce transition system types}
\label{fig:ts-types:updnonce}
\end{figure*}

\begin{figure*}[h]
\begin{code}[hide]
data _⊢_⇀⦇_,UPDN⦈_ where 

  -- TODO should we include special cases here for the epochs right after the hard fork?

  -- applies when 6k/f slot is contained between this slot and last slot
  -- swap the pre-nonce candidate for the current VRF output (i.e. η-epoch nonce)
  -- update nonce state with sig
  -- keep candidate nonce 
  switch-pre-cand : 
    ∙ contains6kof sₗ sₙ ≡ true
    ∙ (( sₗ , sₙ ) ⊢ ηstate ⇀⦇ ( s , iInitialize ⟦ η-epoch ⟧ⁱⁱ )  ,UPDNONESTREAM⦈ ηstate' ) -- checked to be 9k/f by UPDNONESTREAM transition
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-head , η-epoch ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , candidate-η ) ⇀⦇ (s , sig )  ,UPDN⦈  ( η-epoch , ηstate' , candidate-η )

  -- keep pre-nonce candidate 
  -- initialize nonce state with pre-η-candidate
  -- keep candidate-η
  init : 
    ∙ (( sₗ , sₙ ) ⊢ ηstate ⇀⦇ ( s , iInitialize ⟦ pre-η-candidate ⟧ⁱⁱ )  ,UPDNONESTREAM⦈ ηstate' ) 
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-head , η-epoch ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , candidate-η ) ⇀⦇ (s , iInitialize ⟦ η-epoch ⟧ⁱⁱ )  ,UPDN⦈  ( pre-η-candidate , ηstate' , candidate-η )

  -- applies when 9k/f slot is contained between this slot and last slot
  -- update nonce state must be iClose 
  -- keep pre-η-candidate
  -- update candidate-η from nonce computed in closed state
  close-switch-cand : 
    let 
      mηₑ = getInlNonce ηstate' 
    in
    ∙ contains9kof sₗ sₙ ≡ true
    ∙ (( sₗ , sₙ ) ⊢ ηstate ⇀⦇ ( s , iClose  ⟦ φ ⟧ᶜ )  ,UPDNONESTREAM⦈ ηstate' ) -- checked to be 9k/f by UPDNONESTREAM transition
    ∙ (mηₑ ≡ just candidate-η)
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-head , η-epoch ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , candidate-η' ) ⇀⦇ (s , iClose  ⟦ φ ⟧ᶜ )  ,UPDN⦈  ( pre-η-candidate , ηstate' , candidate-η )

  -- if the signal is neither iInitialize nor iClose, update nonce state with signal
  -- keep pre-η-candidate and candidate-η
  update-noswitch : 
    ∙ sig ≢ iClose cl 
    ∙ sig ≢ iInitialize iInl
    ∙ ( sₗ , sₙ ) ⊢ ηstate ⇀⦇ (s , sig)  ,UPDNONESTREAM⦈  ηstate'
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-head , η-epoch ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , candidate-η ) ⇀⦇ (s , sig)  ,UPDN⦈  ( pre-η-candidate , ηstate' , candidate-η )

\end{code}
\caption{Update Nonce transition system rules}
\label{fig:ts-rules:updatenonce}
\end{figure*}
