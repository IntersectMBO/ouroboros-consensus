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
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : Spec.VDF.Discriminant crypto nonces) → Set )
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
-- uses security parameter I and slot number to compute interval number
getInterval : ℕ → Slot → ℕ
getInterval i s = 0


-- TODO define/find
nextSlot : Slot → Slot
nextSlot s = s

-- TODO define
-- TODO this is ℤ in CIP, why is it not Nonce?
combinEIN : Epoch → Nonce → Discriminant
combinEIN e n = n

record Parametrized : Type where
  constructor ⟦_,_⟧ᵖ
  field
    securityParameter : ℕ -- The security parameter, defining the size (in bits) of the VDF discriminant.
    I : ℕ -- The per-interval VDF iteration count, computed from TΦ and evenly distributed across 82 computation intervals.

record Initialized : Type where
  constructor ⟦_,_,_,_⟧ⁱ
  field
    parametrized : Parametrized
    discriminant : Discriminant
    epochIdₑ : Epoch -- Numerical identifier for epoch e - the epoch in which initialization happened??
    pre-ηₑ : Nonce 
    
  Gr = setupVDFGroup (parametrized .Parametrized.securityParameter) (combinEIN epochIdₑ pre-ηₑ)  -- VDF group used for exponentiation
  VDFG = setupVDF Gr

-- module _  where 

-- TODO this is temporary - G needs to come from initialization/parametrization
-- remove this, and replace all G's with some function getting it from the state
VDFg = setupVDF G
open import Spec.OutsVec crypto nonces G

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
  constructor ⟦_,_⟧ᵃᵖ
  field
    initializedState : Initialized 
    -- currentSlot : Slot 
    attestedOutputs : AttestedOutputsM 

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

UpdateNonceCommand = Maybe (G × G )

UpdateNonceSignal = Slot × UpdateNonceCommand

data isPhase : Type where 
  isWithinInitializationGracePhase : isPhase 
  isWithinComputationPhase : isPhase
  isWithinCurrentInterval : isPhase 
  isWithinNextInterval : isPhase
  isWithinCatchUpPhase : isPhase 
  isClosable : isPhase
  isUngracefullyClosable : isPhase

-- TODO define this correctly
getPhase : Slot → Initialized → isPhase
getPhase s inl = isWithinInitializationGracePhase

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
    η-epoch : Nonce
    params : Parametrized


\end{code}
\end{AgdaSuppressSpace}
\emph{Update Nonce states}
\begin{AgdaSuppressSpace}
\begin{code}
data UpdateNonceState  : Type where
  sPrePhalanx : UpdateNonceState
  sParametrized : UpdateNonceState
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
  _⊢_⇀⦇_,UPDNONESTREAM⦈_ : UpdateNonceEnv → UpdateNonceState  → UpdateNonceSignal  → UpdateNonceState  → Type
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
  par : Parametrized
  a b c : G
  ds : Nonce
  inl : Initialized
  φᵢ φ : G × G
  xᵢ yᵢ πᵢ x y π : G
  I : G
  Isp : ℕ
  pins pins' : AttestedOutputsM
  m : Bool
  mx : Maybe G
  newState : UpdateNonceState
  sig : UpdateNonceCommand
  ηstate ηstate' : UpdateNonceState 
  pre-η-candidate pre-η-candidate' : Nonce
  next-η next-η' : Nonce
  pre-ηₑ η-epoch : Nonce
  ηₑ : Nonce
  cl : Closed
  aggOut : G × G × G
  agc : AwaitingGracefulClosure

data _⊢_⇀⦇_,UPDNONESTREAM⦈_ where

  -- TODO parametrized state is entered at the hard fork (transition not specified here)
  -- the state machine never again enters the parametrized state 
  -- updated parameters get stored directly in the Initialized state

  -- initialize-pr : 
  --   let 
  --     ds = combinEIN (epoch sₙ) η-epoch -- Δchallenge←Hash(bin(epochIde)|pre-ηe) 
  --   in
  --   ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sInitialized ⟦ par , ds , (epoch sₙ) , η-epoch ⟧ⁱ ) ⇀⦇ (s , nothing) ,UPDNONESTREAM⦈ newState
  --   ────────────────────────────────
  --   ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ sParametrized ⇀⦇ (s , nothing )  ,UPDNONESTREAM⦈ newState 

  initialize-cl : 
      -- TODO why do we even need ds in the state? it's just computed from (epoch sₙ) and η-epoch
      -- note here that current phalanx parameters par are stored in sInitialized, they will be used for the rest of the cycle 
      -- this is where the parameter update (from ps to par, in this rule) takes effect in the Phalanx nonce update
    let 
      ds = combinEIN (epoch sₙ) η-epoch -- Δchallenge←Hash(bin(epochIde)|pre-ηe)
    in
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sInitialized ⟦ par , ds , (epoch sₙ) , η-epoch ⟧ⁱ ) ⇀⦇ ( s , nothing) ,UPDNONESTREAM⦈ newState 
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sClosed ⟦ ⟦ ps , ds , (epoch sₙ) , pre-ηₑ ⟧ⁱ , pins , ( a , b , c ) , ηₑ ⟧ᶜᵈ ) ⇀⦇ (s , nothing )  ,UPDNONESTREAM⦈ newState 

  tick-i : -- (1) <- this number is used to enumerate "tick"-type rules 
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ (sInitialized inl ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-igp : -- (2)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState      
    ∙ isWithinInitializationGracePhase ≡ getPhase s inl
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-ic : -- (3) 
    let 
      pn = inl .Initialized.pre-ηₑ
    in 
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , allN ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState   
    ∙ isWithinComputationPhase ≡ getPhase s inl
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch ,  par ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState

  -- works for missing and non-missing
  provideOut :
    let lam = inl .Initialized.parametrized .Parametrized.securityParameter
        Isp = inl .Initialized.parametrized .Parametrized.I
        i = getInterval Isp s 
        mxᵢ = mkInput lam (epoch sₙ) pre-ηₑ i  --  Hash(b“challenge"||bin(e)||pre-ηe||bin(i)) fix does this need to take I or (atIndex i pins)?
        pins' = unsafeUpdateAt82 i xᵢ (proj₁ φᵢ) pins
    in
    ∙ mxᵢ ≡ just xᵢ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided m ⟦ inl , pins' ⟧ᵃᵖ ) ⇀⦇ ( s , nothing ) ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting m ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , just φᵢ )  ,UPDNONESTREAM⦈ newState 

  tick-ci-p : -- (4)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState  
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-ni-p : -- (5)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState  
    ∙ getPhase s inl ≡ isWithinNextInterval
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-cup-p : --(6)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCatchUpPhase
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState

  tick-ci-a : -- (7)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-cu-a : -- (8)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCatchUpPhase
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-ic-a : -- (9)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState

  tick-cu-mp : -- (10)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-ni-mp : -- (11)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isWithinNextInterval
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-ic-mp : -- (12)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-uc-mp : --(13)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState 
    ∙ getPhase s inl ≡ isUngracefullyClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState

  tick-ci-m : -- (14)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≡ isWithinCurrentInterval
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-c-m : -- (15)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState
    ∙ allJust (proj₁ pins) ≡ true
    ∙ getPhase s inl ≡ isClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState
    
  tick-uc-m : -- (16)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≡ isUngracefullyClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState 

  close-rule : 
    let 
      ηₑ = computeNonce y -- η e = Hash ( 256 ) ( y ) — Apply the SHA-256 hash function to y 
      pn = inl .Initialized.pre-ηₑ
      lam = inl .Initialized.parametrized .Parametrized.securityParameter
      Isp = inl .Initialized.parametrized .Parametrized.I
      i = getInterval Isp s 
      aggOuts = foldr (compAgg (VDFg .VDF.aggUpdate) lam) (just (VDFg .VDF.init lam pn)) (proj₁ pins) 
    in 
    ∙ mx ≡ just x  
    ∙ aggOuts ≡ just (x , y , π)
    -- TODO probably need another rule here to allow additional ticks after sClose is reached and before initializing!
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sClosed  ⟦ inl , pins , (x , y , π) , ηₑ ⟧ᶜᵈ ) ⇀⦇ ( s , nothing ) ,UPDNONESTREAM⦈ newState
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , (x , y , π) ⟧ᵃᵍᶜ ) ⇀⦇ (s , just φ )  ,UPDNONESTREAM⦈ newState

  tick-gcl : -- (17)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≡ isUngracefullyClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState 

  tick-ngcl : -- (18)
    ∙ s > sₙ
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (nextSlot s , nothing)  ,UPDNONESTREAM⦈ newState
    ∙ getPhase s inl ≢ isUngracefullyClosable
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( sAwaitingGracefulClosure ⟦ inl , pins , aggOut ⟧ᵃᵍᶜ ) ⇀⦇ (s , nothing)  ,UPDNONESTREAM⦈ newState

\end{code}
\caption{Update Nonce transition system rules}
\label{fig:ts-rules:updatenonce}
\end{figure*}

\begin{figure*}[h]
\begin{code}
UpdateNonce3EpochState = Nonce × UpdateNonceState × Nonce

data
  _⊢_⇀⦇_,UPDN⦈_ : UpdateNonceEnv → UpdateNonce3EpochState  → UpdateNonceSignal  → UpdateNonce3EpochState  → Type
\end{code}
\end{AgdaAlign}
\caption{Update Nonce transition system types}
\label{fig:ts-types:updnonce}
\end{figure*}

\begin{figure*}[h]
\begin{code}[hide]
data _⊢_⇀⦇_,UPDN⦈_ where 

  -- TODO should we include special cases here for the epochs right after the hard fork?
  -- TODO prove relations are computational
  -- TODO check no cases are excluded
  -- TODO remove unnecessary constraints
  -- TODO we are assuming there is a block between 6k/f and 9k/f - is this valid?

  -- applies when 6k/f slot is contained between this slot and last slot
  -- swap the pre-nonce candidate for the current VRF output (i.e. η-epoch nonce)
  -- update nonce state with sig
  -- keep candidate nonce 
  -- TODO check conditions : not initialization
  switch-pre-cand : 
    ∙ contains6kof sₗ sₙ ≡ true 
    ∙ ηstate ≢ sParametrized 
    ∙ ηstate ≢ sClosed cl 
    ∙ (⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ηstate ⇀⦇ ( s , nothing )  ,UPDNONESTREAM⦈ ηstate' ) 
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , next-η ) ⇀⦇ (s , sig )  ,UPDN⦈  ( η-epoch , ηstate' , next-η )

  -- keep pre-nonce candidate 
  -- initialize nonce state with pre-η-candidate
  -- keep next-η
  -- TODO check conditions : initializing (from closed or parametrized)
  init : 
    ∙ ( ηstate ≡ sClosed cl ) ⊎ ( ηstate ≡ sParametrized )
    ∙ (⟦ sₗ , sₙ , pre-η-candidate , par ⟧ᵘᵉ ⊢ ηstate ⇀⦇ ( s , nothing )  ,UPDNONESTREAM⦈ ηstate' ) 
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , next-η ) ⇀⦇ (s , nothing )  ,UPDN⦈  ( pre-η-candidate , ηstate' , next-η )

  -- applies when 9k/f slot is contained between this slot and last slot, and ηstate is getting closed
  -- keep pre-η-candidate
  -- update next-η from nonce computed in closed state
  -- TODO check the conditions : crossing 9k/f and not closing 
  close-switch-cand : 
    let 
      mηₑ = getInlNonce ηstate' 
    in
    ∙ contains9kof sₗ sₙ ≡ true
    ∙ ηstate ≡ sAwaitingGracefulClosure agc 
    ∙ (⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ηstate ⇀⦇ ( s , just φ )  ,UPDNONESTREAM⦈ ηstate' ) 
    ∙ (mηₑ ≡ just next-η) 
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , next-η' ) ⇀⦇ (s , just φ )  ,UPDN⦈  ( pre-η-candidate , ηstate' , next-η )

  -- if the signal is neither nothing nor iClose, update nonce state with signal
  -- keep pre-η-candidate and next-η
  -- TODO check the conditions : not closing or initializing and not crossing 6k/f or 9k/f
  update-noswitch : 
    ∙ ¬ ((ηstate ≡ sAwaitingGracefulClosure agc) × (sig ≡ just φ))
    ∙ ¬ ((ηstate ≡ sInitialized inl) × (sig ≡ just φ))
    ∙ contains6kof sₗ sₙ ≢ true 
    ∙ contains9kof sₗ sₙ ≢ true 
    ∙ ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ηstate ⇀⦇ (s , sig)  ,UPDNONESTREAM⦈  ηstate'
    ────────────────────────────────
    ⟦ sₗ , sₙ , η-epoch , par ⟧ᵘᵉ ⊢ ( pre-η-candidate , ηstate , next-η ) ⇀⦇ (s , sig)  ,UPDN⦈  ( pre-η-candidate , ηstate' , next-η )

\end{code}
\caption{Update Nonce transition system rules}
\label{fig:ts-rules:updatenonce}
\end{figure*}
