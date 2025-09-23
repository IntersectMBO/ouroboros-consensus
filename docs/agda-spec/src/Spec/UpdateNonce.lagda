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
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)
import Spec.VDF
open import Ledger.Prelude
open import Relation.Binary.PropositionalEquality using (inspect; [_])

module Spec.UpdateNonce
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  -- TODO these are introduced at the hard fork transition? no fix it
  (sP : ℕ) -- The security parameter, defining the size (in bits) of the VDF discriminant.
  (Ip : ℕ) -- The per-interval VDF iteration count, computed from TΦ and evenly distributed across 82 computation intervals.
  -- TODO instantiate thes with correct VDF setup!
  -- TODO change List Bool type to something?
  (setupVDFGroup : (securityParam : ℕ) → ∀ (Δ-challenge : List Bool) → Set )
  (setupVDF : (G : Set) → (Spec.VDF.VDF crypto nonces {G}))
  -- TODO implement
  (combinEIN : ℕ → Nonce → List Bool)
  where

open module VDF' = Spec.VDF crypto nonces


\end{code}

\begin{figure*}[h]
\begin{AgdaAlign}
\emph{Update Nonce states}
\begin{AgdaSuppressSpace}
\begin{code}

getInterval : Slot → ℕ
getInterval s = 0

record Parametrized : Type where
  constructor ⟦_,_⟧ᵖ
  field
    securityParameter : ℕ -- The security parameter, defining the size (in bits) of the VDF discriminant.
    I : ℕ -- The per-interval VDF iteration count, computed from TΦ and evenly distributed across 82 computation intervals.

record Initialized : Type where
  constructor ⟦_,_,_,_⟧ⁱ
  field
    parametrized : Parametrized
    discriminant : ℤ -- Epoch-specific VDF discriminant
    epochIdₑ : ℕ -- Numerical identifier for epoch e TODO pprolly dont need this
    pre-ηₑ : Nonce
    
  G = setupVDFGroup (parametrized .Parametrized.securityParameter) (combinEIN epochIdₑ pre-ηₑ)  -- VDF group used for exponentiation
  VDFG = setupVDF G 

-- TODO temporary! the group should always be obtained from the Initialized state!
-- remove module here, and replace all G's with some function getting it from the state
module _ {G : Set} {_*ᵍ_ : G × G → G} {idᵍ : G} where 

  -- TODO also temporary!
  VDFg = setupVDF G
  open import Spec.OutsVec crypto nonces G

  record initialize : Type where
    constructor ⟦_⟧ⁱⁱ
    field
      -- parametrizedState : Parametrized -- TODO always has to be the same as the existing parametrized state (or else need to re-parametrize!)
      epochIdₑ : ℕ -- Numerical identifier for epoch e TODO pprolly dont need this

  record AwaitingComputationPhase  : Type where
    constructor ⟦_⟧ᵃᶜᵖ
    field
      initializedState : Initialized 
      -- currentSlot : Slot this info is always coming from the current block

  record tickInitState  : Type where
    constructor ⟦_,_⟧ᵗⁱˢ
    field
      initializedStateParam : Initialized 
      -- currentSlot : Maybe Slot 

  data AwaitingOrProvided : Set where
    awaiting  : AwaitingOrProvided 
    provided : AwaitingOrProvided


  -- represents states : AwaitingAttestedOutput AttestedOutputProvided AwaitingMissingAttestedOutput AttestedMissingOutputProvided 
  record AwaitingProvided  (awaitingOrProvided : AwaitingOrProvided) (missing : Bool) : Type where
    constructor ⟦_,_⟧ᵃᵖ
    field
      initializedState : Initialized 
      -- currentSlot : Slot 
      attestedOutputs : AttestedOutputsM 

  record provideAttestedOutput  : Type where
    constructor ⟦_⟧ᵖᵃᵒ
    field
      -- awaitingAttestedOutputState : AwaitingProvided  awaiting  TODO this is not necessary 
      φᵢ : G × G

  -- record tickAwaitingAttested  (awaitingOrProvided : AwaitingOrProvided) : Type where
  --   constructor ⟦_,_⟧ᵗᵃᵃ
  --   field
  --     awaitingProvidedState : AwaitingProvided  awaitingOrProvided
  --     φᵢₘ : Maybe (G × G)

  -- record tickMissingProvided  : Type where
  --   constructor ⟦_⟧ᵗᵐᵖ
  --   field
  --     missingAttestedOutputProvidedState : AwaitingProvided  provided

  record provideMissingAttestedOutput  : Type where
    constructor ⟦_⟧ᵖᵐᵒ
    field
      -- awaitingMissingAttestedOutputState : AwaitingProvided  awaiting  probably dont need it
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
    constructor ⟦_,_⟧ᵃᵍᶜ
    field
      initialized : Initialized 
      -- currentSlot : Slot 
      attestedOutputs : AttestedOutputsM --  [(y,π)]82

  record close  : Type where
    constructor ⟦_⟧ᶜ
    field
      -- awaitingGracefulClosureState : AwaitingGracefulClosure  -- dont need this probably
      φᵢ : G × G

  -- record tickAwaitingGracefulClosure  (awaitingOrProvided : AwaitingOrProvided) : Type where
  --   constructor ⟦_⟧ᵗᵃᵍᶜ
  --   field
  --     awaitingGracefulClosureState : AwaitingProvided  awaiting

  data UpdateNonceCommand  : Type where
    iInitialize : initialize → UpdateNonceCommand
    -- iTickInitState : tickInitState  → UpdateNonceCommand
    iProvideAttestedOutput : (a : AwaitingOrProvided) → provideAttestedOutput  → UpdateNonceCommand
    -- iTickAwaitingAttested : (a : AwaitingOrProvided) → tickAwaitingAttested  a → UpdateNonceCommand
    -- iTickMissingProvided : tickMissingProvided  → UpdateNonceCommand 
    iProvideMissingAttestedOutput : provideMissingAttestedOutput  → UpdateNonceCommand 
    iClose : close  → UpdateNonceCommand 
    -- iTickAwaitingGracefulClosure : (a : AwaitingOrProvided) → tickAwaitingGracefulClosure  a → UpdateNonceCommand 
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
  mkInput : Nonce → ℕ → G
  mkInput pre-ηₑ i = idᵍ


  record UpdateNonceEnv : Type where
    constructor ⟦_⟧ᵘᵉ
    field
      η : Nonce -- new nonce
  \end{code}
  \end{AgdaSuppressSpace}
  \emph{Update Nonce states}
  \begin{AgdaSuppressSpace}
  \begin{code}
  data UpdateNonceState  : Type where
    sParametrized : Parametrized → UpdateNonceState
    sInitialized : Initialized  → UpdateNonceState
    sAwaitingComputationPhase : AwaitingComputationPhase → UpdateNonceState
    sAwaitingProvided : (a : AwaitingOrProvided) → (missing : Bool) → AwaitingProvided a missing → UpdateNonceState
    sClosed : Closed  → UpdateNonceState
    sAwaitingGracefulClosure : AwaitingGracefulClosure  → UpdateNonceState
    sUngracefullyClosed : UngracefullyClosed  → UpdateNonceState
    -- constructor ⟦_,_,_⟧ᵘˢ
    -- field
      -- pre-ηc : Nonce -- evolving phi-nonce TODO rename
      -- ηv : Nonce -- evolving eta-nonce TODO rename
      -- ηc : Nonce -- candidate eta-nonce
  \end{code}
  \end{AgdaSuppressSpace}
  \emph{Update Nonce transitions}
  \begin{code}[hide]

  data
  \end{code}
  \begin{code}
    _⊢_⇀⦇_,UPDN⦈_ : UpdateNonceEnv → UpdateNonceState  → UpdateNonceSignal  → UpdateNonceState  → Type
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
    s : Slot
    spr : ℕ -- VDF discriminant size
    cnt : ℕ
    ei ei' : ℕ
    pz ps : Parametrized
    a b c : G
    ds : ℤ
    η ηv ηc pre-ηc pre-ηₑ ηₑ : Nonce
    inl : Initialized
    φᵢ : G × G
    xᵢ yᵢ πᵢ : G
    I : G
    pins pins' : AttestedOutputsM
    m : Bool

    -- make this operate on intervals not slots TODO
    -- 3 k/f 
    -- intervals = 12 (hours) * numSecondsInHour
    -- slotToIntervalIndex s = s / 120
    -- convert RandomnessStabilisationWindowPlusOne to intervals also
    -- 9 k/f - beginning of computation phase 
    
  -- TODO parametrize transition probably should be the hard for transition spec?
  -- TODO  initialize-pr , ... , tick-ic rules all need the update process for the Nonce to continue as in the previous Ouroboros version 
  -- TODO state must always have SOME epoch nonce!
  -- TODO List Bool should be Nonce

  data _⊢_⇀⦇_,UPDN⦈_ where
    -- Update-Both :
    --   ∙ s + RandomnessStabilisationWindow < firstSlot (sucᵉ (epoch s))
    --   ────────────────────────────────
    --   ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηv ⋆ η ⟧ᵘˢ

    -- Only-Evolve :
    --   ∙ s + RandomnessStabilisationWindow ≥ firstSlot (sucᵉ (epoch s))
    --   ────────────────────────────────
    --   ⟦ η ⟧ᵘᵉ ⊢ ⟦ ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ ηv ⋆ η , ηc ⟧ᵘˢ

    initialize-pr : 
      ∙ true ≡ true 
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ (sParametrized ps ) ⇀⦇ (s , iInitialize ⟦ ei ⟧ⁱⁱ)  ,UPDN⦈ (sInitialized ⟦ ps , ds , ei , pre-ηₑ ⟧ⁱ)

    -- TODO how is I used here at all?
    initialize-cl : 
      ∙ true ≡ true 
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sClosed ⟦ ⟦ ps , ds , ei , pre-ηₑ ⟧ⁱ , pins , ( a , b , c ) , ηₑ ⟧ᶜᵈ ) ⇀⦇ (s , iInitialize ⟦ ei ⟧ⁱⁱ)  ,UPDN⦈ (sInitialized ⟦ ps , ds , ei , pre-ηₑ ⟧ⁱ)

    tick-i : 
      ∙ true ≡ true 
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ (sInitialized inl ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ )
      
    tick-igp : 
      ∙ isIGP ≡ getPhase s inl
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ )
      
    tick-ic : -- TODO allN is an empty vector of outputs, is this right? not in the CIP
      ∙ isCP ≡ getPhase s inl
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided awaiting false ⟦ inl , allN ⟧ᵃᵖ )

    -- works for missing and non-missing
    provideOut : -- TODO allN is an empty vector of outputs, is this right? not in the CIP
      let i = getInterval s
          xᵢ = mkInput pre-ηₑ i -- TODO what is b“challenge"||bin(e)?? fix
          pins' = unsafeUpdateAt82 i xᵢ (proj₁ φᵢ) pins
      in
      ∙ (VDFg .VDF.verify) xᵢ (proj₁ φᵢ) (inl .Initialized.parametrized .Parametrized.I) (proj₂ φᵢ) ≡ true
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting m ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , (iProvideAttestedOutput awaiting ⟦ φᵢ ⟧ᵖᵃᵒ ))  ,UPDN⦈ ( sAwaitingProvided provided m ⟦ inl , pins' ⟧ᵃᵖ )    
  
    tick-ci-p : 
      ∙ getPhase s inl ≡ isWithinCurrentInterval
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ )
      
    tick-ni-p : 
      ∙ getPhase s inl ≡ isWithinNextInterval
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ )
      
    tick-cup-p :
      ∙ getPhase s inl ≡ isWithinCatchUpPhase
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ )

    tick-ci-a : 
      ∙ getPhase s inl ≡ isWithinCurrentInterval
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingComputationPhase ⟦ inl ⟧ᵃᶜᵖ )
      
    tick-cu-a : 
      ∙ getPhase s inl ≡ isWithinCatchUpPhase
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ )
      
    tick-ic-a :
      ∙ getPhase s inl ≡ isClosable
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting false ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingGracefulClosure ⟦ inl , pins ⟧ᵃᵍᶜ )

    tick-cu-mp : 
      ∙ getPhase s inl ≡ isWithinCurrentInterval
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ )
      
    tick-ni-mp : 
      ∙ getPhase s inl ≡ isWithinNextInterval
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ )
      
    tick-ic-mp : 
      ∙ getPhase s inl ≡ isClosable
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingGracefulClosure ⟦ inl , pins ⟧ᵃᵍᶜ )
      
    tick-uc-mp : 
      ∙ getPhase s inl ≡ isUngracefullyClosable
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided provided true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ )

    tick-ci-m : 
      ∙ getPhase s inl ≡ isWithinCurrentInterval
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ )
      
    tick-c-m : 
      ∙ getPhase s inl ≡ isClosable
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sAwaitingGracefulClosure ⟦ inl , pins ⟧ᵃᵍᶜ )
      
    tick-uc-m : 
      ∙ getPhase s inl ≡ isUngracefullyClosable
      ────────────────────────────────
      ⟦ η ⟧ᵘᵉ ⊢ ( sAwaitingProvided awaiting true ⟦ inl , pins ⟧ᵃᵖ ) ⇀⦇ (s , iTick)  ,UPDN⦈ ( sUngracefullyClosed ⟦ inl , pins , inl .Initialized.pre-ηₑ ⟧ᵘᶜ ) 
  \end{code}
  \caption{Update Nonce transition system rules}
  \label{fig:ts-rules:updatenonce}
  \end{figure*}

-- xi=Hash(b“challenge"||bin(e)||pre-ηe||bin(i))
-- attestedOutputs[i]↢ϕi
-- Ensure ϕ i is valid by verifying: VDF.Verify ( ( G , Δ , ⋅ ) , x i , y i , I , π i )

-- AwaitingComputationPhase{initialized↢initialiinitializedzedState,currentSlot↢0}
--   Δ challenge ← Hash ( bin ( epochId e ) | pre- η e ) 
--   ( G , Δ , ⋅ ) ← VDF.Setup ( λ , Δ challenge ) 
--   Returned State 
--   Initialized { parametrized ↢ ( λ , I ) , group ↢ G , discriminant ↢ Δ , operation ↢ ⋅ , epochId e ↢ epochId e , pre- η e ↢ pre- η e }


--     -- [ new-pre-nonce ] -> [ new-pre-nonce ; vda-function new-pre-nonce ]
--     -- applies at : one slot right before candidate becomes fixed
--     -- new pre-nonce from evolving nonce 
--     -- candidate nonce set to final update of current pre-nonce
--       ∙ slotToIntervalIndex s ≡ ? -- interval = 0
--       ────────────────────────────────
--       ⟦ η ⟧ᵘᵉ ⊢ ⟦ pre-ηc , ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ securityParameter , I , ηv ⋆ η , ηv ⋆ η , grindingf pre-ηc ⟧ᵘˢ

--     -- applies at : all other slots before candidate is fixed
--     -- pre-nonce updated with grindingf
--     -- candidate nonce set the same as pre-nonce
--     Update-All :
--       ∙ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s))
--       ────────────────────────────────
--       ⟦ η ⟧ᵘᵉ ⊢ ⟦ pre-ηc , ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ grindingf pre-ηc ∷ pre-ηc , ηv ⋆ η , grindingf pre-ηc ⟧ᵘˢ

--     -- applies at : within RandomnessStabilisationWindowPlusOne of next epoch
--     -- pre-nonce updated with grindingf
--     -- candidate nonce kept constant
--     Keep-PreN :
--       ∙ s + RandomnessStabilisationWindowPlusOne > firstSlot (sucᵉ (epoch s))
--       ────────────────────────────────
--       ⟦ η ⟧ᵘᵉ ⊢ ⟦ pre-ηc , ηv , ηc ⟧ᵘˢ ⇀⦇ s ,UPDN⦈ ⟦ grindingf pre-ηc , ηv ⋆ η , ηc ⟧ᵘˢ 


