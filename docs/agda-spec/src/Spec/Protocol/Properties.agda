{-# OPTIONS --safe #-}

open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions
open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Data.Rational.Ext

module Spec.Protocol.Properties
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)
  (bs     : BlockStructure crypto nonces es ss) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  (rs     : _) (open RationalExtStructure rs)
  where

open import Data.Rational as ℚ using (1ℚ)
open import Ledger.Prelude
open import Tactic.GenError
open import Spec.Protocol crypto nonces es ss bs af rs
open import Spec.BaseTypes crypto using (OCertCounters)
open import Spec.UpdateNonce crypto nonces es
open import Spec.UpdateNonce.Properties crypto nonces es
open import Spec.OperationalCertificate crypto nonces es ss bs af
open import Spec.OperationalCertificate.Properties crypto nonces es ss bs af
open import InterfaceLibrary.Common.BaseTypes crypto using (PoolDistr; lookupPoolDistr)

private

  checkLeaderVal? : ∀ cert int σ → Dec (checkLeaderVal cert int σ)
  checkLeaderVal? (certℕ , certℕprf) (f , posf , f≤1) σ
    with f ≟ 1ℚ
  ... | yes _   = yes _
  ... | no f≢1ℚ =
        let
          p = ℤ.pos certℕ ℚ./ (2 ^ 512)
          q = 1ℚ ℚ.- p
          p≢1ℚ  = ↥p<↧p⇒p≢1 {p} (n<m⇒↥[n/m]<↧[n/m] certℕprf)
          1-f≥0 = p≤1⇒1-p≥0 f≤1
          1-f≢0 = p≢1⇒1-p≢0 f≢1ℚ
          instance
            q≢0ℚ  = ℚ.≢-nonZero (p≢1⇒1-p≢0 p≢1ℚ)
            1-f>0 = ℚ.positive (≤∧≢⇒< 1-f≥0 $ ≢-sym 1-f≢0)
          c = ln (1ℚ ℚ.- f)
        in
          ℚ.1/ q ℚ.<? exp ((ℚ.- σ) ℚ.* c)

  vrfChecks? : ∀ η₀ pd f bhb → Dec (vrfChecks η₀ pd f bhb)
  vrfChecks? η₀ pd f bhb
    with lookupPoolDistr pd hk
    where open BHBody bhb; hk = hash issuerVk
  ... | nothing          = no λ ()
  ... | just (σ , vrfHK) =
          vrfHK ≟ hash vrfVk
          ×-dec ¿ verify {T = VRFRes} ¿³ vrfVk seed (vrfPrf , vrfRes)
          ×-dec checkLeaderVal? (hBLeader bhb) f σ
    where
      open BHBody bhb
      seed = slotToSeed slot XOR nonceToSeed η₀

instance

  _ = Monad-ComputationResult

  Computational-PRTCL : Computational _⊢_⇀⦇_,PRTCL⦈_ String
  Computational-PRTCL = record {Go} where
    open Computational ⦃...⦄ renaming (computeProof to comp; completeness to complete)
    computeUPDN  = comp {STS = _⊢_⇀⦇_,UPDN⦈_}
    computeOCERT = comp {STS = _⊢_⇀⦇_,OCERT⦈_}
    module Go
      (Γ  : PrtclEnv)   (let ⟦ pd , η₀ ⟧ᵖᵉ = Γ)
      (s  : PrtclState) (let ⟦ cs , ηv , ηc ⟧ᵖˢ = s)
      (bh : BHeader)    (let (bhb , σ) = bh; open BHBody bhb)
      where

      η      = hBNonce bhb
      updnΓ  = UpdateNonceEnv ∋ ⟦ η ⟧ᵘᵉ
      updnSt = UpdateNonceState ∋ ⟦ ηv , ηc ⟧ᵘˢ
      ocertΓ = OCertEnv ∋ dom (pd ˢ)

      hyps = vrfChecks? η₀ pd ActiveSlotCoeff bhb

      computeProof : ComputationResult String (∃[ s′ ] Γ ⊢ s ⇀⦇ bh ,PRTCL⦈ s′)
      computeProof = case hyps of λ where
        (yes prf) → do
          (_ , updnStep)  ← computeUPDN updnΓ updnSt slot
          (_ , ocertStep) ← computeOCERT ocertΓ cs bh
          success (-, Evolve-Prtcl (updnStep , ocertStep , prf))
        (no ¬prf) → failure (genErrors ¬prf)

      completeness : ∀ s′ → Γ ⊢ s ⇀⦇ bh ,PRTCL⦈ s′ → (proj₁ <$> computeProof) ≡ success s′
      completeness _ (Evolve-Prtcl (updnStep , ocertStep , p))
        with hyps
      ... | no ¬p = contradiction p ¬p
      ... | yes _
        with computeUPDN updnΓ updnSt slot | complete updnΓ updnSt _ _ updnStep
      ... | success _ | refl
        with computeOCERT ocertΓ cs bh | complete _ _ _ _ ocertStep
      ... | success _ | refl = refl
