{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)

module Spec.UpdateNonce.Properties
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (grindingf : Nonce → Nonce)
  where

open import Ledger.Prelude
open Computational ⦃...⦄
open import Spec.UpdateNonce crypto nonces es grindingf

instance
  Computational-UPDN : Computational _⊢_⇀⦇_,UPDN⦈_ String
  Computational-UPDN .computeProof _ _ s =
    case ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ of λ where
      (yes p) → success (-, Update-All p)
      (no ¬p) → case ¿ s + RandomnessStabilisationWindowPlusOne ≡ firstSlot (sucᵉ (epoch s)) ¿ of λ where
        (yes q) → success (-, New-PreN q)
        (no ¬q) → success (-, Keep-PreN {!   !} ) --(≮⇒≥ {A = Slot} _≟_ ¬p))
  Computational-UPDN .completeness _ _ s _ = {!   !} 
  -- (Update-All p)
  --   rewrite dec-yes ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ p .proj₂ =
  --     refl
  -- Computational-UPDN .completeness _ _ s _ (New-PreN p)
  --   rewrite dec-no  ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ $ ≥⇒≮ {A = Slot} p =
  --     refl
  -- Computational-UPDN .completeness _ _ s _ (Keep-PreN p)
  --   rewrite dec-yes ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ p .proj₂ =
  --     refl
