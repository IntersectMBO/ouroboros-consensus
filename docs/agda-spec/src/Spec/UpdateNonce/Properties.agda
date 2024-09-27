{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)

module Spec.UpdateNonce.Properties
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  where

open import Ledger.Prelude
open Computational ⦃...⦄
open import Spec.UpdateNonce crypto nonces es

instance
  Computational-UPDN : Computational _⊢_⇀⦇_,UPDN⦈_ String
  Computational-UPDN .computeProof _ _ s =
    case ¿ s + RandomnessStabilisationWindow < firstSlot (sucᵉ (epoch s)) ¿ of λ where
      (yes p) → success (-, Update-Both p)
      (no ¬p) → success (-, Only-Evolve (≮⇒≥ {A = Slot} _≟_ ¬p))
  Computational-UPDN .completeness _ _ s _ (Update-Both p)
    rewrite dec-yes ¿ s + RandomnessStabilisationWindow < firstSlot (sucᵉ (epoch s)) ¿ p .proj₂ =
      refl
  Computational-UPDN .completeness _ _ s _ (Only-Evolve p)
    rewrite dec-no  ¿ s + RandomnessStabilisationWindow < firstSlot (sucᵉ (epoch s)) ¿ $ ≥⇒≮ {A = Slot} p =
      refl
