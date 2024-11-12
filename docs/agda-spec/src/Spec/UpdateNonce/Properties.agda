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

sr<fs : ∀ s → Dec (s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)))
sr<fs s = ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ 

sr≡fs : ∀ (s : Slot) → Dec (s + RandomnessStabilisationWindowPlusOne ≡ firstSlot (sucᵉ (epoch s)))
sr≡fs s = ¿ s + RandomnessStabilisationWindowPlusOne ≡ firstSlot (sucᵉ (epoch s)) ¿ 

sr>fs : ∀ s → Dec (s + RandomnessStabilisationWindowPlusOne > firstSlot (sucᵉ (epoch s)))
sr>fs s = ¿ s + RandomnessStabilisationWindowPlusOne > firstSlot (sucᵉ (epoch s)) ¿ 

instance
  Computational-UPDN : Computational _⊢_⇀⦇_,UPDN⦈_ String
  Computational-UPDN .computeProof _ _ s =
    case ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ of λ where
      (yes p) → success (-, Update-All p)
      (no ¬p) → case ¿ s + RandomnessStabilisationWindowPlusOne ≡ firstSlot (sucᵉ (epoch s)) ¿ of λ where
        (yes q) → success (-, New-PreN q)
        (no ¬q) → success (-, Keep-PreN (≥∧≉⇒> (s + RandomnessStabilisationWindowPlusOne) (firstSlot (sucᵉ (epoch s))) ( (≮⇒≥ {A = Slot} _≟_ ¬p) , ¬q ) ))
  
  Computational-UPDN .completeness _ _ s _ (Update-All p) -- <
        with (sr<fs s)                 | (sr≡fs s)                | (sr>fs s)
  ... | yes _                   | _                    | _      = refl 
  ... | _                      | yes q                  | _      = contradiction q (proj₂ ( <⇒≤∧≉ {x = s + RandomnessStabilisationWindowPlusOne} {y = firstSlot (sucᵉ (epoch s))} p)) 
  ... | _                   |  _                      | yes q  = contradiction q ( <-asymmetric {x = s + RandomnessStabilisationWindowPlusOne} {y = firstSlot (sucᵉ (epoch s))} p) 
  ... | no ¬p                   | no _                   | no _  = contradiction p ¬p

  Computational-UPDN .completeness _ _ s _ (New-PreN p) 
        with (sr≡fs s)            |  (sr<fs s)               | (sr>fs s)
  ... | yes q                   | no ¬t                    | _  rewrite dec-yes (sr≡fs s) q .proj₂ = refl
  ... | _                      | yes q                  | _      = contradiction p (proj₂ ( <⇒≤∧≉ {x = s + RandomnessStabilisationWindowPlusOne} {y = firstSlot (sucᵉ (epoch s))} q))
  ... | _                   |  _                      | yes q  = contradiction p (>⇒≉ (s + RandomnessStabilisationWindowPlusOne) (firstSlot (sucᵉ (epoch s))) q )
  ... | no ¬p                   | no _                   | no _  = contradiction p ¬p
 
  Computational-UPDN .completeness _ _ s _ (Keep-PreN p) -- >
       with (sr<fs s)         | (sr≡fs s)                | (sr>fs s)
  ... | yes q                   | _                    | _      = contradiction p ( <-asymmetric {x = s + RandomnessStabilisationWindowPlusOne} {y = firstSlot (sucᵉ (epoch s))} q) 
  ... | _                      | yes q                  | _      = contradiction q (>⇒≉ (s + RandomnessStabilisationWindowPlusOne) (firstSlot (sucᵉ (epoch s))) p )
  ... | no ¬t                   |  no ¬w                      | yes q = {!   !} -- rewrite dec-no (sr>fs s) $ >⇒≉∧n< {A = Slot} (s + RandomnessStabilisationWindowPlusOne) (firstSlot (sucᵉ (epoch s))) q .proj₂ = ? --refl
  ... | no _                   | no _                   | no ¬p  = contradiction p ¬p

-- rewrite dec-no  ¿ s + RandomnessStabilisationWindowPlusOne < firstSlot (sucᵉ (epoch s)) ¿ $ ≥⇒≮ {A = Slot} p = refl