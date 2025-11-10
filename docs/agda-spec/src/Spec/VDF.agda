{-# OPTIONS --safe #-} 

open import Ledger.Crypto
open import Ledger.Types.Epoch using (EpochStructure)
open import Spec.BaseTypes using (Nonces)

open import Ledger.Prelude

module Spec.VDF
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  where


-- TODO - replace with real group proof requirements! 
isGroup : (S : Set) → S → (S × S → S) → Set
isGroup _ _ _ = ⊤

-- TODO specify the real type we want discriminant to have
Discriminant : Type 
Discriminant = Nonce 

-- _ₛ : ∀ {Grp : Set} → Group {Grp} → Set
-- (_ₛ) {G} gr = G

record VDF {G : Set} : Type where
  field
    _*ᵍ_ : G × G → G -- Group operation used for VDF evaluation (e.g., modular exponentiation)
    idᵍ : G
    isG : isGroup (G) idᵍ _*ᵍ_ 
    Δ : Nonce -- (G,Δ,⋅)←VDF.Setup(λ,Δchallenge)
    iterationsFromDuration : ℕ → ℕ
    evaluate : G → ℕ → G -- y←VDF.Evaluate(x,I)
    prove : G → G → ℕ → G -- π←VDF.Prove(x,y,I)
    verify : G → G → ℕ → G → Bool -- {0,1}←VDF.Verify(x,y,I,π)
   -- aggregation 
    init : ℕ → Nonce → G × G × G -- (Accx,Accy,α)←VDF.Aggregation.Init(λ,pre-ηe)
    aggUpdate : ℕ → (G × G ) → (G × G × G ) → G × G × G -- (Accx,Accy,α)←VDF.Aggregation.Update(λ,(xi,yi),(Accx,Accy,α))
    aggProve : (G × G × G ) → ℕ → G -- π←VDF.Aggregation.Prove((Accx,Accy,_α),I)
    aggVerify : (G × G × ℕ) → ℕ → G → Bool -- {0,1}←VDF.Aggregation.Verify((Accx,Accy,_α),I,π)


    
        
