{-# OPTIONS --safe #-}

open import Ledger.Crypto
open import Ledger.Script
open import Ledger.Types.Epoch
open import Spec.BaseTypes using (Nonces)
open import Spec.BlockDefinitions

module Spec.OperationalCertificate.Properties
  (crypto : _) (open Crypto crypto)
  (nonces : Nonces crypto) (open Nonces nonces)
  (es     : _) (open EpochStructure es)
  (ss     : ScriptStructure crypto es) (open ScriptStructure ss)
  (bs     : BlockStructure crypto nonces es ss) (open BlockStructure bs)
  (af     : _) (open AbstractFunctions af)
  where

open import Ledger.Prelude
open import Data.Maybe.Relation.Unary.Any as M
open import Spec.OperationalCertificate crypto nonces es ss bs af

instance

  Computational-OCERT : Computational _⊢_⇀⦇_,OCERT⦈_ String
  Computational-OCERT = record {Go} where
    module Go (stpools : OCertEnv) (cs : OCertState) (bh : BHeader) where
      bhb = bh .proj₁; open BHBody bhb
      σ = bh .proj₂
      open OCert oc renaming (σ to τ)
      hk = hash issuerVk
      kp = kesPeriod slot
      t = kp -ᵏ c₀

      hyps = ¿ c₀ ≤ kp
             × kp < c₀ +ᵏ MaxKESEvo
             × M.Any (λ m → n ≡ m ⊎ n ≡ suc m) (currentIssueNo stpools cs hk)
             × isSignedˢ issuerVk (encode (vkₕ , n , c₀)) τ
             × isSignedᵏ vkₕ t (encode bhb) σ ¿

      computeProof : ComputationResult String (∃[ cs′ ] stpools ⊢ cs ⇀⦇ bh ,OCERT⦈ cs′)
      computeProof with hyps
      ... | no ¬p = failure "Failed in OCERT"
      ... | yes (p₁ , p₂ , p₃ , p₄ , p₅) = success (-, Update-OCert (p₁ , p₂ , satisfied× p₃ , p₄ , p₅))
        where
          satisfied× : ∀ {a p} {A : Set a} {y : Maybe A} {P : Pred A p} → M.Any P y → ∃[ x ] just x ≡ y × P x
          satisfied× (just Px) = (-, refl , Px)

      completeness : ∀ cs′ → stpools ⊢ cs ⇀⦇ bh ,OCERT⦈ cs′ → (proj₁ <$> computeProof) ≡ success cs′
      completeness _ (Update-OCert (p₁ , p₂ , (_ , p , q) , p₄ , p₅))
        with hyps
      ... | yes prf rewrite dec-yes hyps prf .proj₂ = refl
      ... | no ¬prf = contradiction (p₁ , p₂ , AnyP×≡→AnyP (Equivalence.to M.just-equivalence q) p , p₄ , p₅) ¬prf
        where
          AnyP×≡→AnyP : ∀ {a p} {A : Set a} {x : A} {y : Maybe A} {P : Pred A p} → M.Any P (just x) → just x ≡ y → M.Any P y
          AnyP×≡→AnyP {P = P} prf eq = subst (M.Any P) eq prf
