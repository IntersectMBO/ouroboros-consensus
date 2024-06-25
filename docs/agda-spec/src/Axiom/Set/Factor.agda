{-# OPTIONS --safe --no-import-sorts #-}

open import Prelude
open import Axiom.Set

module Axiom.Set.Factor (th : Theory {lzero}) where

open Theory th
open import Axiom.Set.Properties th

import Function.Related.Propositional as R
open import Data.List.Ext.Properties
open import Data.List.Membership.Propositional using () renaming (_∈_ to _∈ˡ_)
open import Data.List.Relation.Binary.BagAndSetEquality
open import Data.List.Relation.Binary.Disjoint.Propositional
open import Data.List.Relation.Binary.Permutation.Propositional
open import Data.List.Relation.Unary.Unique.DecPropositional.Properties
open import Data.List.Relation.Unary.Unique.Propositional
open import Data.List.Relation.Unary.Unique.Propositional.Properties.WithK
open import Class.DecEq
open import Relation.Binary

open Equivalence

private variable A B : Type

_ᶠ : (X : Set A) → ⦃ finite X ⦄ → FinSet A
_ᶠ X ⦃ Xᶠ ⦄ = X , Xᶠ

instance
  ∪-preserves-finite' : {X Y : Set A} → ⦃ finite X ⦄ → ⦃ finite Y ⦄ → finite (X ∪ Y)
  ∪-preserves-finite' ⦃ Xᶠ ⦄ ⦃ Yᶠ ⦄ = ∪-preserves-finite Xᶠ Yᶠ

module Factor (_≈_ : B → B → Type) (f : List A → B) (f-cong : ∀ {l l'} → l ∼[ set ] l' → f l ≈ f l') where

  factor : FinSet A → B
  factor (_ , l , _) = f l

  factor-cong : factor Preserves (_≡ᵉ_ on proj₁) ⟶ _≈_
  factor-cong {X , Xˡ , hX} {Y , Yˡ , hY} X≡ᵉY = f-cong λ {a} →
    a ∈ˡ Xˡ ∼⟨ R.SK-sym hX ⟩ a ∈  X  ∼⟨ to ≡ᵉ⇔≡ᵉ' X≡ᵉY _ ⟩
    a ∈  Y  ∼⟨ hY ⟩          a ∈ˡ Yˡ ∎
    where open R.EquationalReasoning

  factor-∪ : ∀ {R : B → B → B → Type} {X Y : Set A} ⦃ Xᶠ : finite X ⦄ ⦃ Yᶠ : finite Y ⦄
           → (∀ {l l'} → R (f l) (f l') (f (l ++ l')))
           → R (factor (X ᶠ)) (factor (Y ᶠ)) (factor ((X ∪ Y) ᶠ))
  factor-∪ hR = hR

module FactorUnique ⦃ _ : DecEq A ⦄ (_≈_ : B → B → Type) (f : (Σ (List A) Unique) → B)
                    (f-cong : ∀ {l l'} → proj₁ l ↭ proj₁ l' → f l ≈ f l') where

  f-cong' : ∀ {l l'} → (∀ {a} → a ∈ˡ proj₁ l ⇔ a ∈ˡ proj₁ l') → f l ≈ f l'
  f-cong' {l} {l'} h = f-cong (∼bag⇒↭ (unique∧set⇒bag (proj₂ l) (proj₂ l') h))

  deduplicate-Σ : List A → Σ (List A) Unique
  deduplicate-Σ l = (deduplicate _≟_ l , deduplicate-! _≟_ _)

  ext : List A → B
  ext = f ∘ deduplicate-Σ

  ext-cong : ∀ {l l'} → l ∼[ set ] l' → ext l ≈ ext l'
  ext-cong {l} {l'} h = f-cong' λ {a} →
    a ∈ˡ deduplicate _≟_ l  ∼⟨ R.SK-sym ∈-dedup ⟩ a ∈ˡ l                  ∼⟨ h ⟩
    a ∈ˡ l'                 ∼⟨ ∈-dedup ⟩          a ∈ˡ deduplicate _≟_ l' ∎
    where open R.EquationalReasoning

  open Factor _≈_ ext ext-cong public

  factor-∪' : ∀ {R : B → B → B → Type} {X Y : Set A} ⦃ Xᶠ : finite X ⦄ ⦃ Yᶠ : finite Y ⦄
            → disjoint X Y
            → (∀ {l l'} → Disjoint l l' → R (ext l) (ext l') (ext (l ++ l')))
            → R (factor (X ᶠ)) (factor (Y ᶠ)) (factor ((X ∪ Y) ᶠ))
  factor-∪' ⦃ _ , Xᶠ ⦄ ⦃ _ , Yᶠ ⦄ disj hR = hR λ where (a∈X , a∈Y) → ⊥-elim $ disj (from Xᶠ a∈X) (from Yᶠ a∈Y)
