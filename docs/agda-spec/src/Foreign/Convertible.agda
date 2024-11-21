module Foreign.Convertible where

open import Ledger.Prelude
open import Foreign.HaskellTypes

record Convertible (A B : Type) : Type where
  field to   : A → B
        from : B → A
open Convertible ⦃...⦄ public

HsConvertible : (A : Set) → ⦃ HasHsType A ⦄ → Set
HsConvertible A = Convertible A (HsType A)

Convertible-Refl : ∀ {A} → Convertible A A
Convertible-Refl = λ where .to → id; .from → id

Convertible₁ : (Type → Type) → (Type → Type) → Type₁
Convertible₁ T U = ∀ {A B} → ⦃ Convertible A B ⦄ → Convertible (T A) (U B)

Convertible₂ : (Type → Type → Type) → (Type → Type → Type) → Type₁
Convertible₂ T U = ∀ {A B} → ⦃ Convertible A B ⦄ → Convertible₁ (T A) (U B)

Functor⇒Convertible : ∀ {F : Type↑} → ⦃ Functor F ⦄ → Convertible₁ F F
Functor⇒Convertible = λ where
  .to   → map to
  .from → map from

Bifunctor⇒Convertible : ∀ {F} → ⦃ Bifunctor F ⦄ → Convertible₂ F F
Bifunctor⇒Convertible = λ where
  .to   → bimap to to
  .from → bimap from from

_⨾_ : ∀ {A B C} → Convertible A B → Convertible B C → Convertible A C
(c ⨾ c') .to   = c' .to   ∘ c  .to
(c ⨾ c') .from = c  .from ∘ c' .from

-- ** instances

open import Foreign.Haskell
open import Foreign.Haskell.Coerce using (coerce)

instance
  Convertible-Maybe : Convertible₁ Maybe Maybe
  Convertible-Maybe = Functor⇒Convertible

  Convertible-× : Convertible₂ _×_ _×_
  Convertible-× = Bifunctor⇒Convertible

  Convertible-Pair : Convertible₂ _×_ Pair
  Convertible-Pair = λ where
    .to   → coerce ∘ bimap to to
    .from → bimap from from ∘ coerce

  Convertible-⊎ : Convertible₂ _⊎_ _⊎_
  Convertible-⊎ = Bifunctor⇒Convertible

  Convertible-Either : Convertible₂ _⊎_ Either
  Convertible-Either = λ where
    .to   → coerce ∘ bimap to to
    .from → bimap from from ∘ coerce

  Convertible-FinSet : Convertible₁ ℙ_ List
  Convertible-FinSet = λ where
    .to   → map to   ∘ setToList
    .from → fromList ∘ map from

  Convertible-Map : ∀ {K K' V V'} → ⦃ DecEq K ⦄
    → ⦃ Convertible K K' ⦄ → ⦃ Convertible V V' ⦄
    → Convertible (K ⇀ V) (List $ Pair K' V')
  Convertible-Map = λ where
    .to   → to        ∘ proj₁
    .from → fromListᵐ ∘ map from

  Convertible-List : Convertible₁ List List
  Convertible-List = λ where
    .to   → map to
    .from → map from

  Convertible-Fun : ∀ {A A' B B'} → ⦃ Convertible A A' ⦄ → ⦃ Convertible B B' ⦄ → Convertible (A → B) (A' → B')
  Convertible-Fun = λ where
    .to   → λ f → to   ∘ f ∘ from
    .from → λ f → from ∘ f ∘ to
