{-# OPTIONS --safe #-}
module Interface.ToBool where

open import Prelude

private variable
  ℓ ℓ′ : Level
  X : Type ℓ; P : X → Type ℓ

record ToBool′ (A : Type ℓ) (P 𝕋 𝔽 : A → Type ℓ′) : Type (ℓ ⊔ˡ ℓ′) where
  field decide : (a : A) → ⦃ P a ⦄ → 𝕋 a ⊎ 𝔽 a

  infix -10 if_then_else_
  if_then_else_ : (a : A) ⦃ _ : P a ⦄ → ({𝕋 a} → X) → ({𝔽 a} → X) → X
  if a then t else f =
    case decide a of λ where
      (inj₁ 𝕥) → t {𝕥}
      (inj₂ 𝕗) → f {𝕗}

  toBool : (a : A) ⦃ _ : P a ⦄ → Bool
  toBool a = if a then true else false
open ToBool′ ⦃...⦄ public

ToBool : (A : Type ℓ) (𝕋 𝔽 : A → Type ℓ′) → Type (ℓ ⊔ˡ ℓ′)
ToBool {ℓ} A = ToBool′ A (λ _ → ⊤↑)

instance
  ToBool-Bool : ToBool Bool (_≡ true) (_≡ false)
  ToBool-Bool .decide = λ where
    true  → inj₁ refl
    false → inj₂ refl

  ToBool-Dec : ToBool (Dec X) (const X) (const $ ¬ X)
  ToBool-Dec .decide = λ where
    (yes x) → inj₁ x
    (no ¬x) → inj₂ ¬x

  ToBool-Maybe : ToBool (Maybe X) (const X) (const ⊤↑)
  ToBool-Maybe .decide = λ where
    (just x) → inj₁ x
    nothing  → inj₂ tt↑

  ToBool-⁇ : ToBool′ (Type ℓ) _⁇ id ¬_
  ToBool-⁇ .decide _ = decide dec
