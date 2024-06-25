{-# OPTIONS --safe #-}

module Interface.IsCommutativeMonoid where

open import Algebra using (IsCommutativeMonoid; CommutativeMonoid)
open import Prelude

record IsCommutativeMonoid' c ℓ Carrier : Type (sucˡ (c ⊔ˡ ℓ)) where
  infix  4 _≈_
  field
    _≈_                 : Carrier → Carrier → Type ℓ
    ⦃ semigroup ⦄       : Semigroup Carrier
    ⦃ monoid ⦄          : Monoid Carrier
    isCommutativeMonoid : IsCommutativeMonoid {c} _≈_ _◇_ ε

fromCommMonoid' : ∀ {c ℓ Carrier} → IsCommutativeMonoid' c ℓ Carrier → CommutativeMonoid c ℓ
fromCommMonoid' c = record { IsCommutativeMonoid' c }

toCommMonoid' : ∀ {c ℓ} → (m : CommutativeMonoid c ℓ) → IsCommutativeMonoid' c ℓ (CommutativeMonoid.Carrier m)
toCommMonoid' c = record
  { CommutativeMonoid c using (_≈_; isCommutativeMonoid)
  ; semigroup = λ where ._◇_ → CommutativeMonoid._∙_ c
  ; monoid    = λ where .ε   → CommutativeMonoid.ε c }

open import Data.Nat.Properties using (+-0-commutativeMonoid)

instance
  ℕ-commMonoid' = toCommMonoid' +-0-commutativeMonoid
