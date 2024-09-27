{-# OPTIONS --safe #-}

module Interface.HasOrder.Instance where

open import Prelude
open import Class.DecEq
open import Interface.HasOrder

instance
  import Data.Sum as ⊎

  import Data.Nat.Properties as Nat hiding (_≟_)
  ℕ-hasPreorder = HasPreorder ∋ record {Nat; ≤⇔<∨≈ = let open Nat in mk⇔
    (λ a≤b → case _ ≟ _ of λ where (yes p) → inj₂ p ; (no ¬p) → inj₁ (≤∧≢⇒< a≤b ¬p))
    ⊎.[ <⇒≤ , ≤-reflexive ] }
  ℕ-hasPartialOrder = HasPartialOrder ∋ record
    { ≤-antisym = Nat.≤-antisym }
  ℕ-hasDecPartialOrder = HasDecPartialOrder {A = ℕ} ∋ record {}
  ℕ-hasTotalOrder = HasTotalOrder ∋ record
    { ≤-total = Nat.≤-total }
  ℕ-hasDecTotalOrder = HasDecTotalOrder {A = ℕ} ∋ record {}

  import Data.Integer.Properties as Int hiding (_≟_)
  ℤ-hasPreorder = HasPreorder ∋ record {Int; ≤⇔<∨≈ = let open Int in mk⇔
    (λ a≤b → case _ ≟ _ of λ where (yes p) → inj₂ p ; (no ¬p) → inj₁ (≤∧≢⇒< a≤b ¬p))
    ⊎.[ <⇒≤ , ≤-reflexive ] }
  ℤ-hasPartialOrder = HasPartialOrder ∋ record { ≤-antisym = Int.≤-antisym }
  ℤ-hasDecPartialOrder = HasDecPartialOrder {A = ℤ} ∋ record {}

  open import Data.Rational using (ℚ)
  import Data.Rational.Properties as Rat hiding (_≟_)
  ℚ-hasPreorder = hasPreorderFromNonStrict Rat.≤-isPreorder _≟_
  ℚ-hasPartialOrder = HasPartialOrder ∋ record { ≤-antisym = Rat.≤-antisym }
  ℚ-hasDecPartialOrder = HasDecPartialOrder {A = ℚ} ∋ record {}
  ℚ-hasTotalOrder = HasTotalOrder ∋ record
    { ≤-total = Rat.≤-total }
  ℚ-hasDecTotalOrder = HasDecTotalOrder {A = ℚ} ∋ record {}
