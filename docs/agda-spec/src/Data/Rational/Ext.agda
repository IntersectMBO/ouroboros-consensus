{-# OPTIONS --safe #-}

open import Agda.Primitive renaming (Set to Type)

module Data.Rational.Ext where

open import Function using (_∘_; _$_)
open import Data.Rational
open import Data.Rational.Properties
open import Data.Product using (_×_; ∃-syntax)
open import Data.Nat as ℕ using (ℕ)
open import Data.Integer as ℤ using (ℤ; +_)
open import Data.Integer.GCD using (gcd)
import Data.Integer.Properties as ℤ
open import Relation.Binary.PropositionalEquality.Core using (_≡_; _≢_; refl)
open import Relation.Binary.PropositionalEquality using (cong; sym; module ≡-Reasoning)
open import Relation.Nullary.Negation using (contradiction)

-- The interval (0, 1]
PosUnitInterval : Type
PosUnitInterval = ∃[ p ] Positive p × p ≤ 1ℚ

record RationalExtStructure : Type where
  field
    exp : ℚ → ℚ
    ln  : (p : ℚ) → ⦃ Positive p ⦄ → ℚ

p≢1⇒1-p≢0 : ∀ {p : ℚ} → p ≢ 1ℚ → 1ℚ - p ≢ 0ℚ
p≢1⇒1-p≢0 {p} p≢1 1-p≡0 = contradiction p≡1 p≢1
  where
    open ≡-Reasoning
    p≡1 = sym $ begin
      1ℚ             ≡⟨ sym $ +-identityʳ 1ℚ ⟩
      1ℚ + 0ℚ        ≡⟨ cong (λ i → 1ℚ + i) (sym $ +-inverseˡ p) ⟩
      1ℚ + (- p + p) ≡⟨ sym $ +-assoc 1ℚ (- p) p ⟩
      (1ℚ - p) + p   ≡⟨ cong (_+ p) 1-p≡0 ⟩
      0ℚ + p         ≡⟨ +-identityˡ p ⟩
      p              ∎

p≤1⇒1-p≥0 : ∀ {p : ℚ} → p ≤ 1ℚ → 1ℚ - p ≥ 0ℚ
p≤1⇒1-p≥0 {p} p≤1 = +-monoʳ-≤ 1ℚ -p≥-1
  where
    -p≥-1 = neg-antimono-≤ p≤1

drop-≢ : ∀ {p q : ℚ} → p ≢ q → ↥ p ℤ.* ↧ q ≢ ↥ q ℤ.* ↧ p
drop-≢ p≢q = p≢q ∘ ≃⇒≡ ∘ *≡*

≤∧≢⇒< : ∀ {p q : ℚ} → p ≤ q → p ≢ q → p < q
≤∧≢⇒< p≤q p≢q = *<* $ ℤ.≤∧≢⇒< (drop-*≤* p≤q) (drop-≢ p≢q)

p≡1⇒↥p≡↧p : ∀ {p : ℚ} → p ≡ 1ℚ → ↥ p ≡ ↧ p
p≡1⇒↥p≡↧p {p} p≡1
  rewrite sym (ℤ.*-identityʳ (↥ p))
        | drop-*≡* (≡⇒≃ p≡1)
        | ℤ.*-identityˡ (↧ p)
        = refl 

↥p<↧p⇒p≢1 : ∀ {p : ℚ} → ↥ p ℤ.< ↧ p → p ≢ 1ℚ
↥p<↧p⇒p≢1 n<m n/m≡1 = (ℤ.<⇒≢ n<m) (p≡1⇒↥p≡↧p n/m≡1)

n<m⇒↥[n/m]<↧[n/m] : ∀ {n m : ℕ} → ⦃ _ : ℕ.NonZero m ⦄ → n ℕ.< m → ↥ (+ n / m) ℤ.< ↧ (+ n / m)
n<m⇒↥[n/m]<↧[n/m] {n} {m} n<m = ℤ.*-cancelʳ-<-nonNeg g $ begin-strict
  (↥ (+ n / m)) ℤ.* g ≡⟨ ↥-/ (+ n) m ⟩
  + n                 <⟨ ℤ.+<+ n<m ⟩
  + m                 ≡⟨ sym (↧-/ (+ n) m) ⟩
  ↧ (+ n / m) ℤ.* g   ∎ where open ℤ.≤-Reasoning; g = gcd (+ n) (+ m)
