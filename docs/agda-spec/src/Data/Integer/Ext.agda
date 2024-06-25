{-# OPTIONS --safe #-}

module Data.Integer.Ext where

open import Data.Integer
open import Data.Integer.Properties using ([1+m]⊖[1+n]≡m⊖n)
open import Data.Nat
open import Data.Product
open import Data.Sign
open import Relation.Binary.PropositionalEquality using (_≡_; sym; cong; trans)

ℤtoSignedℕ : ℤ → Sign × ℕ
ℤtoSignedℕ x = (sign x , ∣ x ∣)

posPart : ℤ → ℕ
posPart x with ℤtoSignedℕ x
... | (Sign.+ , x) = x
... | _            = 0

negPart : ℤ → ℕ
negPart x with ℤtoSignedℕ x
... | (Sign.- , x) = x
... | _            = 0

∸≡posPart⊖ : {m n : ℕ} → (m ∸ n) ≡ posPart (m ⊖ n)
∸≡posPart⊖ {zero} {zero} = _≡_.refl
∸≡posPart⊖ {zero} {ℕ.suc n} = _≡_.refl
∸≡posPart⊖ {ℕ.suc m} {zero} = _≡_.refl
∸≡posPart⊖ {ℕ.suc m} {ℕ.suc n} = trans (∸≡posPart⊖{m}{n}) (sym (cong posPart (([1+m]⊖[1+n]≡m⊖n m n))))
