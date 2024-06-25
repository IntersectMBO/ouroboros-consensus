{-# OPTIONS --safe --without-K #-}

module Prelude where

open import Agda.Primitive using (lzero) renaming (Set to Type) public

open import Level public
  hiding (lower)
  renaming (_⊔_ to _⊔ˡ_; suc to sucˡ; zero to zeroˡ)
open import Function public

open import Data.Bool public
  hiding (_≟_; _≤_; _≤?_; _<_; _<?_; if_then_else_)
open import Data.Empty public
open import Data.List public
  hiding (align; alignWith; filter; fromMaybe; map; zip; zipWith)
open import Data.List.Membership.Propositional public
  using () renaming (_∈_ to _∈ˡ_; _∉_ to _∉ˡ_)
open import Data.Maybe public
  hiding (_>>=_; align; alignWith; ap; fromMaybe; map; zip; zipWith)
open import Data.Unit public
  using (⊤; tt)
open import Data.Unit.Polymorphic public
  using ()
  renaming (⊤ to ⊤↑; tt to tt↑)
instance Poly-tt = tt↑
open import Data.Sum public
  hiding (assocʳ; assocˡ; map; map₁; map₂; reduce; swap)
open import Data.Product public
  hiding (assocʳ; assocˡ; map; map₁; map₂; map₂′; swap; _<*>_)
open import Data.Nat public
  hiding (_≟_; _≤_; _≤?_; _<_; _<?_; _≤ᵇ_; _≡ᵇ_; _≥_; _>_; less-than-or-equal)
  renaming (_+_ to _+ℕ_)
open import Data.Integer as ℤ public
  using (ℤ)
  renaming (_+_ to _+ℤ_)
open import Data.String public
  using (String; _<+>_)

open import Relation.Nullary public
open import Relation.Nullary.Negation public
open import Relation.Nullary.Decidable public
  using (Dec; yes; no; dec-yes; dec-no; ⌊_⌋; ¬?; toWitness; fromWitness)
  renaming (map′ to mapDec)
open import Relation.Unary public
  using (Pred) renaming (Decidable to Decidable¹)
open import Relation.Binary public
  using () renaming (Decidable to Decidable²)
open import Relation.Binary.PropositionalEquality public
  hiding (preorder; isPreorder; setoid; [_])

open import Class.Core public
  using (Type↑)
open import Class.Functor public
  renaming (fmap to map)
open import Class.Bifunctor public
open import Class.Semigroup public
open import Class.Monoid public
open import Class.Applicative public
open import Class.Monad public
open import Class.DecEq public; instance DecEq-×′ = DecEq-×
open import Class.Decidable public
∃⁇ : ∀ {ℓ} → Set (sucˡ ℓ)
∃⁇ {ℓ} = Σ (Set ℓ) _⁇
open import Class.Show public

∃₂-syntax : ∀ {a b c} {A : Set a} {B : Set b}
     (C : A → B → Set c) → Set _
∃₂-syntax C = ∃ λ a → ∃ λ b → C a b

infix 2 ∃₂-syntax
syntax ∃₂-syntax (λ x y → C) = ∃₂[ x , y ] C

-- Instance for number literals, not enabled by default
import Data.Nat.Literals as ℕ

instance Number-ℕ = ℕ.number
