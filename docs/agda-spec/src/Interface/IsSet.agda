{-# OPTIONS --safe --no-import-sorts #-}

open import Axiom.Set

module Interface.IsSet (th : Theory) where

open import Prelude using (Type)
import Axiom.Set.Rel th as Rel
open import Axiom.Set.Map th as Map
open import Axiom.Set.TotalMap th as TotalMap
open import Data.Product
open import Function

open Theory th renaming (_∈_ to _∈ᵗ_; _∉_ to _∉ᵗ_)

private variable A B X : Type

record IsSet (A B : Type) : Type where
  field
    toSet : A → Set B

  infix 4 _∈_ _∉_
  _∈_ _∉_ : B → A → Type
  a ∈ X = a ∈ᵗ (toSet X)
  a ∉ X = a ∉ᵗ (toSet X)

open IsSet ⦃...⦄ public

infix 2 All-syntax
All-syntax : ∀ {A X} ⦃ _ : IsSet X A ⦄ → (A → Type) → X → Type
All-syntax P X = All P (toSet X)
syntax All-syntax (λ x → P) l = ∀[ x ∈ l ] P

infix 2 Ex-syntax
Ex-syntax : ∀ {A X} ⦃ _ : IsSet X A ⦄ → (A → Type) → X → Type
Ex-syntax P X = Any P (toSet X)
syntax Ex-syntax (λ x → P) l = ∃[ x ∈ l ] P

module _ ⦃ _ : IsSet X (A × B) ⦄ where
  dom : X → Set A
  dom = Rel.dom ∘ toSet

  range : X → Set B
  range = Rel.range ∘ toSet

instance
  IsSet-Set : IsSet (Set A) A
  IsSet-Set .toSet A = A

  IsSet-Map : IsSet (Map A B) (A × B)
  IsSet-Map .toSet = _ˢ

  IsSet-TotalMap : IsSet (TotalMap A B) (A × B)
  IsSet-TotalMap .toSet = TotalMap.rel
