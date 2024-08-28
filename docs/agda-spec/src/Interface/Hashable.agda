{-# OPTIONS --safe #-}
module Interface.Hashable where

open import Agda.Builtin.Equality
open import Agda.Primitive using () renaming (Set to Type)
open import Function.Definitions using (Injective)

record Hashable (T THash : Type) : Type where
  field hash    : T → THash

open Hashable ⦃...⦄ public

Hashable₁ : (Type → Type) → Type → Type₁
Hashable₁ F THash = {A : Type} → ⦃ Hashable A THash ⦄ → Hashable (F A) THash

Hashable₂ : (Type → Type → Type) → Type → Type₁
Hashable₂ F THash = {A B : Type} → ⦃ Hashable A THash ⦄ → ⦃ Hashable B THash ⦄ → Hashable (F A B) THash
