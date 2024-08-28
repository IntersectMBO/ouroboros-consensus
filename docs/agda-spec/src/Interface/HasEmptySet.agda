{-# OPTIONS --safe #-}

module Interface.HasEmptySet where

open import Prelude using (Type)

record HasEmptySet (A : Type) : Type where
  field
    ∅ : A

open HasEmptySet ⦃...⦄ public
