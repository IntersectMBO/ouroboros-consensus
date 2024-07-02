{-# OPTIONS --safe #-}

open import Axiom.Set using (Theory)

module Interface.HasEmptySet.Instances (th : Theory) where

open import Interface.HasEmptySet
open Theory th renaming (Set to ℙ_; ∅ to ∅ˢ)
open import Axiom.Set.Map th

instance
  HasEmptySet-Set : {A : Set} → HasEmptySet (ℙ A)
  HasEmptySet-Set = record { ∅ = ∅ˢ }

  HasEmptySet-Map : {A B : Set} → HasEmptySet (Map A B)
  HasEmptySet-Map = record { ∅ = ∅ᵐ }
