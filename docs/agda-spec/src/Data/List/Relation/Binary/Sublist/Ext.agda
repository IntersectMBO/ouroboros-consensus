{-# OPTIONS --safe #-}
module Data.List.Relation.Binary.Sublist.Ext where

open import Data.List.Relation.Binary.Sublist.Propositional public
  using (_⊆_; []; _∷_; _∷ʳ_)
open import Data.List using (List; []; _∷_)

[]⊆ : ∀ {ℓ}{A : Set ℓ} {xs : List A} → [] ⊆ xs
[]⊆ {xs = []} = []
[]⊆ {xs = _ ∷ _} = _ ∷ʳ []⊆
