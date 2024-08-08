{-# OPTIONS --safe #-}

module Data.Relation.Nullary.Decidable.Ext where

open import Level using (Level)
open import Function.Bundles using (_⇔_; mk⇔; module Equivalence)
open import Relation.Nullary.Decidable.Core using (Dec; map′)

private variable
  a : Level
  A B : Set a

map′⇔ : (A ⇔ B) → (Dec A ⇔ Dec B)
map′⇔ A⇔B = mk⇔ (map′ (to A⇔B) (from A⇔B)) (map′ (from A⇔B) (to A⇔B))
  where open Equivalence
