------------------------------------------------------------------------
-- The Agda standard library
--
-- Properties of unique lists (setoid equality) using K
------------------------------------------------------------------------

{-# OPTIONS --safe --with-K #-}

module Data.List.Relation.Unary.Unique.Propositional.Properties.WithK where

open import Data.Empty
open import Data.List using (List)
open import Data.List.Membership.Propositional
open import Data.List.Relation.Binary.BagAndSetEquality
open import Data.List.Relation.Unary.All.Properties
open import Data.List.Relation.Unary.Any
open import Data.List.Relation.Unary.Unique.Propositional
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Level using (Level)
open import Function.Bundles

private
  variable
    a : Level
    A : Set a
    l l' : List A

------------------------------------------------------------------------
-- membership

unique⇒∈-prop : Unique l → (x : A) → Irrelevant (x ∈ l)
unique⇒∈-prop (hx ∷ h) x (here refl) (here refl) = refl
unique⇒∈-prop (hx ∷ h) x (here refl) (there b)   = ⊥-elim (All¬⇒¬Any hx b)
unique⇒∈-prop (hx ∷ h) x (there a)   (here refl) = ⊥-elim (All¬⇒¬Any hx a)
unique⇒∈-prop (hx ∷ h) x (there a)   (there b) rewrite unique⇒∈-prop h x a b = refl

unique∧set⇒bag : Unique l → Unique l' → l ∼[ set ] l' → l ∼[ bag ] l'
unique∧set⇒bag h h' eq {a} = record
  { to = to
  ; from = from
  ; to-cong = to-cong
  ; from-cong = from-cong
  ; inverse = (λ {x} {y} _ → unique⇒∈-prop h' a (to y) x)
            , (λ {x} {y} _ → unique⇒∈-prop h a (from y) x)
  }
  where open Equivalence (eq {a})
