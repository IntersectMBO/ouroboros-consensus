{-# OPTIONS --safe --no-import-sorts #-}

open import Axiom.Set using ( Theory )

module Axiom.Set.TotalMap (th : Theory) where

open import Prelude hiding (lookup; map)

open import Data.Product.Properties     using (Σ-≡,≡→≡)
open import Axiom.Set.Map th            using (left-unique; Map ; mapWithKey-uniq ; left-unique-mapˢ)
open import Axiom.Set.Rel th            using (Rel ; dom ; dom∈)
open import Class.DecEq                 using (DecEq ; _≟_)

open Theory th    using (_∈_ ; map ; Set ; ∈-map ; ∈-map′ ; isMaximal)
open Equivalence  using (to ; from)

private variable A B : Type


-- defines a total map for a given set
total : Rel A B → Type
total R = ∀ {a} → a ∈ dom R


record TotalMap (A B : Type) : Type where
  field  rel              : Set (A × B)
         left-unique-rel  : left-unique rel
         total-rel        : total rel

  toMap : Map A B
  toMap = rel , left-unique-rel

  lookup : A → B
  lookup _ = proj₁ (from dom∈ total-rel)

  -- verify that lookup is what we expect
  lookup∈rel : {a : A} → (a , lookup a) ∈ rel
  lookup∈rel = proj₂ (from dom∈ total-rel)

  -- this is useful for proving equalities involving lookup
  ∈-rel⇒lookup-≡ : {a : A}{b : B} → (a , b) ∈ rel → lookup a ≡ b
  ∈-rel⇒lookup-≡ ab∈rel = sym (left-unique-rel ab∈rel (proj₂ (from dom∈ total-rel)))


module Update {B : Type} ⦃ _ : DecEq A ⦄ where

  private
    updateFn : A × B → A → B → B
    updateFn (a , b) x y with (x ≟ a)
    ... | yes  _ = b
    ... | no   _ = y

  updateFn-id : {a : A}{b b' : B} → b ≡ updateFn (a , b) a b'
  updateFn-id {a = a} with (a ≟ a)
  ... | yes _ = refl
  ... | no ¬p = ⊥-elim (¬p refl)


  open TotalMap

  mapWithKey : {B' : Type} → (A → B → B') → TotalMap A B → TotalMap A B'
  mapWithKey f tm .rel              = map (λ{(x , y) → x , f x y}) (rel tm)
  mapWithKey _ tm .left-unique-rel  = mapWithKey-uniq (left-unique-rel tm)
  mapWithKey _ tm .total-rel        = ∈-map′ (∈-map′ (proj₂ (from dom∈ (total-rel tm))))

  update : A → B → TotalMap A B → TotalMap A B
  update a b = mapWithKey (updateFn (a , b))



module LookupUpdate
  {X : Set A}
  {a : A} {a∈X : a ∈ X}
  {b : B}
  ⦃ decEqA : DecEq A ⦄ where

  open TotalMap
  open Update

  ∈-rel-update  : (tm : TotalMap A B) → (a , b) ∈ rel (update a b tm)
  ∈-rel-update tm = to ∈-map ((a , lookup tm a) , Σ-≡,≡→≡ (refl , updateFn-id {A = A}) , lookup∈rel tm)

  lookup-update-id  : (tm : TotalMap A B) → lookup (update a b tm) a ≡ b
  lookup-update-id tm = ∈-rel⇒lookup-≡ (update _ _ tm) (∈-rel-update tm)



------------------------------------------------------
-- Correspondences between total maps and functions --

module FunTot (X : Set A) (⋁A≡X : isMaximal X) where
  open TotalMap

  Fun⇒Map : ∀ {A B} {f : A → B} (X : Set A) → Map A B
  Fun⇒Map {f = f} X = map (λ x → (x , f x)) X , left-unique-mapˢ X


  Fun⇒TotalMap : (f : A → B) → TotalMap A B
  Fun⇒TotalMap f .rel              = map (λ x → (x , f x)) X
  Fun⇒TotalMap _ .left-unique-rel  = left-unique-mapˢ X
  Fun⇒TotalMap _ .total-rel        = ∈-map′ (∈-map′ ⋁A≡X)

  Fun∈TotalMap  : {f : A → B}{a : A}
                → a ∈ X → (a , f a) ∈ rel (Fun⇒TotalMap f)
  Fun∈TotalMap a∈X = ∈-map′ a∈X

  lookup∘Fun⇒TotalMap-id  : {f : A → B}{a : A}
                          → lookup (Fun⇒TotalMap f) a ≡ f a
  lookup∘Fun⇒TotalMap-id {f = f} = ∈-rel⇒lookup-≡ ((Fun⇒TotalMap f)) (Fun∈TotalMap ⋁A≡X)
