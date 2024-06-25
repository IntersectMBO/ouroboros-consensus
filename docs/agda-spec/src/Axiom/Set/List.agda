{-# OPTIONS --safe --no-import-sorts #-}

module Axiom.Set.List where

open import Prelude hiding (find)

open import Axiom.Set

open import Data.List using (filter)
import Data.List.Relation.Unary.All as All
import Data.List.Relation.Unary.Any as Any
import Function
import Function.Properties.Inverse as I
import Function.Related.Propositional as R
import Relation.Nullary.Decidable as Dec
open import Data.List.Membership.Propositional using (find; lose) renaming (_∈_ to _∈ˡ_)
open import Data.List.Membership.Propositional.Properties
open import Data.Product
open import Data.Product.Algebra
open import Data.Product.Properties.Ext
open import Class.DecEq
open import Relation.Binary using () renaming (Decidable to Dec₂)
open import Relation.Nullary.Decidable

List-Model : Theory
List-Model = λ where
  .Set           → List
  ._∈_           → _∈ˡ_
  .sp            → Dec-SpecProperty
  .specification → λ X P? → filter P? X , mk⇔
    (λ where (Pa , a∈X) → ∈-filter⁺ P? a∈X Pa)
    (λ a∈f → Data.Product.swap (∈-filter⁻ P? a∈f))
  .unions        → λ X → concat X , mk⇔
    (λ where (T , T∈X , a∈T) → ∈-concat⁺′ a∈T T∈X)
    (λ a∈cX → case ∈-concat⁻′ _ a∈cX of λ where (T , a∈T , T∈X) → (T , T∈X , a∈T))
  .replacement   → λ f X → Data.List.map f X , λ {b} →
    (∃[ a ] b ≡ f a × a ∈ˡ X) ∼⟨ ∃-cong′ (I.↔⇒⇔ (×-comm _ _)) ⟩
    (∃[ a ] a ∈ˡ X × b ≡ f a) ⤖⟨ I.↔⇒⤖ (map-∈↔ f) ⟩
    b ∈ˡ Data.List.map f X    ∎
  .listing → λ l → l , mk⇔ id id
    where open Theory hiding (filter)
          open R.EquationalReasoning

List-Modelᶠ : Theoryᶠ
List-Modelᶠ = λ where
  .theory     → List-Model
  .finiteness → λ X → X , mk⇔ id id
    where open Theoryᶠ

module Decˡ {A : Type} ⦃ _ : DecEq A ⦄ where
  open Theory List-Model

  _∈?_ : Dec₂ (_∈ˡ_ {A = A})
  _∈?_ a = Any.any? (a ≟_)

  DecEq-Set : DecEq (Set A)
  DecEq-Set = DecEq-List

List-Modelᵈ : Theoryᵈ
List-Modelᵈ = record
  { th = List-Model
  ; ∈-sp = Decˡ._∈? _
  ; _∈?_ = Decˡ._∈?_
  ; all? = λ P? {X} → Dec.map (mk⇔ All.lookup All.tabulate) (All.all? P? X)
  ; any? = λ P? X → Dec.map (mk⇔ find (uncurry lose ∘ proj₂)) (Any.any? P? X) }
