{-# OPTIONS --safe --no-import-sorts #-}

open import Axiom.Set using (Theory)

module Axiom.Set.Properties {ℓ} (th : Theory {ℓ}) where

open import Prelude hiding (isEquivalence; trans; map; map₂)
open Theory th

import Data.List
import Data.Sum
import Function.Related.Propositional as R
import Relation.Nullary.Decidable
open import Data.List.Ext.Properties using (_×-cong_; _⊎-cong_)
open import Data.List.Membership.DecPropositional using () renaming (_∈?_ to _∈ˡ?_)
open import Data.List.Membership.Propositional.Properties using (∈-filter⁺; ∈-filter⁻; ∈-++⁺ˡ; ∈-++⁺ʳ; ∈-++⁻)
open import Data.List.Relation.Binary.BagAndSetEquality using (∼bag⇒↭)
open import Data.List.Relation.Binary.Permutation.Propositional.Properties using (↭-length)
open import Data.List.Relation.Binary.Subset.Propositional using () renaming (_⊆_ to _⊆ˡ_)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.List.Relation.Unary.Unique.Propositional.Properties.WithK using (unique∧set⇒bag)
open import Data.Product using (map₂; swap)
open import Data.Product.Properties.Ext
open import Data.Relation.Nullary.Decidable.Ext using (map′⇔)
open import Relation.Binary hiding (_⇔_)
open import Relation.Binary.Lattice
import Relation.Binary.Lattice.Properties.BoundedJoinSemilattice as Bounded∨Semilattice
import Relation.Binary.Lattice.Properties.JoinSemilattice as ∨Semilattice
open import Relation.Binary.Morphism using (IsOrderHomomorphism)

open Equivalence

private variable
  A B C : Type ℓ
  X Y Z : Set A

module _ {f : A → B} {X} {b} where
  ∈-map⁻' : b ∈ map f X → (∃[ a ] b ≡ f a × a ∈ X)
  ∈-map⁻' = from ∈-map

  ∈-map⁺' : (∃[ a ] b ≡ f a × a ∈ X) → b ∈ map f X
  ∈-map⁺' = to ∈-map

∈-map⁺'' : ∀ {f : A → B} {X} {a} → a ∈ X → f a ∈ map f X
∈-map⁺'' h = to ∈-map (-, refl , h)

module _ {X : Set A} {P : A → Type} {sp-P : specProperty P} {a} where
  ∈-filter⁻' : a ∈ filter sp-P X → (P a × a ∈ X)
  ∈-filter⁻' = from ∈-filter

  ∈-filter⁺' : (P a × a ∈ X) → a ∈ filter sp-P X
  ∈-filter⁺' = to ∈-filter

module _ {X Y : Set A} {a} where
  ∈-∪⁻ : a ∈ X ∪ Y → a ∈ X ⊎ a ∈ Y
  ∈-∪⁻ = from ∈-∪

  ∈-∪⁺ : a ∈ X ⊎ a ∈ Y → a ∈ X ∪ Y
  ∈-∪⁺ = to ∈-∪

module _ {l : List A} {a} where
  ∈-fromList⁻ : a ∈ fromList l → a ∈ˡ l
  ∈-fromList⁻ = from ∈-fromList

  ∈-fromList⁺ : a ∈ˡ l → a ∈ fromList l
  ∈-fromList⁺ = to ∈-fromList

open import Tactic.AnyOf
open import Tactic.Defaults

-- Because of missing macro hygiene, we have to copy&paste this.
-- c.f. https://github.com/agda/agda/issues/3819
private macro
  ∈⇒P = anyOfⁿᵗ
    (quote ∈-filter⁻' ∷ quote ∈-∪⁻ ∷ quote ∈-map⁻' ∷ quote ∈-fromList⁻ ∷ [])
  P⇒∈ = anyOfⁿᵗ
    (quote ∈-filter⁺' ∷ quote ∈-∪⁺ ∷ quote ∈-map⁺' ∷ quote ∈-fromList⁺ ∷ [])
  ∈⇔P = anyOfⁿᵗ
    ( quote ∈-filter⁻' ∷ quote ∈-∪⁻ ∷ quote ∈-map⁻' ∷ quote ∈-fromList⁻
    ∷ quote ∈-filter⁺' ∷ quote ∈-∪⁺ ∷ quote ∈-map⁺' ∷ quote ∈-fromList⁺ ∷ [])

_≡_⨿_ : Set A → Set A → Set A → Type ℓ
X ≡ Y ⨿ Z = X ≡ᵉ Y ∪ Z × disjoint Y Z

-- FIXME: proving this has some weird issues when making a implicit in
-- in the definiton of _≡ᵉ'_
≡ᵉ⇔≡ᵉ' : X ≡ᵉ Y ⇔ X ≡ᵉ' Y
≡ᵉ⇔≡ᵉ' = mk⇔
  (λ where (X⊆Y , Y⊆X) _ → mk⇔ X⊆Y Y⊆X)
  (λ a∈X⇔a∈Y → (λ {_} → to (a∈X⇔a∈Y _)) , λ {_} → from (a∈X⇔a∈Y _))

cong-⊆⇒cong : {f : Set A → Set B} → f Preserves _⊆_ ⟶ _⊆_ → f Preserves _≡ᵉ_ ⟶ _≡ᵉ_
cong-⊆⇒cong h X≡ᵉX' = h (proj₁ X≡ᵉX') , h (proj₂ X≡ᵉX')

cong-⊆⇒cong₂ : {f : Set A → Set B → Set C}
             → f Preserves₂ _⊆_ ⟶ _⊆_ ⟶ _⊆_ → f Preserves₂ _≡ᵉ_ ⟶ _≡ᵉ_ ⟶ _≡ᵉ_
cong-⊆⇒cong₂ h X≡ᵉX' Y≡ᵉY' = h (proj₁ X≡ᵉX') (proj₁ Y≡ᵉY')
                           , h (proj₂ X≡ᵉX') (proj₂ Y≡ᵉY')

⊆-Transitive : Transitive (_⊆_ {A})
⊆-Transitive X⊆Y Y⊆Z = Y⊆Z ∘ X⊆Y

≡ᵉ-isEquivalence : IsEquivalence (_≡ᵉ_ {A})
≡ᵉ-isEquivalence = record
  { refl  = id , id
  ; sym   = λ where (h , h') → (h' , h)
  ; trans = λ eq₁ eq₂ → ⊆-Transitive (proj₁ eq₁) (proj₁ eq₂)
                      , ⊆-Transitive (proj₂ eq₂) (proj₂ eq₁)
  }

≡ᵉ-Setoid : ∀ {A} → Setoid ℓ ℓ
≡ᵉ-Setoid {A} = record
  { Carrier       = Set A
  ; _≈_           = _≡ᵉ_
  ; isEquivalence = ≡ᵉ-isEquivalence
  }

⊆-isPreorder : IsPreorder (_≡ᵉ_ {A}) _⊆_
⊆-isPreorder = λ where
  .isEquivalence → ≡ᵉ-isEquivalence
  .reflexive     → proj₁
  .trans         → ⊆-Transitive
    where open IsPreorder

⊆-Preorder : {A} → Preorder _ _ _
⊆-Preorder {A} = record
  { Carrier = Set A ; _≈_ = _≡ᵉ_ ; _≲_ = _⊆_ ; isPreorder = ⊆-isPreorder }

⊆-PartialOrder : IsPartialOrder (_≡ᵉ_ {A}) _⊆_
⊆-PartialOrder = record
  { isPreorder = ⊆-isPreorder
  ; antisym    = _,_ }

∈-× : {a : A} {b : B} → (a , b) ∈ X → (a ∈ map proj₁ X × b ∈ map proj₂ X)
∈-× {a = a} {b} x = to ∈-map ((a , b) , refl , x) , to ∈-map ((a , b) , refl , x)

module _ {f : A → B} {g : B → C} where
  map-⊆∘ : map g (map f X) ⊆ map (g ∘ f) X
  map-⊆∘ a∘∈
    with b , a≡gb , b∈prfX ← from ∈-map a∘∈
    with a , refl , a∈X    ← from ∈-map b∈prfX
    = to ∈-map (a , a≡gb , a∈X)

  map-∘⊆ : map (g ∘ f) X ⊆ map g (map f X)
  map-∘⊆ a∈∘ with from ∈-map a∈∘
  ... | a₁ , a₁≡gfa , a₁∈X = to ∈-map (f a₁ , a₁≡gfa , to ∈-map (a₁ , refl , a₁∈X))

  map-∘ : map g (map f X) ≡ᵉ map (g ∘ f) X
  map-∘ = map-⊆∘ , map-∘⊆

  ∈-map⁺-∘ : ∀ {b} → b ∈ map f X → g b ∈ map (g ∘ f) X
  ∈-map⁺-∘ = map-⊆∘ ∘ ∈-map⁺''

map-⊆ : {X Y : Set A} {f : A → B} → X ⊆ Y → map f X ⊆ map f Y
map-⊆ x⊆y a∈map with from ∈-map a∈map
... | a₁ , a≡fa₁ , a₁∈x = to ∈-map (a₁ , a≡fa₁ , x⊆y a₁∈x)

map-≡ᵉ : {X Y : Set A} {f : A → B} → X ≡ᵉ Y → map f X ≡ᵉ map f Y
map-≡ᵉ (x⊆y , y⊆x) = map-⊆ x⊆y , map-⊆ y⊆x

∉-∅ : {a : A} → a ∉ ∅
∉-∅ h = case ∈⇔P h of λ ()

∅-minimum : Minimum (_⊆_ {A}) ∅
∅-minimum = λ _ → ⊥-elim ∘ ∉-∅

∅-least : X ⊆ ∅ → X ≡ᵉ ∅
∅-least X⊆∅ = (X⊆∅ , ∅-minimum _)

∅-weakly-finite : weakly-finite {A = A} ∅
∅-weakly-finite = [] , ⊥-elim ∘ ∉-∅

∅-finite : finite {A = A} ∅
∅-finite = [] , mk⇔ (⊥-elim ∘ ∉-∅) λ ()

map-∅ : {X : Set A} {f : A → B} → map f ∅ ≡ᵉ ∅
map-∅ = ∅-least λ x∈map → case ∈-map⁻' x∈map of λ where (_ , _ , h) → ⊥-elim (∉-∅ h)

map-∪ : {X Y : Set A} → (f : A → B) → map f (X ∪ Y) ≡ᵉ map f X ∪ map f Y
map-∪ {X = X} {Y} f = from ≡ᵉ⇔≡ᵉ' λ b →
    b ∈ map f (X ∪ Y)
      ∼⟨ R.SK-sym ∈-map ⟩
    (∃[ a ] b ≡ f a × a ∈ X ∪ Y)
      ∼⟨ ∃-cong′ (R.K-refl ×-cong R.SK-sym ∈-∪) ⟩
    (∃[ a ] b ≡ f a × (a ∈ X ⊎ a ∈ Y))
      ↔⟨ ∃-cong′ ×-distribˡ-⊎' ⟩
    (∃[ a ] (b ≡ f a × a ∈ X ⊎ b ≡ f a × a ∈ Y))
      ↔⟨ ∃-distrib-⊎' ⟩
    (∃[ a ] b ≡ f a × a ∈ X ⊎ ∃[ a ] b ≡ f a × a ∈ Y)
      ∼⟨ ∈-map ⊎-cong ∈-map ⟩
    (b ∈ map f X ⊎ b ∈ map f Y)
      ∼⟨ ∈-∪ ⟩
    b ∈ map f X ∪ map f Y ∎
  where open R.EquationalReasoning

mapPartial-∅ : {f : A → Maybe B} → mapPartial f ∅ ≡ᵉ ∅
mapPartial-∅ {f = f} = ∅-least λ x∈map → case from (∈-mapPartial {f = f}) x∈map of λ where
  (_ , h , _) → ⊥-elim (∉-∅ h)

card-≡ᵉ : (X Y : Σ (Set A) strongly-finite) → proj₁ X ≡ᵉ proj₁ Y → card X ≡ card Y
card-≡ᵉ (X , lX , lXᵘ , eqX) (Y , lY , lYᵘ , eqY) X≡Y =
  ↭-length $ ∼bag⇒↭ $ unique∧set⇒bag lXᵘ lYᵘ λ {a} →
    a ∈ˡ lX  ∼⟨ R.SK-sym eqX ⟩
    a ∈ X    ∼⟨ to ≡ᵉ⇔≡ᵉ' X≡Y a ⟩
    a ∈ Y    ∼⟨ eqY ⟩
    a ∈ˡ lY  ∎
  where open R.EquationalReasoning

module _ {P : A → Type} {sp-P : specProperty P} where

  filter-∅ : (∀ a → a ∈ X → ¬ P a) → filter sp-P X ≡ᵉ ∅
  proj₁ (filter-∅ h) {a} h' with from ∈-filter h'
  ... | (Pa , a∈) = ⊥-elim (h a a∈ Pa)
  proj₂ (filter-∅ h) {a} h' = ⊥-elim (∉-∅ h')

  filter-⊆ : filter sp-P X ⊆ X
  filter-⊆ = proj₂ ∘′ ∈⇔P

  filter-pres-⊆ : X ⊆ Y → filter sp-P X ⊆ filter sp-P Y
  filter-pres-⊆ xy a∈ = let Pa∈ = from ∈-filter a∈ in
    to ∈-filter (map₂ xy Pa∈)

  filter-pres-≡ᵉ : X ≡ᵉ Y → filter sp-P X ≡ᵉ filter sp-P Y
  filter-pres-≡ᵉ (X⊆Y , Y⊆X) = filter-pres-⊆ X⊆Y , filter-pres-⊆ Y⊆X

  filter-split-∪ : ∀ {a} → a ∈ filter sp-P (X ∪ Y) → (P a × a ∈ X) ⊎ (P a × a ∈ Y)
  filter-split-∪ a∈ = case (proj₁ (from ∈-filter a∈) , from ∈-∪ (proj₂ (from ∈-filter a∈))) of
    λ where
      (Pa , inj₁ a∈X) → inj₁ (Pa , a∈X)
      (Pa , inj₂ a∈Y) → inj₂ (Pa , a∈Y)

  filter-hom-⊆ : filter sp-P (X ∪ Y) ⊆ filter sp-P X ∪ filter sp-P Y
  filter-hom-⊆ {a = a} a∈ = to ∈-∪ (case filter-split-∪ a∈ of λ where
    (inj₁ v) → inj₁ (to ∈-filter v)
    (inj₂ v) → inj₂ (to ∈-filter v))

  filter-hom-⊇ : filter sp-P X ∪ filter sp-P Y ⊆ filter sp-P (X ∪ Y)
  filter-hom-⊇ a∈ = to ∈-filter (case (from ∈-∪ a∈) of λ where
       (inj₁ v) → proj₁ (from ∈-filter v) , to ∈-∪ (inj₁ (proj₂ (from ∈-filter v)))
       (inj₂ v) → proj₁ (from ∈-filter v) , to ∈-∪ (inj₂ (proj₂ (from ∈-filter v))) )

  filter-hom-∪ : filter sp-P (X ∪ Y) ≡ᵉ (filter sp-P X) ∪ (filter sp-P Y)
  filter-hom-∪ = filter-hom-⊆ , filter-hom-⊇

Dec-∈-fromList : ∀ {a : A} → ⦃ DecEq A ⦄ → (l : List A) → Decidable¹ (_∈ fromList l)
Dec-∈-fromList _ _ = Relation.Nullary.Decidable.map ∈-fromList (_∈ˡ?_ _≟_ _ _)

Dec-∈-singleton : ∀ {a : A} → ⦃ DecEq A ⦄ → Decidable¹ (_∈ ❴ a ❵)
Dec-∈-singleton _ = Relation.Nullary.Decidable.map ∈-singleton (_ ≟ _)

singleton-finite : ∀ {a : A} → finite ❴ a ❵
singleton-finite {a = a} = [ a ] , λ {x} →
  x ∈ ❴ a ❵  ∼⟨ R.SK-sym ∈-fromList ⟩
  x ∈ˡ [ a ] ∎
  where open R.EquationalReasoning

filter-finite : ∀ {P : A → Type}
              → (sp : specProperty P) → Decidable¹ P → finite X → finite (filter sp X)
filter-finite {X = X} {P} sp P? (l , hl) = Data.List.filter P? l , λ {a} →
  a ∈ filter sp X            ∼⟨ R.SK-sym ∈-filter ⟩
  (P a × a ∈ X)              ∼⟨ R.K-refl ×-cong hl ⟩
  (P a × a ∈ˡ l)             ∼⟨ mk⇔ (uncurry $ flip $ ∈-filter⁺ P?)
                                    (swap ∘ ∈-filter⁻ P?) ⟩
  a ∈ˡ Data.List.filter P? l ∎
  where open R.EquationalReasoning

∪-⊆ˡ : X ⊆ X ∪ Y
∪-⊆ˡ = ∈⇔P ∘′ inj₁

∪-⊆ʳ : Y ⊆ X ∪ Y
∪-⊆ʳ = ∈⇔P ∘′ inj₂

∪-⊆ : X ⊆ Z → Y ⊆ Z → X ∪ Y ⊆ Z
∪-⊆ X⊆Z Y⊆Z = λ a∈X∪Y → [ X⊆Z , Y⊆Z ]′ (∈⇔P a∈X∪Y)

⊆→∪ : X ⊆ Y → X ∪ Y ≡ᵉ Y
⊆→∪ X⊆Y = (λ {a} x → case from ∈-∪ x of λ where
            (inj₁ v) → X⊆Y v
            (inj₂ v) → v) , ∪-⊆ʳ

∪-Supremum : Supremum (_⊆_ {A}) _∪_
∪-Supremum _ _ = ∪-⊆ˡ , ∪-⊆ʳ , λ _ → ∪-⊆

∪-cong-⊆ : _∪_ {A} Preserves₂ _⊆_ ⟶ _⊆_ ⟶ _⊆_
∪-cong-⊆ X⊆X' Y⊆Y' = ∈⇔P ∘′ (Data.Sum.map X⊆X' Y⊆Y') ∘′ ∈⇔P

∪-cong : _∪_ {A} Preserves₂ _≡ᵉ_ ⟶ _≡ᵉ_ ⟶ _≡ᵉ_
∪-cong = cong-⊆⇒cong₂ ∪-cong-⊆

∪-preserves-finite : _∪_ {A} Preservesˢ₂ finite
∪-preserves-finite {a = X} {Y} (l , hX) (l' , hY) = (l ++ l') , λ {a} →
  a ∈ X ∪ Y          ∼⟨ R.SK-sym ∈-∪ ⟩
  (a ∈ X ⊎ a ∈ Y)    ∼⟨ hX ⊎-cong hY ⟩
  (a ∈ˡ l ⊎ a ∈ˡ l') ∼⟨ mk⇔ Data.Sum.[ ∈-++⁺ˡ , ∈-++⁺ʳ _ ] (∈-++⁻ _) ⟩
  a ∈ˡ l ++ l'       ∎
  where open R.EquationalReasoning

∪-sym : X ∪ Y ≡ᵉ Y ∪ X
∪-sym = ∪-⊆ ∪-⊆ʳ ∪-⊆ˡ , ∪-⊆ ∪-⊆ʳ ∪-⊆ˡ

Set-JoinSemilattice : IsJoinSemilattice (_≡ᵉ_ {A}) _⊆_ _∪_
Set-JoinSemilattice = record
  { isPartialOrder = ⊆-PartialOrder ; supremum = ∪-Supremum }

Set-BoundedJoinSemilattice : IsBoundedJoinSemilattice (_≡ᵉ_ {A}) _⊆_ _∪_ ∅
Set-BoundedJoinSemilattice = record
  { isJoinSemilattice = Set-JoinSemilattice ; minimum = ∅-minimum }

Set-BddSemilattice : {A : Type ℓ} → BoundedJoinSemilattice _ _ _
Set-BddSemilattice {A} = record
                      { Carrier = Set A
                      ; _≈_ = _≡ᵉ_ {A}
                      ; _≤_ = _⊆_
                      ; _∨_ = _∪_
                      ; ⊥ = ∅
                      ; isBoundedJoinSemilattice = Set-BoundedJoinSemilattice
                      }

module _ {A : Type ℓ} where
  open Bounded∨Semilattice (Set-BddSemilattice {A})
  open ∨Semilattice (BoundedJoinSemilattice.joinSemilattice (Set-BddSemilattice {A}))
    using (∨-comm; ∨-assoc)

  ∪-identityˡ : (X : Set A) → ∅ ∪ X ≡ᵉ X
  ∪-identityˡ = identityˡ

  ∪-identityʳ : (X : Set A) → X ∪ ∅ ≡ᵉ X
  ∪-identityʳ = identityʳ

  ∪-comm : (X Y : Set A) → X ∪ Y ≡ᵉ Y ∪ X
  ∪-comm = ∨-comm

  ∪-assoc : (X Y Z : Set A) → (X ∪ Y) ∪ Z ≡ᵉ X ∪ (Y ∪ Z)
  ∪-assoc = ∨-assoc

fromList-∪-singleton : {A : Type ℓ} {x : A} {l : List A} → fromList (x ∷ l) ≡ᵉ ❴ x ❵ ∪ fromList l
fromList-∪-singleton .proj₁ h with from ∈-fromList h
... | here refl = ∈-∪⁺ (inj₁ (to ∈-fromList (here refl)))
... | there q = ∈-∪⁺ (inj₂ (to ∈-fromList q))
fromList-∪-singleton .proj₂ h with ∈-∪⁻ h
... | (inj₁ a∈) = to ∈-fromList (here (from ∈-singleton a∈))
... | (inj₂ a∈) = to ∈-fromList (there (from ∈-fromList a∈))

∪-fromList-++ : (ll lr : List A) → fromList ll ∪ fromList lr ≡ᵉ fromList (ll ++ lr)
∪-fromList-++ [] lr = ∪-identityˡ (fromList lr)
∪-fromList-++ (x ∷ l) lr =
  begin
  fromList (x ∷ l) ∪ fromList lr      ≈⟨ ∪-cong fromList-∪-singleton ≡ᵉ.refl ⟩
  (❴ x ❵ ∪ fromList l) ∪ fromList lr  ≈⟨ ∪-assoc ❴ x ❵ (fromList l) (fromList lr) ⟩
  ❴ x ❵ ∪ (fromList l ∪ fromList lr)  ≈⟨ ∪-cong ≡ᵉ.refl (∪-fromList-++ l lr) ⟩
  ❴ x ❵ ∪ fromList (l ++ lr)          ≈˘⟨ fromList-∪-singleton ⟩
  fromList (x ∷ (l ++ lr))            ∎
  where
  module ≡ᵉ = IsEquivalence (≡ᵉ-isEquivalence)
  open import Relation.Binary.Reasoning.Setoid ≡ᵉ-Setoid

disjoint-sym : disjoint X Y → disjoint Y X
disjoint-sym disj = flip disj

module Intersectionᵖ (sp-∈ : spec-∈ A) where
  open Intersection sp-∈

  disjoint⇒disjoint' : disjoint X Y → disjoint' X Y
  disjoint⇒disjoint' h = ∅-least (⊥-elim ∘ uncurry h ∘ from ∈-∩)

  disjoint'⇒disjoint : disjoint' X Y → disjoint X Y
  disjoint'⇒disjoint h a∈X a∈Y = ∉-∅ (to (to ≡ᵉ⇔≡ᵉ' h _) (to ∈-∩ (a∈X , a∈Y)))

  ∩-⊆ˡ : X ∩ Y ⊆ X
  ∩-⊆ˡ = proj₁ ∘ from ∈-∩

  ∩-⊆ʳ : X ∩ Y ⊆ Y
  ∩-⊆ʳ = proj₂ ∘ from ∈-∩

  ∩-⊆ : Z ⊆ X → Z ⊆ Y → Z ⊆ X ∩ Y
  ∩-⊆ Z⊆X Z⊆Y = λ x∈Z → to ∈-∩ (< Z⊆X , Z⊆Y > x∈Z)

  ∩-Infimum : Infimum _⊆_ _∩_
  ∩-Infimum X Y = ∩-⊆ˡ , ∩-⊆ʳ , λ _ → ∩-⊆

  ∩-preserves-finite : _∩_ Preservesˢ₂ weakly-finite
  ∩-preserves-finite _ = ⊆-weakly-finite ∩-⊆ʳ

  ∩-cong-⊆ : _∩_ Preserves₂ _⊆_ ⟶ _⊆_ ⟶ _⊆_
  ∩-cong-⊆ X⊆X' Y⊆Y' a∈X∩Y = to ∈-∩ (Data.Product.map X⊆X' Y⊆Y' (from ∈-∩ a∈X∩Y))

  ∩-cong : _∩_ Preserves₂ _≡ᵉ_ ⟶ _≡ᵉ_ ⟶ _≡ᵉ_
  ∩-cong = cong-⊆⇒cong₂ ∩-cong-⊆

  ∩-OrderHomomorphismʳ : ∀ {X} → IsOrderHomomorphism _≡ᵉ_ _≡ᵉ_ _⊆_ _⊆_ (X ∩_)
  ∩-OrderHomomorphismʳ = record { cong = ∩-cong (id , id) ; mono = ∩-cong-⊆ id }

  ∩-OrderHomomorphismˡ : ∀ {X} → IsOrderHomomorphism _≡ᵉ_ _≡ᵉ_ _⊆_ _⊆_ (_∩ X)
  ∩-OrderHomomorphismˡ = record
    { cong = flip ∩-cong (id , id) ; mono = flip ∩-cong-⊆ id }

  Set-Lattice : IsLattice _≡ᵉ_ _⊆_ _∪_ _∩_
  Set-Lattice = record
    { isPartialOrder = ⊆-PartialOrder ; supremum = ∪-Supremum ; infimum = ∩-Infimum }

  ∩-sym⊆ : X ∩ Y ⊆ Y ∩ X
  ∩-sym⊆ a∈X∩Y with from ∈-∩ a∈X∩Y
  ... | a∈X , a∈Y = to ∈-∩ (a∈Y , a∈X)

  ∩-sym : X ∩ Y ≡ᵉ Y ∩ X
  ∩-sym = ∩-sym⊆ , ∩-sym⊆

-- Additional properties of lists and sets.
module _ {L : List A} where
  open Equivalence

  sublist-⇔ : {l : List A} → fromList l ⊆ fromList L ⇔ l ⊆ˡ L
  sublist-⇔ {[]} = mk⇔ (λ x ()) (λ _ {_} → ⊥-elim ∘ ∉-∅)
  sublist-⇔ {x ∷ xs} = mk⇔ onlyif (λ u → to ∈-fromList ∘ u ∘ from ∈-fromList)
    where
    onlyif : ({a : A} → a ∈ fromList (x ∷ xs) → a ∈ fromList L) → x ∷ xs ⊆ˡ L
    onlyif h (here refl) = from ∈-fromList (h (to ∈-fromList (here refl)))
    onlyif h (there x'∈) = from ∈-fromList (h (to ∈-fromList (there x'∈)))

  module _ {ℓ : Level} {P : Pred (List A) ℓ} where
    ∃-sublist-⇔ : (∃[ l ] fromList l ⊆ fromList L × P l) ⇔ (∃[ l ] l ⊆ˡ L × P l)
    ∃-sublist-⇔ = mk⇔ (λ (l , l⊆L , Pl) → l , to sublist-⇔ l⊆L , Pl)
                      (λ (l , l⊆L , Pl) → l , from sublist-⇔ l⊆L , Pl)

    ∃?-sublist-⇔ : Dec (∃[ l ] fromList l ⊆ fromList L × P l) ⇔ Dec (∃[ l ] l ⊆ˡ L × P l)
    ∃?-sublist-⇔ = map′⇔ ∃-sublist-⇔


