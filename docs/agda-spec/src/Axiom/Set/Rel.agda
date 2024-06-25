{-# OPTIONS --safe --no-import-sorts #-}
{-# OPTIONS -v allTactics:100 #-}

open import Prelude hiding (map)
open import Axiom.Set using (Theory)

module Axiom.Set.Rel (th : Theory {lzero}) where

import Relation.Binary.Reasoning.Setoid as SetoidReasoning
import Function.Related.Propositional as R

open Theory th
open import Axiom.Set.Properties th

import Data.Product
open import Data.List.Ext.Properties using (_⊎-cong_)
open import Data.These hiding (map)
open import Data.Maybe.Base using () renaming (map to map?)
open import Data.Product.Properties using (,-injectiveˡ; ×-≡,≡→≡)
open import Data.Product.Properties.Ext using (∃-cong′; ∃-distrib-⊎')
open import Relation.Unary using (Decidable)
open import Relation.Nullary using (yes; no)
open import Relation.Binary using (_Preserves_⟶_)
import Relation.Binary.PropositionalEquality as I

open Equivalence

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

Rel : Type → Type → Type
Rel A B = Set (A × B)

private variable A A' B B' C : Type
                 R R' : Rel A B
                 X : Set A

relatedˡ : Rel A B → Set A
relatedˡ = map proj₁

∅ʳ : Rel A B
∅ʳ = ∅

dom : Rel A B → Set A
dom = map proj₁

range : Rel A B → Set B
range = map proj₂

disjoint-dom⇒disjoint : disjoint (dom R) (dom R') → disjoint R R'
disjoint-dom⇒disjoint disj = ∈-map⁺'' -⟨ disj ⟩- ∈-map⁺''

_∣'_ : {P : A → Type} → Rel A B → specProperty P → Rel A B
m ∣' P? = filter (sp-∘ P? proj₁) m

_∣^'_ : {P : B → Type} → Rel A B → specProperty P → Rel A B
m ∣^' P? = filter (sp-∘ P? proj₂) m

impl⇒res⊆ : ∀ {X : Rel A B} {P P'} (sp-P : specProperty P) (sp-P' : specProperty P')
          → (∀ {a} → P a → P' a) → X ∣' sp-P ⊆ X ∣' sp-P'
impl⇒res⊆ sp-P sp-P' P⇒P' a∈X∣'P = ∈⇔P (Data.Product.map₁ P⇒P' (∈⇔P a∈X∣'P))

impl⇒cores⊆ : ∀ {X : Rel A B} {P P'} (sp-P : specProperty P) (sp-P' : specProperty P')
            → (∀ {b} → P b → P' b) → X ∣^' sp-P ⊆ X ∣^' sp-P'
impl⇒cores⊆ sp-P sp-P' P⇒P' a∈X∣^'P = ∈⇔P (Data.Product.map₁ P⇒P' (∈⇔P a∈X∣^'P))

mapˡ : (A → A') → Rel A B → Rel A' B
mapˡ f R = map (Data.Product.map₁ f) R

mapʳ : (B → B') → Rel A B → Rel A B'
mapʳ f R = map (Data.Product.map₂ f) R

dom∈ : ∀ {a} → (∃[ b ] (a , b) ∈ R) ⇔ a ∈ dom R
dom∈ {R = R} {a} =
  (∃[ b ] (a , b) ∈ R)            ∼⟨ R.SK-sym (mk⇔ (λ { ((_ , y) , refl , ay∈R) → y , ay∈R })
                                              (λ (x , ax∈R) → (a , x) , refl , ax∈R)) ⟩
  (∃[ a₁ ] a ≡ proj₁ a₁ × a₁ ∈ R) ∼⟨ ∈-map ⟩

  a ∈ dom R                       ∎
  where open R.EquationalReasoning

module _ {x : A} {y : B} where
  module _ {a : A} where
    ∈-dom-singleton-pair : a ≡ x ⇔ a ∈ dom ❴ x , y ❵
    ∈-dom-singleton-pair = mk⇔ (λ a≡x → to dom∈ (y , to ∈-singleton (×-≡,≡→≡ (a≡x , refl))))
                               (,-injectiveˡ ∘ from ∈-singleton ∘ proj₂ ∘ from dom∈)

    dom-single→single : a ∈ dom ❴ x , y ❵ → a ∈ ❴ x ❵
    dom-single→single = to ∈-singleton ∘ from ∈-dom-singleton-pair

    single→dom-single : a ∈ ❴ x ❵ → a ∈ dom ❴ x , y ❵
    single→dom-single = to ∈-dom-singleton-pair ∘ from ∈-singleton

  dom-single≡single : dom ❴ x , y ❵ ≡ᵉ ❴ x ❵
  dom-single≡single = dom-single→single , single→dom-single

∈-dom : {a : A × B} → a ∈ R → proj₁ a ∈ dom R
∈-dom {a = a} a∈ = to ∈-map (a , (refl , a∈))

∉-dom∅ : {a : A} → a ∉ dom{A}{B} ∅
∉-dom∅ {a} a∈dom∅ = ⊥-elim $ ∉-∅ $ proj₂ $ (from dom∈) a∈dom∅

dom∅ : dom{A}{B} ∅ ≡ᵉ ∅
dom∅ = ⊥-elim ∘ ∉-dom∅ , ∅-minimum (dom ∅)

dom∪ : dom (R ∪ R') ≡ᵉ dom R ∪ dom R'
dom∪ {R = R} {R'} = from ≡ᵉ⇔≡ᵉ' λ a →
  a ∈ dom (R ∪ R')                           ∼⟨ R.SK-sym dom∈ ⟩
  (∃[ b ] (a , b) ∈ R ∪ R')                  ∼⟨ ∃-cong′ (R.SK-sym ∈-∪) ⟩
  (∃[ b ] ((a , b) ∈ R ⊎ (a , b) ∈ R'))      ↔⟨ ∃-distrib-⊎' ⟩
  (∃[ b ] (a , b) ∈ R ⊎ ∃[ b ] (a , b) ∈ R') ∼⟨ dom∈ ⊎-cong dom∈ ⟩
  (a ∈ dom R ⊎ a ∈ dom R')                   ∼⟨ ∈-∪ ⟩
  a ∈ dom R ∪ dom R'                         ∎
  where open R.EquationalReasoning

dom⊆ : dom{A}{B} Preserves _⊆_ ⟶ _⊆_
dom⊆ R⊆R' a∈ = to dom∈ $ proj₁ (from dom∈ a∈) , R⊆R' (proj₂ (from dom∈ a∈))

dom-cong : R ≡ᵉ R' → dom R ≡ᵉ dom R'
dom-cong RR' = (dom⊆ (proj₁ RR')) , (dom⊆ (proj₂ RR'))

dom-⊆mapʳ : {f : B → B'} → dom R ⊆ dom (mapʳ f R)
dom-⊆mapʳ {f = f} {a} a∈domR with from dom∈ a∈domR
... | b , ab∈R = to dom∈ (f b , to ∈-map ((a , b) , refl , ab∈R))

dom-mapʳ⊆ : {f : B → B'} → dom (mapʳ f R) ⊆ dom R
dom-mapʳ⊆ a∈dmR with from dom∈ a∈dmR
... | _ , p∈map with from ∈-map p∈map
... | (_ , b) , refl , ab∈R = to dom∈ (b , ab∈R)

mapʳ-dom : {f : B → B'} → dom R ≡ᵉ dom (mapʳ f R)
mapʳ-dom = dom-⊆mapʳ , dom-mapʳ⊆

dom-∅ : dom R ⊆ ∅ → R ≡ᵉ ∅
dom-∅ dom⊆∅ = ∅-least (λ {x} x∈R → ⊥-elim $ ∉-∅ $ dom⊆∅ $ to dom∈ (-, x∈R))

mapPartialLiftKey : (A → B → Maybe B') → A × B → Maybe (A × B')
mapPartialLiftKey f (k , v) = map? (k ,_) (f k v)

mapPartialLiftKey-map : ∀ {a : A} {b' : B'} {f : A → B → Maybe B'} {r : Rel A B}
  → just (a , b') ∈ map (mapPartialLiftKey f) r
  → ∃[ b ] just b' ≡ f a b × (a , b) ∈ r
mapPartialLiftKey-map {f = f} ab∈m
  with from ∈-map ab∈m
... | (a' , b') , ≡ , a'b'∈r
  with f a' b' in eq
mapPartialLiftKey-map {f = f} ab∈m | (a' , b') , refl , a'b'∈r | just x
  = b' , sym eq , a'b'∈r

mapMaybeWithKey : (A → B → Maybe B') → Rel A B → Rel A B'
mapMaybeWithKey f r = mapPartial (mapPartialLiftKey f) r

∈-mapMaybeWithKey : ∀ {a : A} {b' : B'} {f : A → B → Maybe B'} {r : Rel A B}
  → (a , b') ∈ mapMaybeWithKey f r
  → ∃[ b ] (just b' ≡ f a b × (a , b) ∈ r)
∈-mapMaybeWithKey {a = a} {b'} {f} ab'∈
  = mapPartialLiftKey-map {f = f}
  $ ⊆-mapPartial
  $ to (∈-map {f = just}) ((a , b') , refl , ab'∈)

module Restriction (sp-∈ : spec-∈ A) where

  _∣_ : Rel A B → Set A → Rel A B
  m ∣ X = m ∣' sp-∈ {X}

  _∣_ᶜ : Rel A B → Set A → Rel A B
  m ∣ X ᶜ = m ∣' sp-¬ (sp-∈ {X})

  _⟪$⟫_ : Rel A B → Set A → Set B
  m ⟪$⟫ X = range (m ∣ X)

  res-cong : (R ∣_) Preserves _≡ᵉ_ ⟶ _≡ᵉ_
  res-cong (X⊆Y , Y⊆X) = (λ ∈R∣X → ∈⇔P (Data.Product.map₁ X⊆Y (∈⇔P ∈R∣X)))
                       , (λ ∈R∣Y → ∈⇔P (Data.Product.map₁ Y⊆X (∈⇔P ∈R∣Y)))

  res-dom : dom (R ∣ X) ⊆ X
  res-dom a∈dom with ∈⇔P a∈dom
  ... | _ , refl , h = proj₁ $ ∈⇔P h

  res-domᵐ : dom (R ∣ X) ⊆ dom R
  res-domᵐ a∈dom with ∈⇔P a∈dom
  ... | _ , refl , h = ∈-map⁺'' $ proj₂ (∈⇔P h)

  res-comp-cong : (R ∣_ᶜ) Preserves _≡ᵉ_ ⟶ _≡ᵉ_
  res-comp-cong (X⊆Y , Y⊆X) = (λ ∈R∣X → ∈⇔P (Data.Product.map₁ (_∘ Y⊆X) (∈⇔P ∈R∣X)))
                            , (λ ∈R∣Y → ∈⇔P (Data.Product.map₁ (_∘ X⊆Y) (∈⇔P ∈R∣Y)))

  res-comp-dom : ∀ {a} → a ∈ dom (R ∣ X ᶜ) → a ∉ X
  res-comp-dom a∈dom with ∈⇔P a∈dom
  ... | _ , refl , h = proj₁ $ ∈⇔P h


  res-comp-domᵐ : dom (R ∣ X ᶜ) ⊆ dom R
  res-comp-domᵐ a∈dom with ∈⇔P a∈dom
  ... | _ , refl , h = ∈-map⁺'' (proj₂ (∈⇔P h))

  res-⊆ : (R ∣ X) ⊆ R
  res-⊆ = proj₂ ∘′ ∈⇔P

  ex-⊆ : (R ∣ X ᶜ) ⊆ R
  ex-⊆ = proj₂ ∘′ ∈⇔P

  res-∅ : R ∣ ∅ ≡ᵉ ∅
  res-∅ = dom-∅ res-dom

  res-∅ᶜ : R ∣ ∅ ᶜ ≡ᵉ R
  res-∅ᶜ = ex-⊆ , λ a∈R → ∈⇔P (∉-∅ , a∈R)

  ∈-resᶜ-dom⁻ : ∀ {a} → a ∈ dom (R ∣ X ᶜ) → a ∉ X × ∃[ b ] (a , b) ∈ R
  ∈-resᶜ-dom⁻ a∈ = res-comp-dom a∈ , from dom∈ (dom⊆ ex-⊆ a∈)

  ∈-resᶜ-dom⁺ : ∀ {a} → a ∉ X × ∃[ b ] (a , b) ∈ R → a ∈ dom (R ∣ X ᶜ)
  ∈-resᶜ-dom⁺ (a∉X , (b , ab∈R)) = to dom∈ (b , (∈⇔P (a∉X , ab∈R)))

  ∈-resᶜ-dom : ∀ {a} → a ∈ dom (R ∣ X ᶜ) ⇔ (a ∉ X × ∃[ b ] (a , b) ∈ R)
  ∈-resᶜ-dom = mk⇔ ∈-resᶜ-dom⁻ ∈-resᶜ-dom⁺

  res-ex-∪ : Decidable (_∈ X) → (R ∣ X) ∪ (R ∣ X ᶜ) ≡ᵉ R
  res-ex-∪ ∈X? = ∪-⊆ res-⊆ ex-⊆ , λ {a} h → case ∈X? (proj₁ a) of λ where
    (yes p) → ∈⇔P (inj₁ (∈⇔P (p , h)))
    (no ¬p) → ∈⇔P (inj₂ (∈⇔P (¬p , h)))

  res-ex-disjoint : disjoint (dom (R ∣ X)) (dom (R ∣ X ᶜ))
  res-ex-disjoint h h' = res-comp-dom h' (res-dom h)

  res-ex-disj-∪ : Decidable (_∈ X) → R ≡ (R ∣ X) ⨿ (R ∣ X ᶜ)
  res-ex-disj-∪ ∈X? = IsEquivalence.sym ≡ᵉ-isEquivalence (res-ex-∪ ∈X?)
                    , disjoint-dom⇒disjoint res-ex-disjoint
    where open import Relation.Binary using (IsEquivalence)

  curryʳ : Rel (A × B) C → A → Rel B C
  curryʳ R a = mapˡ proj₂ (R ∣' (sp-∘ (sp-∈ {X = ❴ a ❵}) proj₁))

  ∈-curryʳ : ∀ {a} {b : B} {c : C} → (b , c) ∈ curryʳ R a → ((a , b) , c) ∈ R
  ∈-curryʳ h = case ∈⇔P h of λ where
    (((a , b) , c) , refl , h'') → case ∈⇔P h'' of λ where
      (p , p') → case from ∈-singleton p of λ where refl → p'

  open Intersection sp-∈
  open Intersectionᵖ sp-∈

  res-dom-comm⊆∩ : {m : Rel A B} {m' : Rel A C} → dom (m ∣ dom m') ⊆ dom m ∩ dom m'
  res-dom-comm⊆∩ x = to ∈-∩ (res-domᵐ x , res-dom x)

  res-dom-comm∩⊆ : {m : Rel A B} {m' : Rel A C} → dom m ∩ dom m' ⊆ dom (m ∣ dom m')
  res-dom-comm∩⊆ {m = m} {m' = m'} x with from ∈-∩ x
  ... | a∈dm , a∈dm' with from dom∈ a∈dm | from dom∈ a∈dm'
  ... | b , ab∈m | c , ac∈m = to dom∈ (b , to ∈-filter (a∈dm' , ab∈m))

  res-dom-comm' : {m : Rel A B} {m' : Rel A C} → dom (m ∣ dom m') ≡ᵉ dom m ∩ dom m'
  res-dom-comm' = res-dom-comm⊆∩ , res-dom-comm∩⊆

  res-dom-comm : {m : Rel A B} {m' : Rel A C} → dom (m ∣ dom m') ≡ᵉ dom (m' ∣ dom m)
  res-dom-comm {m = m} {m'} = begin
    dom (m ∣ dom m') ≈⟨ res-dom-comm' ⟩
    dom m ∩ dom m'   ≈˘⟨ ∩-sym ⟩
    dom m' ∩ dom m   ≈˘⟨ res-dom-comm' ⟩
    dom (m' ∣ dom m) ∎
    where open SetoidReasoning ≡ᵉ-Setoid

module Corestriction (sp-∈ : spec-∈ B) where

  _∣^_ : Rel A B → Set B → Rel A B
  m ∣^ X = m ∣^' sp-∈ {X}

  _∣^_ᶜ : Rel A B → Set B → Rel A B
  m ∣^ X ᶜ = m ∣^' sp-¬ (sp-∈ {X})

  cores-⊆ : (R ∣^ X) ⊆ R
  cores-⊆ = proj₂ ∘′ ∈⇔P

  coex-⊆ : (R ∣^ X ᶜ) ⊆ R
  coex-⊆ = proj₂ ∘′ ∈⇔P
