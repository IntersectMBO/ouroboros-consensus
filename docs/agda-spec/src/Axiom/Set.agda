{-# OPTIONS --safe --no-import-sorts #-}

module Axiom.Set where

open import Prelude hiding (map)

import Function.Related.Propositional as R
open import Data.List.Ext.Properties using (∈-dedup; _×-cong_)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.List.Relation.Unary.Unique.DecPropositional.Properties using (deduplicate-!)
open import Data.List.Relation.Unary.Unique.Propositional using (Unique; [])
open import Data.Product.Algebra using (×-comm)
open import Data.Product.Properties using (∃∃↔∃∃)
open import Data.Product.Properties.Ext using (∃-cong′; ∃-≡)
open import Class.DecEq using (DecEq; _≟_)
open import Relation.Binary using () renaming (Decidable to Dec₂)

private variable
  ℓ : Level
  A B C : Type ℓ
  P : A → Type
  l : List A

_Preserves₁_⟶_ : {A : Type ℓ} → (A → B) → Pred A 0ℓ → Pred B 0ℓ → Type ℓ
f Preserves₁ P ⟶ Q = ∀ {a} → P a → Q (f a)

_Preserves₁₂_⟶_⟶_ : {A B : Type ℓ} → (A → B → C) → Pred A ℓ → Pred B ℓ → Pred C ℓ → Type ℓ
f Preserves₁₂ P ⟶ P' ⟶ Q = ∀ {a b} → P a → P' b → Q (f a b)

record SpecProperty {ℓ} : Type (sucˡ ℓ) where
  field specProperty : {A : Type ℓ} → (A → Type) → Type
        sp-∘ : specProperty P → (f : B → A) → specProperty (P ∘ f)
        sp-¬ : specProperty P → specProperty (¬_ ∘ P)

Dec-SpecProperty : SpecProperty
Dec-SpecProperty = record
  { specProperty = Decidable¹
  ; sp-∘         = λ P? → P? ∘_
  ; sp-¬         = λ P? → ¬? ∘ P?
  }

record Theory {ℓ} : Type (sucˡ ℓ) where
  infix 4 _⊆_ _≡ᵉ_ _∈_ _∉_
  infixr 6 _∪_

  field Set : Type ℓ → Type ℓ
        _∈_ : A → Set A → Type
        sp  : SpecProperty
  open SpecProperty sp public

  _⊆_ : Set A → Set A → Type ℓ
  X ⊆ Y = ∀ {a} → a ∈ X → a ∈ Y

  -- we might want to either have all properties or
  -- decidable properties allowed for specification
  field specification : (X : Set A)
                      → specProperty P → ∃[ Y ] ∀ {a} → (P a × a ∈ X) ⇔ a ∈ Y
        unions        : (X : Set (Set A))
                      → ∃[ Y ] ∀ {a} → (∃[ T ] (T ∈ X × a ∈ T)) ⇔ a ∈ Y
        replacement   : (f : A → B) (X : Set A)
                      → ∃[ Y ] ∀ {b} → (∃[ a ] b ≡ f a × a ∈ X) ⇔ b ∈ Y
        listing       : (l : List A)
                      → ∃[ X ] ∀ {a} → a ∈ˡ l ⇔ a ∈ X
                      -- ^ equivalent to pairing + empty set
        -- power-set     : (X : Set A) → ∃[ Y ] ∀ {T} → T ⊆ X → T ∈ Y

  private variable X X' Y : Set A

  _≡ᵉ_ : Set A → Set A → Type ℓ
  X ≡ᵉ Y = X ⊆ Y × Y ⊆ X

  _≡ᵉ'_ : Set A → Set A → Type ℓ
  X ≡ᵉ' Y = ∀ a → a ∈ X ⇔ a ∈ Y

  _∉_ : A → Set A → Type
  _∉_ = ¬_ ∘₂ _∈_

  ≡→∈ : {X : Set A}{a a' : A} → a ∈ X → a ≡ a' → a' ∈ X
  ≡→∈ a∈X refl = a∈X

  -- The following is useful in case we have `(a , p)` and `(a , q)`, where `p`
  -- and `q` are proofs of `a ∈ X`, and we want to prove `(a , p) ≡ (a , q)`.
  ∈-irrelevant : Set A → Type ℓ
  ∈-irrelevant X = ∀ {a} (p q : a ∈ X) → p ≡ q

  open Equivalence

  _Preservesˢ_ : (Set A → Set B) → (∀ {A} → Set A → Type) → Type ℓ
  f Preservesˢ P = f Preserves₁ P ⟶ P

  _Preservesˢ₂_ : (Set A → Set B → Set C) → (∀ {A : Type ℓ} → Set A → Type ℓ) → Type ℓ
  f Preservesˢ₂ P = f Preserves₁₂ P ⟶ P ⟶ P

  disjoint : Set A → Set A → Type ℓ
  disjoint X Y = ∀ {a} → a ∈ X → a ∈ Y → ⊥

  finite : Set A → Type ℓ
  finite X = ∃[ l ] ∀ {a} → a ∈ X ⇔ a ∈ˡ l

  Show-finite : ⦃ Show A ⦄ → Show (Σ (Set A) finite)
  Show.show Show-finite (X , (l , _)) = Show-List .show l

  weakly-finite : Set A → Type ℓ
  weakly-finite X = ∃[ l ] ∀ {a} → a ∈ X → a ∈ˡ l

  -- there exists a list without duplicates that has exactly the members of the set
  strongly-finite : Set A → Type ℓ
  strongly-finite X = ∃[ l ] Unique l × ∀ {a} → a ∈ X ⇔ a ∈ˡ l

  DecEq∧finite⇒strongly-finite : ⦃ _ : DecEq A ⦄ (X : Set A)
    → finite X → strongly-finite X
  DecEq∧finite⇒strongly-finite ⦃ eq? ⦄ X (l , h) = let _≟_ = eq? ._≟_ in
    deduplicate _≟_ l , deduplicate-! _≟_ l , λ {a} →
      a ∈ X                  ∼⟨ h ⟩
      a ∈ˡ l                 ∼⟨ ∈-dedup ⟩
      a ∈ˡ deduplicate _≟_ l ∎
    where open R.EquationalReasoning

  card : Σ (Set A) strongly-finite → ℕ
  card (_ , l , _) = length l

  ⊆-weakly-finite : X ⊆ Y → weakly-finite Y → weakly-finite X
  ⊆-weakly-finite X⊆Y (l , hl) = l , hl ∘ X⊆Y

  isMaximal : Set A → Type ℓ
  isMaximal {A} X = {a : A} → a ∈ X

  maximal-⊆ : isMaximal Y → X ⊆ Y
  maximal-⊆ maxY _ = maxY

  maximal-unique : isMaximal X → isMaximal Y → X ≡ᵉ Y
  maximal-unique maxX maxY = maximal-⊆ maxY , maximal-⊆ maxX

  FinSet : Type ℓ → Type ℓ
  FinSet A = Σ (Set A) finite

  -- if you can construct a set that contains all elements satisfying
  -- P, you can construct a set containing exactly the elements satisfying P
  strictify : specProperty P → (∃[ Y ] ∀ {a} → P a → a ∈ Y) → ∃[ Y ] ∀ {a} → P a ⇔ a ∈ Y
  strictify sp p with specification (proj₁ p) sp
  ... | (Y , p') = Y , (mk⇔ (λ a∈l → to p' (a∈l , proj₂ p a∈l)) (proj₁ ∘ from p'))

  map : (A → B) → Set A → Set B
  map = proj₁ ∘₂ replacement

  ∈-map : ∀ {f : A → B} {b} → (∃[ a ] b ≡ f a × a ∈ X) ⇔ b ∈ map f X
  ∈-map = proj₂ $ replacement _ _

  ∈-map′ : ∀ {f : A → B} {a} → a ∈ X → f a ∈ map f X
  ∈-map′ {a = a} a∈X = to ∈-map (a , refl , a∈X)

  -- don't know that there's a set containing all members of a type, which this is equivalent to
  -- _⁻¹_ : (A → B) → Set B → Set A
  -- f ⁻¹ X = {!!}

  filter : {P : A → Type} → specProperty P → Set A → Set A
  filter = proj₁ ∘₂ flip specification

  ∈-filter : ∀ {sp-P : specProperty P} {a} → (P a × a ∈ X) ⇔ a ∈ filter sp-P X
  ∈-filter = proj₂ $ specification _ _

  fromList : List A → Set A
  fromList = proj₁ ∘ listing

  ∈-fromList : ∀ {a} → a ∈ˡ l ⇔ a ∈ fromList l
  ∈-fromList = proj₂ $ listing _

  ∈-unions : {a : A} {U : Set (Set A)} → (∃[ T ] T ∈ U × a ∈ T) ⇔ a ∈ proj₁ (unions U)
  ∈-unions = proj₂ $ unions _

  ∅ : Set A
  ∅ = fromList []

  ∅-strongly-finite : strongly-finite {A} ∅
  ∅-strongly-finite = [] , [] , R.SK-sym ∈-fromList

  card-∅ : card (∅ {A} , ∅-strongly-finite) ≡ 0
  card-∅ = refl

  singleton : A → Set A
  singleton a = fromList [ a ]

  ❴_❵ = singleton

  ∈-singleton : {a b : A} → a ≡ b ⇔ a ∈ singleton b
  ∈-singleton {_} {a} {b} =
    a ≡ b           ∼⟨ mk⇔ (λ where refl → here refl) (λ where (here refl) → refl) ⟩
    a ∈ˡ [ b ]      ∼⟨ ∈-fromList ⟩
    a ∈ singleton b ∎
    where open R.EquationalReasoning

  partialToSet : (A → Maybe B) → A → Set B
  partialToSet f a = maybe (fromList ∘ [_]) ∅ (f a)

  ∈-partialToSet : ∀ {a : A} {b : B} {f} → f a ≡ just b ⇔ b ∈ partialToSet f a
  ∈-partialToSet {a = a} {b} {f} = mk⇔
    (λ h → subst (λ x → b ∈ maybe (fromList ∘ [_]) ∅ x) (sym h) (to ∈-singleton refl))
    (case f a returning (λ y → b ∈ maybe (λ x → fromList [ x ]) ∅ y → y ≡ just b) of
      λ where (just x) → λ h → cong just (sym $ from ∈-singleton h)
              nothing  → λ h → case from ∈-fromList h of λ ())

  concatMapˢ : (A → Set B) → Set A → Set B
  concatMapˢ f a = proj₁ $ unions (map f a)

  ∈-concatMapˢ : {y : B} {f : A → Set B}
    → (∃[ x ] x ∈ X × y ∈ f x) ⇔ y ∈ concatMapˢ f X
  ∈-concatMapˢ {X = X} {y} {f} =
    (∃[ x ] x ∈ X × y ∈ f x)
      ∼⟨ ∃-cong′ (λ {x} → ∃-≡ (λ T → x ∈ X × y ∈ T)) ⟩
    (∃[ x ] ∃[ T ] T ≡ f x × x ∈ X × y ∈ T)
      ↔⟨ ∃∃↔∃∃ (λ x T → T ≡ f x × x ∈ X × y ∈ T) ⟩
    (∃[ T ] ∃[ x ] T ≡ f x × x ∈ X × y ∈ T)
      ∼⟨ ∃-cong′ $ mk⇔
        (λ where (x , p₁ , p₂ , p₃) → (x , p₁ , p₂) , p₃)
        (λ where ((x , p₁ , p₂) , p₃) → x , p₁ , p₂ , p₃) ⟩
    (∃[ T ] (∃[ x ] T ≡ f x × x ∈ X) × y ∈ T)
      ∼⟨ ∃-cong′ (∈-map ×-cong R.K-refl) ⟩
    (∃[ T ] T ∈ map f X × y ∈ T)
      ∼⟨ ∈-unions ⟩
    y ∈ concatMapˢ f X ∎
    where open R.EquationalReasoning

  mapPartial : (A → Maybe B) → Set A → Set B
  mapPartial f = concatMapˢ (partialToSet f)

  ∈-mapPartial : {y : B} {f : A → Maybe B}
    → (∃[ x ] x ∈ X × f x ≡ just y) ⇔ y ∈ mapPartial f X
  ∈-mapPartial {X = X} {y} {f} =
    (∃[ x ] x ∈ X × f x ≡ just y)
      ∼⟨ ∃-cong′ (R.K-refl ×-cong (∈-partialToSet {f = f})) ⟩
    (∃[ x ] x ∈ X × y ∈ partialToSet f x)
      ∼⟨ ∈-concatMapˢ ⟩
    y ∈ mapPartial f X ∎
    where open R.EquationalReasoning

  ⊆-mapPartial : ∀ {f : A → Maybe B} → map just (mapPartial f X) ⊆ map f X
  ⊆-mapPartial {f = f} a∈m with from ∈-map a∈m
  ... | x , refl , a∈mp with from (∈-mapPartial {f = f}) a∈mp
  ... | x' , x'∈X , jx≡fx = to ∈-map (x' , sym jx≡fx , x'∈X)

  binary-unions : ∃[ Y ] ∀ {a} → (a ∈ X ⊎ a ∈ X') ⇔ a ∈ Y
  binary-unions {X = X} {X'} with unions (fromList (X ∷ [ X' ]))
  ... | (Y , h) = Y , mk⇔ (λ where
    (inj₁ a∈X)  → to h (X  , to ∈-fromList (here refl)         , a∈X)
    (inj₂ a∈X') → to h (X' , to ∈-fromList (there (here refl)) , a∈X'))
    (λ a∈Y → case from h a∈Y of λ (T , H , a∈T) → case from ∈-fromList H of λ where
      (here refl) → inj₁ a∈T
      (there (here refl)) → inj₂ a∈T)

  _∪_ : Set A → Set A → Set A
  X ∪ Y = proj₁ binary-unions

  ∈-∪ : ∀ {a} → (a ∈ X ⊎ a ∈ Y) ⇔ a ∈ X ∪ Y
  ∈-∪ = proj₂ binary-unions

  spec-∈ : Type ℓ → Type ℓ
  spec-∈ A = {X : Set A} → specProperty (_∈ X)

  -- membership needs to be a specProperty to have intersections
  module Intersection (sp-∈ : spec-∈ A) where

    infixr 7 _∩_
    _∩_ : Set A → Set A → Set A
    X ∩ Y = filter sp-∈ X

    ∈-∩ : ∀ {a} → (a ∈ X × a ∈ Y) ⇔ a ∈ X ∩ Y
    ∈-∩ {X} {Y} {a} = (a ∈ X × a ∈ Y) ↔⟨ ×-comm _ _ ⟩
                      (a ∈ Y × a ∈ X) ∼⟨ ∈-filter ⟩
                      a ∈ X ∩ Y       ∎
      where open R.EquationalReasoning

    disjoint' : Set A → Set A → Type ℓ
    disjoint' X Y = X ∩ Y ≡ᵉ ∅

    _＼_ : Set A → Set A → Set A
    X ＼ Y = filter (sp-¬ (sp-∈ {Y})) X

  All : (A → Type) → Set A → Type ℓ
  All P X = ∀ {a} → a ∈ X → P a

  Any : (A → Type) → Set A → Type ℓ
  Any P X = ∃[ a ] a ∈ X × P a

-- finite set theories
record Theoryᶠ : Type₁ where
  field theory : Theory
  open Theory theory public

  field finiteness : (X : Set A) → finite X

  DecEq⇒strongly-finite : ⦃ DecEq A ⦄ → (X : Set A) → strongly-finite X
  DecEq⇒strongly-finite X = DecEq∧finite⇒strongly-finite X (finiteness X)

  lengthˢ : ⦃ DecEq A ⦄ → Set A → ℕ
  lengthˢ X = card (X , DecEq⇒strongly-finite X)

  module _ {A : Type} ⦃ _ : Show A ⦄ where
    instance
      Show-Set : Show (Set A)
      Show-Set .show = λ x → Show-finite .show (x , (finiteness x))

-- set theories with an infinite set (containing all natural numbers)
record Theoryⁱ : Type₁ where
  field theory : Theory
  open Theory theory public

  field infinity : ∃[ Y ] ((n : ℕ) → n ∈ Y)

-- theories with decidable properties
record Theoryᵈ : Type₁ where
  field th : Theory
  open Theory th public
  open Equivalence

  field
    ∈-sp : ⦃ DecEq A ⦄ → spec-∈ A
    _∈?_ : ⦃ DecEq A ⦄ → Decidable² (_∈_ {A = A})
    all? : {P : A → Type} (P? : Decidable¹ P) {X : Set A} → Dec (All P X)
    any? : {P : A → Type} (P? : Decidable¹ P) (X : Set A) → Dec (Any P X)


  module _ {A : Type} {P : A → Type} where
    module _ ⦃ _ : P ⁇¹ ⦄ where instance
      Dec-Allˢ : All P ⁇¹
      Dec-Allˢ = ⁇¹ λ x → all? dec¹ {x}

      Dec-Anyˢ : Any P ⁇¹
      Dec-Anyˢ = ⁇¹ any? dec¹

    module _ (P? : Decidable¹ P) where
      allᵇ anyᵇ : (X : Set A) → Bool
      allᵇ X = ⌊ all? P? {X} ⌋
      anyᵇ X = ⌊ any? P? X   ⌋

  module _ {A : Type} ⦃ _ : DecEq A ⦄ where

    _∈ᵇ_ : A → Set A → Bool
    a ∈ᵇ X = ⌊ a ∈? X ⌋

    instance
      Dec-∈ : _∈_ {A = A} ⁇²
      Dec-∈ = ⁇² _∈?_

    _ = _∈_  {A = A} ⁇² ∋ it
    _ = _⊆_  {A = A} ⁇² ∋ it
    _ = _≡ᵉ_ {A = A} ⁇² ∋ it

    incl-set' : (X : Set A) → A → Maybe (∃[ a ] a ∈ X)
    incl-set' X x with x ∈? X
    ... | yes p = just (x , p)
    ... | no  p = nothing

    incl-set : (X : Set A) → Set (∃[ a ] a ∈ X)
    incl-set X = mapPartial (incl-set' X) X

    module _ {X : Set A} where
      incl-set-proj₁⊆ : map proj₁ (incl-set X) ⊆ X
      incl-set-proj₁⊆ x with from ∈-map x
      ... | (_ , pf) , refl , _ = pf

      incl-set-proj₁⊇ : X ⊆ map proj₁ (incl-set X)
      incl-set-proj₁⊇ {x} x∈X with x ∈? X in eq
      ... | no ¬p = contradiction x∈X ¬p
      ... | yes p = to ∈-map
        ( (x , p)
        , refl
        , to (∈-mapPartial {f = incl-set' X}) (x , x∈X , helper eq)
        )
        where helper : x ∈? X ≡ yes p → incl-set' X x ≡ just (x , p)
              helper h with x ∈? X | h
              ... | _ | refl = refl

      incl-set-proj₁ : map proj₁ (incl-set X) ≡ᵉ X
      incl-set-proj₁ = incl-set-proj₁⊆ , incl-set-proj₁⊇
