{-# OPTIONS --safe #-}

module Interface.HasOrder where

open import Prelude
open import Relation.Binary

open Equivalence using (from; to)

module _ {a} {A : Set a} where
  module _ {_≈_ : Rel A a} where

    record HasPreorder : Set (sucˡ a) where
      infix 4 _≤_ _<_ _≥_ _>_
      field
        _≤_ _<_       : Rel A a
        ≤-isPreorder  : IsPreorder _≈_ _≤_
        <-irrefl      : Irreflexive _≈_ _<_
        ≤⇔<∨≈         : ∀ {x y} → x ≤ y ⇔ (x < y ⊎ x ≈ y)

      _≥_ = flip _≤_
      _>_ = flip _<_

      open IsPreorder ≤-isPreorder public
        using ()
        renaming (isEquivalence to ≈-isEquivalence; refl to ≤-refl; trans to ≤-trans)


      _≤?_ : ⦃ _≤_ ⁇² ⦄ → Decidable _≤_
      _≤?_ = dec²

      _<?_ : ⦃ _<_ ⁇² ⦄ → Decidable _<_
      _<?_ = dec²

      infix 4 _<?_ _≤?_

      <⇒≤∧≉ : ∀ {x y} → x < y → x ≤ y × ¬ (x ≈ y)
      <⇒≤∧≉ x<y = from ≤⇔<∨≈ (inj₁ x<y) , λ x≈y → <-irrefl x≈y x<y

      ≤∧≉⇒< : ∀ {x y} → x ≤ y × ¬ (x ≈ y) → x < y
      ≤∧≉⇒< {x} {y} (x≤y , ¬x≈y) with to ≤⇔<∨≈ x≤y
      ... | inj₁ x<y = x<y
      ... | inj₂ x≈y = ⊥-elim (¬x≈y x≈y)

      ≤-antisym⇒<-asym : Antisymmetric _≈_ _≤_ → Asymmetric _<_
      ≤-antisym⇒<-asym antisym x<y y<x =
        proj₂ (<⇒≤∧≉ x<y) $ antisym (proj₁ $ <⇒≤∧≉ x<y) (proj₁ $ <⇒≤∧≉ y<x)

    open HasPreorder ⦃...⦄

    record HasDecPreorder : Set (sucˡ a) where
      field ⦃ hasPreorder ⦄ : HasPreorder
            ⦃ dec-≤ ⦄ : _≤_ ⁇²
            ⦃ dec-< ⦄ : _<_ ⁇²

    record HasPartialOrder : Set (sucˡ a) where
      field
        ⦃ hasPreorder ⦄ : HasPreorder
        ≤-antisym : Antisymmetric _≈_ _≤_

      ≤-isPartialOrder : IsPartialOrder _≈_ _≤_
      ≤-isPartialOrder = record { isPreorder = ≤-isPreorder ; antisym = ≤-antisym }

      <-asymmetric : Asymmetric _<_
      <-asymmetric = ≤-antisym⇒<-asym ≤-antisym

      open IsEquivalence ≈-isEquivalence using () renaming (sym to ≈-sym) public

      <-trans : Transitive _<_
      <-trans i<j j<k =
        let
          j≤k = proj₁ $ <⇒≤∧≉ j<k; i≤j = proj₁ $ <⇒≤∧≉ i<j
          i≈k⇒j≈k = λ i≈k → ≤-antisym j≤k $ ≤-trans (from ≤⇔<∨≈ $ inj₂ (≈-sym i≈k)) i≤j
        in
          ≤∧≉⇒< (≤-trans i≤j j≤k , (proj₂ $ <⇒≤∧≉ j<k) ∘ i≈k⇒j≈k)

      <⇒¬>⊎≈ : ∀ {x y} → x < y → ¬ (y < x ⊎ y ≈ x)
      <⇒¬>⊎≈ x<y (inj₁ y<x) = <-asymmetric x<y y<x
      <⇒¬>⊎≈ x<y (inj₂ x≈y) = <-irrefl (≈-sym x≈y) x<y

      ≥⇒≮ : ∀ {x y} → y ≤ x → ¬ (x < y)
      ≥⇒≮ y≤x x<y = contradiction (to ≤⇔<∨≈ y≤x) (<⇒¬>⊎≈ x<y)

    open HasPartialOrder ⦃...⦄

    record HasDecPartialOrder : Set (sucˡ a) where
      field
        ⦃ hasPartialOrder ⦄ : HasPartialOrder
        ⦃ dec-≤ ⦄ : _≤_ ⁇²
        ⦃ dec-< ⦄ : _<_ ⁇²

    record HasTotalOrder : Set (sucˡ a) where
      field
        ⦃ hasPartialOrder ⦄ : HasPartialOrder
        ≤-total : Total _≤_

      ≤-isTotalOrder : IsTotalOrder _≈_ _≤_
      ≤-isTotalOrder = record { isPartialOrder = ≤-isPartialOrder ; total = ≤-total }

      ≮⇒≥ : Decidable _≈_ → ∀ {x y} → ¬ (x < y) → y ≤ x
      ≮⇒≥ _≈?_ {x} {y} x≮y with x ≈? y | ≤-total y x
      ... | yes x≈y  | _        = IsPreorder.reflexive ≤-isPreorder (≈-sym x≈y)
      ... | _        | inj₁ y≤x = y≤x
      ... | no  x≉y  | inj₂ x≤y = contradiction (≤∧≉⇒< (x≤y , x≉y)) x≮y

    open HasTotalOrder ⦃...⦄

    record HasDecTotalOrder : Set (sucˡ a) where
      field
        ⦃ hasTotalOrder ⦄ : HasTotalOrder
        ⦃ dec-≤ ⦄ : _≤_ ⁇²
        ⦃ dec-< ⦄ : _<_ ⁇²

  HasPreorder≡ = HasPreorder {_≈_ = _≡_}
  HasDecPreorder≡ = HasDecPreorder {_≈_ = _≡_}
  HasPartialOrder≡ = HasPartialOrder {_≈_ = _≡_}
  HasDecPartialOrder≡ = HasDecPartialOrder {_≈_ = _≡_}
  HasTotalOrder≡ = HasTotalOrder {_≈_ = _≡_}
  HasDecTotalOrder≡ = HasDecTotalOrder {_≈_ = _≡_}

open HasPreorder ⦃...⦄ public
open HasPartialOrder ⦃...⦄ public hiding (hasPreorder)
open HasDecPartialOrder ⦃...⦄ public hiding (hasPartialOrder)
open HasTotalOrder ⦃...⦄ public hiding (hasPartialOrder)
open HasDecTotalOrder ⦃...⦄ public hiding (hasTotalOrder)

module _ {a} {A : Set a} {_≈_ : Rel A a} where

  module _ {_≤_ : Rel A a} where
    import Relation.Binary.Construct.NonStrictToStrict _≈_ _≤_ as SNS

    module _ (≤-isPreorder : IsPreorder _≈_ _≤_)
             (_≈?_ : Decidable² _≈_) where

      hasPreorderFromNonStrict : HasPreorder
      hasPreorderFromNonStrict = record
        { _≤_           = _≤_
        ; _<_           = SNS._<_
        ; ≤-isPreorder  = ≤-isPreorder
        ; <-irrefl      = SNS.<-irrefl
        ; ≤⇔<∨≈         = λ {a b} → mk⇔
          (λ a≤b → case (a ≈? b) of λ where (yes p) → inj₂ p ; (no ¬p) → inj₁ (a≤b , ¬p))
          λ where (inj₁ a<b) → proj₁ a<b ; (inj₂ a≈b) → IsPreorder.reflexive ≤-isPreorder a≈b
        }

      hasPartialOrderFromNonStrict : Antisymmetric _≈_ _≤_ → HasPartialOrder
      hasPartialOrderFromNonStrict antsym  = record
        { hasPreorder = hasPreorderFromNonStrict
        ; ≤-antisym = antsym
        }

  module _ {_<_ : Rel A a} where

    import Relation.Binary.Construct.StrictToNonStrict _≈_ _<_ as SNS

    module _ (<-isStrictPartialOrder : IsStrictPartialOrder _≈_ _<_) where

        hasPreorderFromStrictPartialOrder : HasPreorder
        hasPreorderFromStrictPartialOrder = record
          { _≤_ = SNS._≤_
          ; _<_ = _<_
          ; ≤-isPreorder = SNS.isPreorder₂ <-isStrictPartialOrder
          ; <-irrefl = <-isStrictPartialOrder .IsStrictPartialOrder.irrefl
          ; ≤⇔<∨≈ = mk⇔ id id
          }

        instance _ = hasPreorderFromStrictPartialOrder

        hasPartialOrderFromStrictPartialOrder : HasPartialOrder
        hasPartialOrderFromStrictPartialOrder = record
          { hasPreorder = hasPreorderFromStrictPartialOrder
          ; ≤-antisym = SNS.isPartialOrder <-isStrictPartialOrder .IsPartialOrder.antisym
          }


    module _ (<-isStrictTotalOrder : IsStrictTotalOrder _≈_ _<_) where

      private spo = IsStrictTotalOrder.isStrictPartialOrder <-isStrictTotalOrder

      hasPreorderFromStrictTotalOrder : HasPreorder
      hasPreorderFromStrictTotalOrder = hasPreorderFromStrictPartialOrder spo

      hasPartialOrderFromStrictTotalOrder : HasPartialOrder
      hasPartialOrderFromStrictTotalOrder = hasPartialOrderFromStrictPartialOrder spo
