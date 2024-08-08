{-# OPTIONS --safe #-}

module Ledger.Types.Epoch where

open import Ledger.Prelude hiding (compare; Rel)

open import Agda.Builtin.FromNat
open import Algebra using (Semiring)
open import Relation.Binary
open import Data.Nat.Properties using (+-*-semiring)
open import Data.Rational using (ℚ)
open import Data.Rational.Ext using (PosUnitInterval)

record EpochStructure : Type₁ where
  field Slotʳ     : Semiring 0ℓ 0ℓ
        Epoch     : Type; ⦃ DecEq-Epoch ⦄ : DecEq Epoch
        KESPeriod : Type; ⦃ DecEq-KESPeriod ⦄ : DecEq KESPeriod; ⦃ HasPreorder-KESPeriod ⦄ : HasPreorder≡ {A = KESPeriod}

  Slot = Semiring.Carrier Slotʳ

  field ⦃ DecPo-Slot ⦄                : HasDecPartialOrder≡ {A = Slot}
        ⦃ DecEq-Slot ⦄                : DecEq Slot

        epoch                         : Slot → Epoch
        firstSlot                     : Epoch → Slot
        StabilityWindow               : Slot
        RandomnessStabilisationWindow : Slot
        sucᵉ                          : Epoch → Epoch
        kesPeriod                     : Slot → KESPeriod
        _+ᵏ_                          : KESPeriod → ℕ → KESPeriod
        _-ᵏ_                          : KESPeriod → KESPeriod → ℕ
        --⦃ -ᵏ-correct ⦄              : ∀ (kp kp′ : KESPeriod) → kp′ ≤ kp → kp′ +ᵏ (kp -ᵏ kp′) ≡ kp
        MaxKESEvo                     : ℕ
        ActiveSlotCoeff               : PosUnitInterval
        MaxMajorPV                    : ℕ

  -- preorders and partial orders

  instance
    preoEpoch : HasPreorder
    preoEpoch = hasPreorderFromStrictPartialOrder {_<_ = _<_ on firstSlot}
      record
        { isEquivalence = isEquivalence
        ; irrefl = λ where refl → <-irrefl {A = Slot} refl
        ; trans  = <-trans {A = Slot}
        ; <-resp-≈ = (λ where refl → id) , (λ where refl → id)
        }

  _ = _<_ {A = Slot}  ⁇² ∋ it
  _ = _≤_ {A = Slot}  ⁇² ∋ it
  _ = _<_ {A = Epoch} ⁇² ∋ it
  _ = _≤_ {A = Epoch} ⁇² ∋ it

  -- addition

  open Semiring Slotʳ renaming (_+_ to _+ˢ_)

  ℕtoEpoch : ℕ → Epoch
  ℕtoEpoch zero    = epoch 0#
  ℕtoEpoch (suc n) = sucᵉ (ℕtoEpoch n)

  _+ᵉ_ : ℕ → Epoch → Epoch
  zero  +ᵉ e = e
  suc n +ᵉ e = sucᵉ (n +ᵉ e)

  instance
    addSlot : HasAdd Slot
    addSlot ._+_ = _+ˢ_

    addEpoch : HasAdd Epoch
    addEpoch ._+_ e e' = epoch (firstSlot e + firstSlot e')

    Number-Epoch : Number Epoch
    Number-Epoch .Number.Constraint _ = ⊤
    Number-Epoch .Number.fromNat    x = ℕtoEpoch x

record GlobalConstants : Type₁ where
  field  Network : Type; ⦃ DecEq-Netw ⦄ : DecEq Network
         SlotsPerEpochᶜ : ℕ; ⦃ NonZero-SlotsPerEpochᶜ ⦄ : NonZero SlotsPerEpochᶜ
         StabilityWindowᶜ : ℕ
         RandomnessStabilisationWindowᶜ : ℕ
         Quorum : ℕ
         NetworkId : Network
         SlotsPerKESPeriodᶜ : ℕ; ⦃ NonZero-SlotsPerKESPeriodᶜ ⦄ : NonZero SlotsPerKESPeriodᶜ
         MaxKESEvoᶜ : ℕ
         ActiveSlotCoeffᶜ : PosUnitInterval
         MaxMajorPVᶜ : ℕ

  ℕEpochStructure : EpochStructure
  ℕEpochStructure = λ where
    .Slotʳ                         → +-*-semiring
    .Epoch                         → ℕ
    .KESPeriod                     → ℕ
    .epoch slot                    → slot / SlotsPerEpochᶜ
    .firstSlot e                   → e * SlotsPerEpochᶜ
    .StabilityWindow               → StabilityWindowᶜ
    .RandomnessStabilisationWindow → RandomnessStabilisationWindowᶜ
    .sucᵉ                          → suc
    .kesPeriod slot                → slot / SlotsPerKESPeriodᶜ
    ._+ᵏ_                          → _+_
    ._-ᵏ_                          → _-_
    .MaxKESEvo                     → MaxKESEvoᶜ
    .ActiveSlotCoeff               → ActiveSlotCoeffᶜ
    .MaxMajorPV                    → MaxMajorPVᶜ

   where open EpochStructure

open GlobalConstants using (ℕEpochStructure) public
