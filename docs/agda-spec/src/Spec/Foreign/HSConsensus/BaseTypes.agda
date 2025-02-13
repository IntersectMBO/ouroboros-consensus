module Spec.Foreign.HSConsensus.BaseTypes where

open import Data.Rational

open import Spec.Foreign.ExternalFunctions
open import Spec.Foreign.HSConsensus.Core public
import Spec.Foreign.HSTypes as F

instance
  iConvTop    = Convertible-Refl {⊤}
  iConvNat    = Convertible-Refl {ℕ}
  iConvRat    = Convertible-Refl {ℚ}
  iConvString = Convertible-Refl {String}
  iConvBool   = Convertible-Refl {Bool}

instance -- TODO: Complete

  -- * Unit and empty

  HsTy-⊥ = MkHsType ⊥ F.Empty
  Conv-⊥ = autoConvert ⊥

  HsTy-⊤ = MkHsType ⊤ ⊤

  -- * Maps and Sets

  HsTy-HSSet : ∀ {A} → ⦃ HasHsType A ⦄ → HasHsType (ℙ A)
  HsTy-HSSet {A} = MkHsType _ (F.HSSet (HsType A))

  Conv-HSSet : ∀ {A} ⦃ _ : HasHsType A ⦄
             → ⦃ HsConvertible A ⦄
             → HsConvertible (ℙ A)
  Conv-HSSet = λ where
    .to → F.MkHSSet ∘ to ∘ setToList
    .from → fromListˢ ∘ from ∘ F.HSSet.elems

  HsTy-Map : ∀ {A B} → ⦃ HasHsType A ⦄ → ⦃ HasHsType B ⦄ → HasHsType (A ⇀ B)
  HsTy-Map {A} {B} = MkHsType _ (F.HSMap (HsType A) (HsType B))

  Conv-HSMap : ∀ {A B} ⦃ _ : HasHsType A ⦄ ⦃ _ : HasHsType B ⦄
    → ⦃ DecEq A ⦄
    → ⦃ HsConvertible A ⦄
    → ⦃ HsConvertible B ⦄
    → HsConvertible (A ⇀ B)
  Conv-HSMap = λ where
    .to → F.MkHSMap ∘ to
    .from → from ∘ F.HSMap.assocList

  -- * ComputationResult

  HsTy-ComputationResult : ∀ {l} {Err} {A : Type l}
                           → ⦃ HasHsType Err ⦄ → ⦃ HasHsType A ⦄
                           → HasHsType (ComputationResult Err A)
  HsTy-ComputationResult {Err = Err} {A} = MkHsType _ (F.ComputationResult (HsType Err) (HsType A))

  Conv-ComputationResult : ConvertibleType ComputationResult F.ComputationResult
  Conv-ComputationResult = autoConvertible

open import Spec.Foreign.HSConsensus.ExternalStructures dummyExternalFunctions
  renaming
    ( HSEpochStructure to DummyEpochStructure
    ; HSCrypto to DummyCrypto
    ; HSNonces to DummyNonces
    ; HSBlockStructure to DummyBlockStructure
    ; HSAbstractFunctions to DummyAbstractFunctions
    ; HSRationalExtStructure to DummyRationalExtStructure
    ; HSLedgerInterface to DummyLedgerInterface
    )
  public

unquoteDecl = do
  hsTypeAlias Slot
  hsTypeAlias Epoch
  hsTypeAlias KeyHashˢ ⊣ withName "KeyHashS"
  hsTypeAlias KeyHashᵛ ⊣ withName "KeyHashV"
